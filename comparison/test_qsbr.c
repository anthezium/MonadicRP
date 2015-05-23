/*
 * test_urcu.c
 *
 * Userspace RCU library - test program
 *
 * Copyright February 2009 - Mathieu Desnoyers <mathieu.desnoyers@efficios.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#define _GNU_SOURCE
#include "../config.h"
#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>
#include <assert.h>
#include <sys/syscall.h>
#include <sched.h>
#include <errno.h>

#include <urcu/arch.h>

/* hardcoded number of CPUs */
#define NR_CPUS 16384

#if defined(_syscall0)
_syscall0(pid_t, gettid)
#elif defined(__NR_gettid)
static inline pid_t gettid(void)
{
	return syscall(__NR_gettid);
}
#else
#warning "use pid as tid"
static inline pid_t gettid(void)
{
	return getpid();
}
#endif

#ifndef DYNAMIC_LINK_TEST
#define _LGPL_SOURCE
#else
#define debug_yield_read()
#endif
#include "urcu-qsbr.h"

struct test_array {
	int a;
};

static volatile int test_go, test_stop;

static unsigned long wdelay;

static struct test_array *test_rcu_pointer;

static unsigned long duration;

/* read-side C.S. duration, in loops */
static unsigned long rduration;

/* write-side C.S. duration, in loops */
static unsigned long wduration;

static inline void loop_sleep(unsigned long l)
{
	while(l-- != 0)
		caa_cpu_relax();
}

static int verbose_mode;

#define printf_verbose(fmt, args...)		\
	do {					\
		if (verbose_mode)		\
			printf(fmt, args);	\
	} while (0)

static unsigned int cpu_affinities[NR_CPUS];
static unsigned int next_aff = 0;
static int use_affinity = 0;

pthread_mutex_t affinity_mutex = PTHREAD_MUTEX_INITIALIZER;

#ifndef HAVE_CPU_SET_T
typedef unsigned long cpu_set_t;
# define CPU_ZERO(cpuset) do { *(cpuset) = 0; } while(0)
# define CPU_SET(cpu, cpuset) do { *(cpuset) |= (1UL << (cpu)); } while(0)
#endif

static void set_affinity(void)
{
	cpu_set_t mask;
	int cpu;
	int ret;

	if (!use_affinity)
		return;

#if HAVE_SCHED_SETAFFINITY
	ret = pthread_mutex_lock(&affinity_mutex);
	if (ret) {
		perror("Error in pthread mutex lock");
		exit(-1);
	}
	cpu = cpu_affinities[next_aff++];
	ret = pthread_mutex_unlock(&affinity_mutex);
	if (ret) {
		perror("Error in pthread mutex unlock");
		exit(-1);
	}
	CPU_ZERO(&mask);
	CPU_SET(cpu, &mask);
#if SCHED_SETAFFINITY_ARGS == 2
	sched_setaffinity(0, &mask);
#else
	sched_setaffinity(0, sizeof(mask), &mask);
#endif
#endif /* HAVE_SCHED_SETAFFINITY */
}

/*
 * returns 0 if test should end.
 */
static int test_duration_write(void)
{
	return !test_stop;
}

static int test_duration_read(void)
{
	return !test_stop;
}

static unsigned long long __thread nr_writes;
static unsigned long long __thread nr_reads;

static unsigned int nr_readers;
static unsigned int nr_writers;

pthread_mutex_t rcu_copy_mutex = PTHREAD_MUTEX_INITIALIZER;

void rcu_copy_mutex_lock(void)
{
	int ret;
	ret = pthread_mutex_lock(&rcu_copy_mutex);
	if (ret) {
		perror("Error in pthread mutex lock");
		exit(-1);
	}
}

void rcu_copy_mutex_unlock(void)
{
	int ret;

	ret = pthread_mutex_unlock(&rcu_copy_mutex);
	if (ret) {
		perror("Error in pthread mutex unlock");
		exit(-1);
	}
}

/*
 * malloc/free are reusing memory areas too quickly, which does not let us
 * test races appropriately. Use a large circular array for allocations.
 * ARRAY_SIZE is larger than nr_writers, which insures we never run over our tail.
 */
#define ARRAY_SIZE (1048576 * nr_writers)
#define ARRAY_POISON 0xDEADBEEF
static int array_index;
static struct test_array *test_array;

static struct test_array *test_array_alloc(void)
{
	struct test_array *ret;
	int index;

	rcu_copy_mutex_lock();
	index = array_index % ARRAY_SIZE;
	assert(test_array[index].a == ARRAY_POISON ||
		test_array[index].a == 0);
	ret = &test_array[index];
	array_index++;
	if (array_index == ARRAY_SIZE)
		array_index = 0;
	rcu_copy_mutex_unlock();
	return ret;
}

static void test_array_free(struct test_array *ptr)
{
	if (!ptr)
		return;
	rcu_copy_mutex_lock();
	ptr->a = ARRAY_POISON;
	rcu_copy_mutex_unlock();
}

void *thr_reader(void *_count)
{
	unsigned long long *count = _count;
	struct test_array *local_ptr;

	printf_verbose("thread_begin %s, thread id : %lx, tid %lu\n",
			"reader", pthread_self(), (unsigned long)gettid());
  
  // look, we can read an rcu-protected reference outside a read-side
  // critical section.
  local_ptr = rcu_dereference(test_rcu_pointer);

	set_affinity();

	rcu_register_thread();

	while (!test_go)
	{
	}
	cmm_smp_mb();

	for (;;) {
		rcu_read_lock();
		local_ptr = rcu_dereference(test_rcu_pointer);
		debug_yield_read();
    // look, we can synchronize inside a read-side critical section.
    synchronize_rcu();
    // look, we can write inside a read-side critical section.
    rcu_assign_pointer(test_rcu_pointer, NULL);
		if (local_ptr)
			assert(local_ptr->a == 8);
		if (unlikely(rduration))
			loop_sleep(rduration);
		rcu_read_unlock();
		nr_reads++;
		/* QS each 1024 reads */
		if (unlikely((nr_reads & ((1 << 10) - 1)) == 0))
			rcu_quiescent_state();
		if (unlikely(!test_duration_read()))
			break;
	}

	rcu_unregister_thread();

	/* test extra thread registration */
	rcu_register_thread();
	rcu_unregister_thread();

	*count = nr_reads;
	printf_verbose("thread_end %s, thread id : %lx, tid %lu\n",
			"reader", pthread_self(), (unsigned long)gettid());
	return ((void*)1);

}

void *thr_writer(void *_count)
{
	unsigned long long *count = _count;
	struct test_array *new, *old;

	printf_verbose("thread_begin %s, thread id : %lx, tid %lu\n",
			"writer", pthread_self(), (unsigned long)gettid());

	set_affinity();

	while (!test_go)
	{
	}
	cmm_smp_mb();

	for (;;) {
		new = test_array_alloc();
		new->a = 8;
		old = rcu_xchg_pointer(&test_rcu_pointer, new);
		if (unlikely(wduration))
			loop_sleep(wduration);
		synchronize_rcu();
		/* can be done after unlock */
		if (old)
			old->a = 0;
		test_array_free(old);
		nr_writes++;
		if (unlikely(!test_duration_write()))
			break;
		if (unlikely(wdelay))
			loop_sleep(wdelay);
	}

	printf_verbose("thread_end %s, thread id : %lx, tid %lu\n",
			"writer", pthread_self(), (unsigned long)gettid());
	*count = nr_writes;
	return ((void*)2);
}

void show_usage(int argc, char **argv)
{
	printf("Usage : %s nr_readers nr_writers duration (s)", argv[0]);
#ifdef DEBUG_YIELD
	printf(" [-r] [-w] (yield reader and/or writer)");
#endif
	printf(" [-d delay] (writer period (us))");
	printf(" [-c duration] (reader C.S. duration (in loops))");
	printf(" [-e duration] (writer C.S. duration (in loops))");
	printf(" [-v] (verbose output)");
	printf(" [-a cpu#] [-a cpu#]... (affinity)");
	printf("\n");
}

int main(int argc, char **argv)
{
	int err;
	pthread_t *tid_reader, *tid_writer;
	void *tret;
	unsigned long long *count_reader, *count_writer;
	unsigned long long tot_reads = 0, tot_writes = 0;
	int i, a;

	if (argc < 4) {
		show_usage(argc, argv);
		return -1;
	}

	err = sscanf(argv[1], "%u", &nr_readers);
	if (err != 1) {
		show_usage(argc, argv);
		return -1;
	}

	err = sscanf(argv[2], "%u", &nr_writers);
	if (err != 1) {
		show_usage(argc, argv);
		return -1;
	}
	
	err = sscanf(argv[3], "%lu", &duration);
	if (err != 1) {
		show_usage(argc, argv);
		return -1;
	}

	for (i = 4; i < argc; i++) {
		if (argv[i][0] != '-')
			continue;
		switch (argv[i][1]) {
#ifdef DEBUG_YIELD
		case 'r':
			yield_active |= YIELD_READ;
			break;
		case 'w':
			yield_active |= YIELD_WRITE;
			break;
#endif
		case 'a':
			if (argc < i + 2) {
				show_usage(argc, argv);
				return -1;
			}
			a = atoi(argv[++i]);
			cpu_affinities[next_aff++] = a;
			use_affinity = 1;
			printf_verbose("Adding CPU %d affinity\n", a);
			break;
		case 'c':
			if (argc < i + 2) {
				show_usage(argc, argv);
				return -1;
			}
			rduration = atol(argv[++i]);
			break;
		case 'd':
			if (argc < i + 2) {
				show_usage(argc, argv);
				return -1;
			}
			wdelay = atol(argv[++i]);
			break;
		case 'e':
			if (argc < i + 2) {
				show_usage(argc, argv);
				return -1;
			}
			wduration = atol(argv[++i]);
			break;
		case 'v':
			verbose_mode = 1;
			break;
		}
	}

	printf_verbose("running test for %lu seconds, %u readers, %u writers.\n",
		duration, nr_readers, nr_writers);
	printf_verbose("Writer delay : %lu loops.\n", wdelay);
	printf_verbose("Reader duration : %lu loops.\n", rduration);
	printf_verbose("thread %-6s, thread id : %lx, tid %lu\n",
			"main", pthread_self(), (unsigned long)gettid());

	test_array = calloc(1, sizeof(*test_array) * ARRAY_SIZE);
	tid_reader = malloc(sizeof(*tid_reader) * nr_readers);
	tid_writer = malloc(sizeof(*tid_writer) * nr_writers);
	count_reader = malloc(sizeof(*count_reader) * nr_readers);
	count_writer = malloc(sizeof(*count_writer) * nr_writers);

	next_aff = 0;

	for (i = 0; i < nr_readers; i++) {
		err = pthread_create(&tid_reader[i], NULL, thr_reader,
				     &count_reader[i]);
		if (err != 0)
			exit(1);
	}
	for (i = 0; i < nr_writers; i++) {
		err = pthread_create(&tid_writer[i], NULL, thr_writer,
				     &count_writer[i]);
		if (err != 0)
			exit(1);
	}

	cmm_smp_mb();

	test_go = 1;

	sleep(duration);

	test_stop = 1;

	for (i = 0; i < nr_readers; i++) {
		err = pthread_join(tid_reader[i], &tret);
		if (err != 0)
			exit(1);
		tot_reads += count_reader[i];
	}
	for (i = 0; i < nr_writers; i++) {
		err = pthread_join(tid_writer[i], &tret);
		if (err != 0)
			exit(1);
		tot_writes += count_writer[i];
	}
	
	printf_verbose("total number of reads : %llu, writes %llu\n", tot_reads,
	       tot_writes);
	printf("SUMMARY %-25s testdur %4lu nr_readers %3u rdur %6lu wdur %6lu "
		"nr_writers %3u "
		"wdelay %6lu nr_reads %12llu nr_writes %12llu nr_ops %12llu\n",
		argv[0], duration, nr_readers, rduration, wduration,
		nr_writers, wdelay, tot_reads, tot_writes,
		tot_reads + tot_writes);
	test_array_free(test_rcu_pointer);
	free(test_array);
	free(tid_reader);
	free(tid_writer);
	free(count_reader);
	free(count_writer);
	return 0;
}
