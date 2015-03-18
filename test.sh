#!/bin/bash

done=""
while [ -z "${done}" ]
do
  x=$(dist/build/RPListMove/RPListMove)
  if echo "${x}" | grep -q "ABCE";
  then
    done="${x}"
  fi
done

echo "${done}"
