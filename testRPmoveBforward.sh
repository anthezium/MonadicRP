#!/bin/bash

done=""
exe="RPmoveBforwardTest"
i=0
N=500000
while [[ -z "${done}" && "${i}" -lt "${N}" ]]
do
  echo "${exe}, i: ${i}"
  x=$(dist/build/${exe}/${exe} 2> /dev/null)
  if echo "${x}" | grep -q "ACD";
  then
    done="${x}"
  fi
  ((i+=1))
done

echo "${done}"
echo "i: ${i}"
