#!/bin/bash

mkdir benchmarks/g

for I in `seq 0 10`
do

    cat > "benchmarks/g/g$I.spec" <<EOF
vars
    v1 v2

rules
    v1 >= 1 ->
		    v1' = v1-1,
		    v2' = v2+1;


init
    v1 = $I, v2 = 0

target
    v2 >= $I
EOF
done