#!/bin/bash

benchmarks=( 'Snapshot' 'Peterson' 'Lamport' 'Szymanski' 'LeaderElectionCR79' 'LeaderElectionDKR82' )
#benchmarks=( 'Lamport' )
nmax=5

#2 hours
time_soft=$(expr 2 \* 3600)
time_hard=$(expr $time_soft + 60)
#2 gigabyte
mem_soft=$(expr 2 \* 1024 \* 1024)
mem_hard=$(expr $mem_soft + 1024)

for benchmark in ${benchmarks[@]}; do
    file=$benchmark.promela
    bf=$benchmark.out
    echo "n user system memory" >$bf
    for n in $(seq 2 $nmax); do
        echo "Testing $file with n=$n"
        echo -n "$n " >>$bf
        output=$file-$n.out
        (
            ulimit -S -t $time_soft
            ulimit -H -t $time_hard
            ulimit -S -v $mem_soft
            ulimit -H -v $mem_hard
            /usr/bin/time -f "%U %S %M" -a -o $bf ./run-spin $file $output $n
        )
        result=$?
        if [[ result -eq 0 ]]; then
            echo 'Positive result'
        elif [[ result -eq 1 ]]; then
            echo 'Negative result'
        else
            echo 'Error'
        fi
    done
done
