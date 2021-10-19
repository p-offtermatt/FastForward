#!/bin/bash

#benchmarks=( 'Snapshot' 'Peterson' 'Lamport' 'Szymanski' 'LeaderElectionCR79' 'LeaderElectionDKR82' )
#benchmarks=( 'Peterson' 'Lamport' 'Szymanski' 'Snapshot' 'LeaderElectionDKR82' )
benchmarks=( 'Snapshot' )
nmax=6

#2 hours
time_soft=$(expr 2 \* 3600)
time_hard=$(expr $time_soft + 60)
#15 gigabyte
mem_soft=$(expr 15 \* 1024 \* 1024)
mem_hard=$(expr $mem_soft + 1024)

bf='benchmarks.out'
echo "n user system memory" >$bf
for benchmark in ${benchmarks[@]}; do
    file=$benchmark.promela
    for n in $(seq 2 $nmax); do
        echo "Testing $file with n=$n"
        echo -n "$file-$n " >>$bf
        output=$file-$n.out
        (
            ulimit -S -t $time_soft
            ulimit -H -t $time_hard
            ulimit -S -v $mem_soft
            ulimit -H -v $mem_hard
            /usr/bin/time -f "%U %S %M" -a -o $bf ./run-spin $file $output $n
        )
        result=$?
        echo -n "$result " >>$bf
        if [[ result -eq 0 ]]; then
            echo 'Positive result'
            echo 'positive' >>$bf
        elif [[ result -eq 1 ]]; then
            echo 'Negative result'
            echo 'negative' >>$bf
        elif [[ result -eq 2 ]]; then
            echo 'Out of memory'
            echo 'memoryout' >>$bf
        else
            echo 'Error'
            echo 'error' >>$bf
        fi
    done
done
