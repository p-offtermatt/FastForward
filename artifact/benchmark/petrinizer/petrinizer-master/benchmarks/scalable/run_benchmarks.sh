#!/usr/bin/bash

benchmarks=( "Lamport" "Dekker" "Szymanski" "Snapshot" "LeaderElectionDKR82" "Peterson" )
suffix=""

#2 hours
time_soft=$(expr 2 \* 3600)
time_hard=$(expr $time_soft + 60)
#6 gigabyte
mem_soft=$(expr 6 \* 1024 \* 1024)
mem_hard=$(expr $mem_soft + 1024)

for benchmark in ${benchmarks[@]}; do
    echo "Runnng benchmark $benchmark"
    bf="${benchmark}/benchmark${suffix}.out"
    echo "n user system elapsed memory" >$bf

    if [[ $benchmark == "Dekker" || $benchmark == "Szymanski" ]]; then
        nmax=2
    else
        nmax=5
    fi

    for n in $(seq 2 $nmax); do
        file="${benchmark}/n${n}${suffix}.pnet"
        echo "Testing net $file"
        echo -n "$n " >>$bf
        output=$file-$n.out
        (
            ulimit -S -t $time_soft
            ulimit -H -t $time_hard
            ulimit -S -v $mem_soft
            ulimit -H -v $mem_hard
            /usr/bin/time -f "%U %S %e %M" -a -o $bf slapnet -vi --auto $file > ${file}${suffix}.out
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
    echo
done
