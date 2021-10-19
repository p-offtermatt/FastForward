#!/bin/bash

#benchmarks=( 'service-tech/ibm-soundness' 'service-tech/sap-reference' )
#benchmarks=( 'cav-benchmarks/mist' 'cav-benchmarks/wahl-kroening' 'cav-benchmarks/soter' 'cav-benchmarks/bug_tracking' 'cav-benchmarks/medical' )
benchmarks=( 'cav-benchmarks/mist' )

tool='mist'
executable='mist'

#1 hour
time_soft=$(expr 1 \* 3600)
time_hard=$(expr $time_soft + 60)
#2 gigabyte
mem_soft=$(expr 2 \* 1024 \* 1024)
mem_hard=$(expr $mem_soft + 1024)

properties=( 'safe' )

for benchmark in ${benchmarks[@]}; do
    benchmark_dir="$benchmark"
    for prop in ${properties[@]}; do
        >$benchmark_dir/$prop-positive-$tool.list
        >$benchmark_dir/$prop-negative-$tool.list
        >$benchmark_dir/$prop-timeout-$tool.list
        >$benchmark_dir/$prop-error-$tool.list
        for file in $(find $benchmark_dir -name "*.spec"); do
            timing="$(date +%s%N)"
            (
                set -o pipefail
                ulimit -S -t $time_soft
                ulimit -H -t $time_hard
                ulimit -S -v $mem_soft
                ulimit -H -v $mem_hard
                $executable $file.safe.target1 2>&1 | tee $file.out
            )
            result=$?
            timing=$(($(date +%s%N)-timing))
            ryes=$(grep "EEC concludes safe" $file.out)
            rno=$(grep "Reachable" $file.out)
            if [[ -n $ryes ]]; then
                list='positive'
            elif [[ -n $rno ]]; then
                list='negative'
            elif [[ result -gt 127 ]]; then
                list='timeout'
            else
                list='error'
            fi
            echo $timing $file >>$benchmark_dir/$prop-$list-$tool.list
        done
    done
done

