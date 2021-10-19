#!/bin/bash

#benchmarks=( 'service-tech/ibm-soundness' 'service-tech/sap-reference' )
#benchmarks=( 'cav-benchmarks/mist' 'cav-benchmarks/wahl-kroening' 'cav-benchmarks/soter' 'cav-benchmarks/bug_tracking' 'cav-benchmarks/medical' )
benchmarks=( 'cav-benchmarks/soter' )

executable='bfc'

#3 hours
time_soft=$(expr 3 \* 3600)
time_hard=$(expr $time_soft + 60)
#2 gigabyte
mem_soft=$(expr 2 \* 1024 \* 1024)
mem_hard=$(expr $mem_soft + 1024)

properties=( 'safe' )

for benchmark in ${benchmarks[@]}; do
    benchmark_dir="$benchmark"
    for prop in ${properties[@]}; do
        >$benchmark_dir/$prop-positive-bfc.list
        >$benchmark_dir/$prop-negative-bfc.list
        >$benchmark_dir/$prop-timeout-bfc.list
        >$benchmark_dir/$prop-error-bfc.list
        for file in $(find $benchmark_dir -name "*.spec"); do
            timing="$(date +%s%N)"
            (
                set -o pipefail
                ulimit -S -t $time_soft
                ulimit -H -t $time_hard
                ulimit -S -v $mem_soft
                ulimit -H -v $mem_hard
                $executable --input-file $file.safe.tts --target $file.safe.prop 2>&1 | tee $file.out
            )
            result=$?
            timing=$(($(date +%s%N)-timing))
            ryes=$(grep "VERIFICATION SUCCESSFUL" $file.out)
            rno=$(grep "VERIFICATION FAILED" $file.out)
            if [[ -n $ryes ]]; then
                list='positive'
            elif [[ -n $rno ]]; then
                list='negative'
            elif [[ result -gt 127 ]]; then
                list='timeout'
            else
                list='error'
            fi
            echo $timing $file >>$benchmark_dir/$prop-$list-bfc.list
        done
    done
done

