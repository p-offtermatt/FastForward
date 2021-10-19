#!/bin/bash

benchmarks=( 'service-tech/ibm-soundness' 'service-tech/sap-reference' )
#benchmarks=( 'service-tech/sap-reference' )
#benchmarks=( 'cav-benchmarks/mist' 'cav-benchmarks/wahl-kroening' 'cav-benchmarks/soter' 'cav-benchmarks/bug_tracking' 'cav-benchmarks/medical' )

extensions=( 'pnet' 'lola' 'tpn' 'spec' )
executable='../slapnet'

#3 hours
time_soft=$(expr 3 \* 3600)

properties=( 'terminating' )
prop_options=( '--no-given-properties --terminating' )

for benchmark in ${benchmarks[@]}; do
    benchmark_dir="$benchmark"
    for (( propi=0;propi<${#properties[@]};propi++)); do
        prop=${properties[$propi]}
        >$benchmark_dir/$prop-positive-slapnet.list
        >$benchmark_dir/$prop-dontknow-slapnet.list
        >$benchmark_dir/$prop-timeout-slapnet.list
        >$benchmark_dir/$prop-error-slapnet.list
        for ext in ${extensions[@]}; do
            for file in $(find $benchmark_dir -name "*.$ext"); do
                # TODO: use /usr/bin/time to measure resources
                # TODO: use ulimit to limit time/memory
                timing="$(date +%s%N)"
                (
                    set -o pipefail

                    echo timeout $time_soft $executable $prop_options --$ext $file -o $file.$prop
                    timeout $time_soft $executable $prop_options --$ext $file 2>&1 | tee $file.$prop.out
                )
                result=$?
                timing=$(($(date +%s%N)-timing))
                if [[ result -eq 0 ]]; then
                    list='positive'
                elif [[ result -eq 2 ]]; then
                    list='dontknow'
                elif [[ result -eq 124 || result -eq 137 ]]; then
                    list='timeout'
                else
                    list='error'
                fi
                echo $timing $file >>$benchmark_dir/$prop-$list-slapnet.list
            done
        done
    done
done
