#!/bin/bash

#benchmarks=( 'cav-benchmarks/mist' 'cav-benchmarks/wahl-kroening' 'cav-benchmarks/soter' 'cav-benchmarks/bug_tracking' 'cav-benchmarks/medical' )
benchmarks=( 'cav-benchmarks/soter' )

properties=( 'safe' )

#1 minute
time_soft=$(expr 1 \* 60)
time_hard=$(expr $time_soft + 10)
#2 gigabyte
mem_soft=$(expr 2 \* 1024 \* 1024)
mem_hard=$(expr $mem_soft + 1024)

for benchmark in ${benchmarks[@]}; do
    benchmark_dir="$benchmark"
    for file in $(find $benchmark_dir -name "*.spec"); do
        for prop in ${properties[@]}; do
            echo "creating $prop spec for $file"
            #sed -e '/vars/,/target/!D' $file > $file.tmpspec
            #cat $file.$prop.target1 >> $file.tmpspec
            #rm $file.$prop.target1
            #mv $file.tmpspec $file.$prop.target1
            (
                ulimit -S -t $time_soft
                ulimit -H -t $time_hard
                ulimit -S -v $mem_soft
                ulimit -H -v $mem_hard
                spec2tts $file.$prop.target1
            )
            mv $file.$prop.target1.tts      $file.$prop.tts
            mv $file.$prop.target1.tts.prop $file.$prop.prop
        done
    done
done
