#!/usr/bin/bash

#benchmarks=( "IBM" "SAP" "Erlang" "Concurrent" "Literature" )
#extensions=( "lola" "tpn" "spec" "pnet" "pnet" )
#properties=( "--termination" "--termination" "--termination --no-given-properties" "")
#ref="-r2"
#min=""

benchmarks=( "Concurrent" )
extensions=( "pnet" )
properties=( "" )
ref="-r2"
min="-m1"


#2 hours
time_soft=$(expr 2 \* 3600)
time_hard=$(expr $time_soft + 60)
#8 gigabyte
mem_soft=$(expr 8 \* 1024 \* 1024)
mem_hard=$(expr $mem_soft + 1024)

for i in "${!benchmarks[@]}"; do 
    benchmark=${benchmarks[$i]}
    extension=${extensions[$i]}
    property=${properties[$i]}
    echo "Runnng benchmark $benchmark with ext $extension and property $property"
    bf="${benchmark}/benchmark${ref}${min}.out"
    echo "file user system elapsed memory" >$bf

    for file in $(find $benchmark -name "*.$extension"); do
        echo "Testing net $file"
        echo -n "$file " >>$bf
        (
            ulimit -S -t $time_soft
            ulimit -H -t $time_hard
            ulimit -S -v $mem_soft
            ulimit -H -v $mem_hard
            /usr/bin/time -f "%U %S %e %M" -a -o $bf slapnet -v $ref $min --$extension $property $file > ${file}${ref}${min}.out
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
            echo 'Inconclusive result'
            echo 'inconclusive' >>$bf
        else
            echo 'Error'
            echo 'error' >>$bf
        fi
    done
    echo
done
