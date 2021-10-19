#!/usr/bin/bash

n=$1
#orders=(ord rand rev)
orders=(ord)

for o in ${orders[@]}; do
    for i in $(seq 1 $n); do
        echo "Creating net for n = $i with order $o"
        ./make_net.py $i $o > "n$i.pnet"
    done
done
