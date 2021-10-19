#!/usr/bin/bash

n=$1

for i in $(seq 1 $n); do
    echo "Creating net for n = $i"
    ./make_net.py $i "ord" > "n$i.pnet"
done
