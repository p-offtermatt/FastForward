#!/bin/bash
for FILE in ../nets/workflows/synthetic/*/lola/*/*.lola;
    do
    echo ${FILE}
    python3 reduce_net.py ${FILE}
    done