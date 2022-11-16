#!/bin/bash
for FILE in ../nets/workflows/workflows_reduced/*/*.lola;
    do
    echo ${FILE}
    python3 reduce_net.py ${FILE}
    done