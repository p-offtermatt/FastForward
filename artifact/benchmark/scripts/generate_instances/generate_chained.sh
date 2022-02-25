#!/bin/bash

REPEATS=20
rm -r ../../instances/chained/

for ((chain=1;chain<=401;chain=chain+20))
    do
    base_path=../../instances/chained/chained_${chain}/

    python3 chain_nets.py ../../instances/IBM-safety-check ../../instances/chained/chained_unreduced_${chain}/ $REPEATS $chain

    mkdir -p ../../instances/chained/chained_templates_${chain}/
    cp ../../instances/chained/chained_unreduced_${chain}/* ../../instances/chained/chained_templates_${chain}/
    mkdir -p "${base_path}woflan/"
    mkdir -p "${base_path}lola/"
    mkdir -p "${base_path}continuous/"

    for FILE in ../../instances/chained/chained_templates_${chain}/*.lola;
        do
        echo ${FILE}
        python3 reduce_net.py ${FILE}


        FILENAME=${FILE##*/}
        FILENAME_NO_EXTENSION=${FILENAME%.lola}

        dotnet ../../tools/fastforward/fastforward.dll translate-wf ${FILE} -f PNML -o "${base_path}woflan/${FILENAME_NO_EXTENSION}" -m Reachability
        dotnet ../../tools/fastforward/fastforward.dll translate-wf ${FILE} -f Lola -o "${base_path}lola/${FILENAME_NO_EXTENSION}" -m Soundness
        dotnet ../../tools/fastforward/fastforward.dll translate-wf ${FILE} -f Lola -o "${base_path}continuous/${FILENAME_NO_EXTENSION}" -m Soundness

        done
    done