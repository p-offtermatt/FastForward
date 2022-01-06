#!/bin/bash

REPEATS=10
rm -r ../nets/workflows/chained_workflows*

for ((chain=1;chain<=201;chain=chain+20))
    do
    base_path=../nets/workflows/chained_workflows_${chain}/

    python3 chain_nets.py ../nets/workflows/fc-safety ../nets/workflows/chained_workflows_unreduced_${chain}/ $REPEATS $chain

    mkdir -p ../nets/workflows/chained_workflows_templates_${chain}/
    cp ../nets/workflows/chained_workflows_unreduced_${chain}/* ../nets/workflows/chained_workflows_templates_${chain}/
    mkdir -p "${base_path}woflan/"
    mkdir -p "${base_path}lola/"
    mkdir -p "${base_path}continuous/"

    for FILE in ../nets/workflows/chained_workflows_templates_${chain}/*.lola;
        do
        echo ${FILE}
        sleep 2c
        python3 reduce_net.py ${FILE}


        FILENAME=${FILE##*/}
        FILENAME_NO_EXTENSION=${FILENAME%.lola}

        dotnet ../fastforward/fastforward.dll translate-wf ${FILE} -f PNML -o "${base_path}woflan/${FILENAME_NO_EXTENSION}" -m Reachability
        dotnet ../fastforward/fastforward.dll translate-wf ${FILE} -f Lola -o "${base_path}lola/${FILENAME_NO_EXTENSION}" -m Soundness
        dotnet ../fastforward/fastforward.dll translate-wf ${FILE} -f Lola -o "${base_path}continuous/${FILENAME_NO_EXTENSION}" -m Soundness

        done
    done