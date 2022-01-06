#!/bin/bash
base_path=../nets/workflows/chained_workflows/

rm -r ../nets/workflows/chained_workflows*
python3 chain_nets.py ../nets/workflows/fc-safety ../nets/workflows/chained_workflows_unreduced/

mkdir -p ../nets/workflows/chained_workflows_templates/
cp ../nets/workflows/chained_workflows_unreduced/* ../nets/workflows/chained_workflows_templates/
mkdir -p "${base_path}woflan/"
mkdir -p "${base_path}lola/"
mkdir -p "${base_path}continuous/"

for FILE in ../nets/workflows/chained_workflows_templates/*.lola;
    do
    echo ${FILE}
    python3 reduce_net.py ${FILE}


    FILENAME=${FILE##*/}
    FILENAME_NO_EXTENSION=${FILENAME%.lola}

    dotnet ../fastforward/fastforward.dll translate-wf ${FILE} -f PNML -o "${base_path}woflan/${FILENAME_NO_EXTENSION}" -m Reachability
    dotnet ../fastforward/fastforward.dll translate-wf ${FILE} -f Lola -o "${base_path}lola/${FILENAME_NO_EXTENSION}" -m Soundness
    dotnet ../fastforward/fastforward.dll translate-wf ${FILE} -f Lola -o "${base_path}continuous/${FILENAME_NO_EXTENSION}" -m Soundness



    done