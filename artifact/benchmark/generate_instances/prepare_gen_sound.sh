#!/bin/bash
base_path="../nets/workflows/free-choice/"
mkdir -p "${base_path}woflan"
mkdir -p "${base_path}continuous"
mkdir -p "${base_path}lola"

for FILE in ../nets/workflows/workflows_transformed/SAP/*.lola;
    do
    echo "${FILE}"
    FILENAME=${FILE#../nets/workflows/workflows_transformed/SAP/}
    FILENAME_NO_EXTENSION=${FILENAME%.lola}

    TARGET="${base_path}continuous/"${FILENAME}
    cp ${FILE} ${TARGET}
    python3 reduce_net.py ${TARGET}
    dotnet ../fastforward/fastforward.dll translate-wf ${TARGET} -f PNML -o "${base_path}woflan/${FILENAME_NO_EXTENSION}" -m Reachability
    dotnet ../fastforward/fastforward.dll translate-wf ${TARGET} -f Lola -o "${base_path}lola/${FILENAME_NO_EXTENSION}" -m Soundness
    done