#!/bin/bash
base_path="../nets/workflows/generalized-soundness/"
mkdir -p "${base_path}woflan"
mkdir -p "${base_path}continuous"
mkdir -p "${base_path}lola"

for FILE in ../nets/workflows/workflows_transformed/IBM/*.lola;
    do
    echo "${FILE}"
    FILENAME=${FILE#../nets/workflows/workflows_transformed/SAP/}
    FILENAME_NO_EXTENSION=${FILENAME%.lola}

    TARGET="${base_path}continuous/"${FILENAME}
    TARGET_NO_EXTENSION=${TARGET%.lola}

    # if this does not give exit code 0, fastforward was not able to parse the net, typically it is not a workflow net
    if dotnet ../fastforward/fastforward.dll translate-wf ${FILE} -f Lola -o ${TARGET_NO_EXTENSION} -m Soundness; then
        python3 reduce_net.py ${TARGET}
        dotnet ../fastforward/fastforward.dll translate-wf ${TARGET} -f PNML -o "${base_path}woflan/${FILENAME_NO_EXTENSION}" -m Reachability
        dotnet ../fastforward/fastforward.dll translate-wf ${TARGET} -f Lola -o "${base_path}lola/${FILENAME_NO_EXTENSION}" -m Soundness
    fi
    done