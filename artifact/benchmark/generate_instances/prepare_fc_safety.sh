#!/bin/bash
base_path="../nets/workflows/fc-safety/"
mkdir -p $base_path

for FILE in ../nets/workflows/workflows_transformed/IBM/*.lola;
    do
    echo "${FILE}"
    FILENAME=${FILE#../nets/workflows/workflows_transformed/IBM/}
    FILENAME_NO_EXTENSION=${FILENAME%.lola}

    TARGET="${base_path}/"${FILENAME}
    TARGET_NO_EXTENSION=${TARGET%.lola}

    cp $FILE $TARGET
    python3 reduce_net.py ${TARGET}

    dotnet ../fastforward/fastforward.dll translate-wf ${TARGET} -f Lola -o "${base_path}/${FILENAME_NO_EXTENSION}" -m Safety

    done