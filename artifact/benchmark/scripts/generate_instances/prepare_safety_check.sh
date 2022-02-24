#!/bin/bash
base_path="../../instances/IBM-safety-check/"
mkdir -p $base_path

for FILE in ../../instances/IBM/*.lola;
    do
    echo "${FILE}"
    FILENAME=${FILE#../../instances/IBM/}
    FILENAME_NO_EXTENSION=${FILENAME%.lola}

    dotnet ../../tools/fastforward/fastforward.dll translate-wf ${FILE} -f Lola -o "${base_path}/${FILENAME_NO_EXTENSION}" -m Safety

    done