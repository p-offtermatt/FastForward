#!/bin/bash
for FILE in $(find ../../instances/synthetic/generalized-soundness-templates -name '*.lola');
    do
    echo "${FILE}"

    FILENAME=${FILE#../../instances/synthetic/generalized-soundness-templates/}
    FILENAME=${FILENAME%.lola}
    TARGET_ROOT="../../instances/synthetic/generalized-soundness/"

    FILENAME_NO_FOLDER=${FILENAME##*/}
    FOLDER_NO_FILENAME=${FILENAME%%/*}

    # filenames are of the form c-foo_1-check; this extracts c
    NUM_C=${FILENAME_NO_FOLDER%%-*}

    FILENAME_WITHOUT_CHECK=${FILENAME%1-check*}

    mkdir -p "${TARGET_ROOT}/continuous/${FOLDER_NO_FILENAME}"
    mkdir -p "${TARGET_ROOT}/lola/${FOLDER_NO_FILENAME}"
    mkdir -p "${TARGET_ROOT}/woflan/${FOLDER_NO_FILENAME}"

    REPLACE_TARGET="${TARGET_ROOT}/lola/${FILENAME}"
    REPLACE_OUTPUT="${REPLACE_TARGET}.lola"

    echo $REPLACE_TARGET
    echo $REPLACE_OUTPUT


    dotnet ../../tools/fastforward/fastforward.dll replace-weights ${FILE} -f Lola -o ${REPLACE_TARGET}

    python3 reduce_net.py ${REPLACE_OUTPUT}

    for ((i=1;i<=NUM_C;i++));
    do
        dotnet ../../tools/fastforward/fastforward.dll translate-wf ${REPLACE_OUTPUT} -m Soundness -k $i -f Lola -o "${TARGET_ROOT}/continuous/${FILENAME_WITHOUT_CHECK}${i}-check"
        dotnet ../../tools/fastforward/fastforward.dll translate-wf ${REPLACE_OUTPUT} -m Soundness -k $i -f Lola -o "${TARGET_ROOT}/lola/${FILENAME_WITHOUT_CHECK}${i}-check"
        dotnet ../../tools/fastforward/fastforward.dll translate-wf ${REPLACE_OUTPUT} -m Soundness -k $i -f TPN -o "${TARGET_ROOT}/woflan/${FILENAME_WITHOUT_CHECK}${i}-check"
    done


    done