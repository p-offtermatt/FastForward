#!/bin/bash
for FILE in ../nets/workflows/synthetic/structural-soundness-templates/*/*-check.lola;
    do
    echo "${FILE}"
    FILENAME=${FILE#../nets/workflows/synthetic/structural-soundness-templates/}
    FILENAME=${FILENAME%.lola}
    TARGET_ROOT="../nets/workflows/synthetic/structural-soundness"
    mkdir -p "${TARGET_ROOT}/continuous/${FILENAME%/*}"
    mkdir -p "${TARGET_ROOT}/lola/${FILENAME%/*}"
    mkdir -p "${TARGET_ROOT}/woflan/${FILENAME%/*}"

    FILENAME_NO_FOLDER=${FILENAME##*/}
    FOLDER_NO_FILENAME=${FILENAME%%/*}
    NUM_C=${FILENAME_NO_FOLDER%%-*}

    FILENAME_NO_K=${FILENAME_NO_FOLDER%%_*}
    echo $FILENAME_NO_K

    LOLA_TARGET="${TARGET_ROOT}/lola/${FOLDER_NO_FILENAME}/${FILENAME_NO_K}_1-check"
    LOLA_OUTPUT=${LOLA_TARGET}.lola

    dotnet ../fastforward/fastforward.dll replace-weights ${FILE} -f Lola -o ${LOLA_TARGET}

    python3 reduce_net.py ${LOLA_OUTPUT}

    for ((i=1;i<=NUM_C;i++));
        do
        echo dotnet ../fastforward/fastforward.dll translate-wf ${LOLA_OUTPUT} -m Soundness -f Lola -o "${TARGET_ROOT}/continuous/${FOLDER_NO_FILENAME}/${FILENAME_NO_K}_${i}-check" -k $i
        dotnet ../fastforward/fastforward.dll translate-wf ${LOLA_OUTPUT} -m Soundness -f Lola -o "${TARGET_ROOT}/continuous/${FOLDER_NO_FILENAME}/${FILENAME_NO_K}_${i}-check" -k $i
        echo dotnet ../fastforward/fastforward.dll translate-wf ${LOLA_OUTPUT} -m Soundness -f Lola -o "${TARGET_ROOT}/lola/${FOLDER_NO_FILENAME}/${FILENAME_NO_K}_${i}-check" -k $i
        dotnet ../fastforward/fastforward.dll translate-wf ${LOLA_OUTPUT} -m Soundness -f Lola -o "${TARGET_ROOT}/lola/${FOLDER_NO_FILENAME}/${FILENAME_NO_K}_${i}-check" -k $i
        echo dotnet ../fastforward/fastforward.dll translate-wf ${LOLA_OUTPUT} -m Soundness -f PNML -o "${TARGET_ROOT}/woflan/${FOLDER_NO_FILENAME}/${FILENAME_NO_K}_${i}-check" -k $i
        dotnet ../fastforward/fastforward.dll translate-wf ${LOLA_OUTPUT} -m Soundness -f PNML -o "${TARGET_ROOT}/woflan/${FOLDER_NO_FILENAME}/${FILENAME_NO_K}_${i}-check" -k $i
        done
    done