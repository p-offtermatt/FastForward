#!/bin/bash
for FILE in ../nets/workflows/synthetic/structural-soundness/lola/*/*_1-check.lola;
    do
    echo "${FILE}"
    FILENAME=${FILE#../nets/workflows/synthetic/structural-soundness/lola/}
    FILENAME=${FILENAME%.lola}
    TARGET_ROOT="../nets/workflows/synthetic/structural-reachability"
    mkdir -p "${TARGET_ROOT}/fastforward/${FILENAME%/*}"
    cp ${FILE} "${TARGET_ROOT}/fastforward/${FILENAME}.lola"
    dotnet ../fastforward/fastforward.dll translate-wf ${FILE} -m StructuralReachability -f Lola -o "${TARGET_ROOT}/lola/${FILENAME}"
    done