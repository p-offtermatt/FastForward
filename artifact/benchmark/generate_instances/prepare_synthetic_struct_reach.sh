#!/bin/bash
for FILE in ../nets/workflows/synthetic/structural-soundness-templates/*/*_1-check.lola;
    do
    echo "${FILE}"
    FILENAME=${FILE#../nets/workflows/synthetic/structural-templates/}
    FILENAME=${FILENAME%.lola}
    TARGET_ROOT="../nets/workflows/synthetic/structural-reachability"
    mkdir -p "${TARGET_ROOT}/continuous/${FILENAME%/*}"
    mkdir -p "${TARGET_ROOT}/lola/${FILENAME%/*}"
    # do not need to perform structural reachability reduction for fastforward, since struct. reach is solved by continuous reach
    # that's why mode is Reachability
    dotnet ../fastforward/fastforward.dll translate-wf ${FILE} -m Reachability -f Lola -o "${TARGET_ROOT}/continuous/${FILENAME}"
    dotnet ../fastforward/fastforward.dll translate-wf ${FILE} -m StructuralReachability -f Lola -o "${TARGET_ROOT}/lola/${FILENAME}"
    done