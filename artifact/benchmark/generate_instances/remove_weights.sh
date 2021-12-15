#!/bin/bash
for FILE in ../nets/workflows/synthetic/*/*/*.lola;
    do FILE_NO_ENDING=${FILE%.lola}
    echo $FILE
    dotnet ../fastforward/fastforward.dll replace-weights $FILE -f Lola -o $FILE_NO_ENDING;
    done