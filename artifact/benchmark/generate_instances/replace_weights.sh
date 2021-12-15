#!/bin/bash
for FILE in ../nets/workflows/synthetic/*/lola/*/*.lola;
    do FILE_NO_ENDING=${FILE%.lola}
    echo $FILE_NO_ENDING
    dotnet ../fastforward/fastforward.dll replace-weights $FILE -f Lola -o $FILE_NO_ENDING;
    done