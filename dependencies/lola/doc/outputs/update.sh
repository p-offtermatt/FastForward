#!/bin/bash

#set -x #echo on

BINDIR=../../../src
EXAMPLEDIR=../../examples

function create_output {
    cd $1
    echo "\$ $2" > $1.txt
    eval $BINDIR/$2 2>> $1.txt
    cat $1.txt | fold -s -w 96 > ../$1.txt
    cd ..
    rm -fr $1
}

mkdir -p output1
cp $EXAMPLEDIR/phils/phils10.lola output1
create_output output1 "lola --check=none phils10.lola"

mkdir -p output2
cp $EXAMPLEDIR/phils/phils10.lola output2
create_output output2 "lola --check=full phils10.lola"

mkdir -p output3
cp $EXAMPLEDIR/phils/phils10.lola output3
create_output output3 "lola --formula=\"EF DEADLOCK\" phils10.lola"

mkdir -p output4
cp $EXAMPLEDIR/phils/phils10.lola output4
create_output output4 "lola --formula=\"EF ea.2 > 0\" phils10.lola"

mkdir -p output5
cp $EXAMPLEDIR/phils/phils10.lola output5
create_output output5 "lola --formula=\"EF (ea.2 > 0 AND ea.3 > 0)\" phils10.lola"

mkdir -p output6
cp $EXAMPLEDIR/phils/phils10.lola output6
create_output output6 "lola --formula=\"AG (ea.2 = 0 OR ea.3 = 0)\" phils10.lola"

mkdir -p output7
cp $EXAMPLEDIR/phils/phils10.lola output7
create_output output7 "lola --formula=\"AG hl.3 <= 1\" phils10.lola"

mkdir -p output8
cp $EXAMPLEDIR/phils/phils10.lola output8
create_output output8 "lola --formula=\"AG NOT FIREABLE(tl.[y=3])\" phils10.lola"

mkdir -p output9
cp $EXAMPLEDIR/phils/phils10.lola output9
create_output output9 "lola --formula=\"AGEF FIREABLE(tl.[y=3])\" phils10.lola"

mkdir -p output10
cp $EXAMPLEDIR/phils/phils10.lola output10
create_output output10 "lola --formula=\"AGEF hl.1 > 0\" phils10.lola"

mkdir -p output11
cp $EXAMPLEDIR/phils/phils10.lola output11
create_output output11 "lola --formula=\"AGEF INITIAL\" phils10.lola"

mkdir -p output12
cp $EXAMPLEDIR/planner/planner.lola output12
create_output output12 "lola --search=cover --encoder=full --formula=\"AG Sent2Disp.<NEC-MT1065|Doc2|FALSE> < oo\" planner.lola"

mkdir -p output13
cp $EXAMPLEDIR/phils/phils10.lola output13
create_output output13 "lola --formula=\"AG ea.1 != 1\" --json=../output13.tmp --jsoninclude=path phils10.lola"
jq < output13.tmp "." > output13.txt
rm output13.tmp

mkdir -p output14
cat << EOF > output14/net.lola
PLACE p1, p2, p3, p4;

MARKING p1, p2;

TRANSITION t
CONSUME p1, p2;
PRODUCE p5;

EOF
create_output output14 "lola --check=none net.lola"

mkdir -p output15
cp $EXAMPLEDIR/garavel/garavel.lola output15
create_output output15 "lola --search=findpath --formula=\"EF FIREABLE(t520)\" garavel.lola"

mkdir -p output16
cp $EXAMPLEDIR/phils/phils10.lola output16
create_output output16 "lola --formula=\"EF DEADLOCK\" --search=sweep phils10.lola"

mkdir -p output17
cp $EXAMPLEDIR/phils/phils10.lola output17
create_output output17 "lola --formula=\"EF DEADLOCK\" --quiet phils10.lola"

mkdir -p output18
cp $EXAMPLEDIR/phils/phils10.lola output18
create_output output18 "lola --formula=\"EF DEADLOCK\" --state=../output18.tmp phils10.lola"
mv output18.tmp output18.txt

mkdir -p output19
cp $EXAMPLEDIR/phils/phils10.lola output19
create_output output19 "lola --formula=\"EF DEADLOCK\" --path=../output19.tmp phils10.lola"
mv output19.tmp output19.txt
