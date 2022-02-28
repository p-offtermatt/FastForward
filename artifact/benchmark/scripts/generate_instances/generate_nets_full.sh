#!/bin/bash
NUMBERS="1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40"

BASE_PATH=../../instances/synthetic/

mkdir -p $BASE_PATH/structural-soundness-templates/unsound/
mkdir -p $BASE_PATH/structural-soundness-templates/sound/
mkdir -p $BASE_PATH/structural-soundness-templates/unreach/
mkdir -p $BASE_PATH/generalized-soundness-templates/unsound/
python3 generate_struct_unsound.py $BASE_PATH/structural-soundness-templates/unsound/ -nc -s $NUMBERS
python3 generate_struct_sound.py $BASE_PATH/structural-soundness-templates/sound/ -nc -s $NUMBERS
python3 generate_unreach.py $BASE_PATH/structural-soundness-templates/unreach/ -nc -s $NUMBERS
python3 generate_gen_sound_modulo.py $BASE_PATH/generalized-soundness-templates/unsound/ -nc -s $NUMBERS
