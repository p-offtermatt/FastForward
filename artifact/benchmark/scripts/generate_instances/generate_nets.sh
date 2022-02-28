#!/bin/bash
NUMBERS="1 4 7 10 13 16 19 22 25 28 31 34 37 40"

BASE_PATH=../../instances/synthetic/

mkdir -p $BASE_PATH/structural-soundness-templates/unsound/
mkdir -p $BASE_PATH/structural-soundness-templates/sound/
mkdir -p $BASE_PATH/structural-soundness-templates/unreach/
mkdir -p $BASE_PATH/generalized-soundness-templates/unsound/
python3 generate_struct_unsound.py $BASE_PATH/structural-soundness-templates/unsound/ -nc -s $NUMBERS
python3 generate_struct_sound.py $BASE_PATH/structural-soundness-templates/sound/ -nc -s $NUMBERS
python3 generate_unreach.py $BASE_PATH/structural-soundness-templates/unreach/ -nc -s $NUMBERS
python3 generate_gen_sound_modulo.py $BASE_PATH/generalized-soundness-templates/unsound/ -nc -s $NUMBERS
