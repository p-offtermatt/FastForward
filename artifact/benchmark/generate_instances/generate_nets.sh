#!/bin/bash
NUMBERS="2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40"

mkdir -p ../nets/workflows/synthetic/structural-soundness-templates/unsound/
mkdir -p ../nets/workflows/synthetic/structural-soundness-templates/sound/
mkdir -p ../nets/workflows/synthetic/structural-soundness-templates/unreach/
mkdir -p ../nets/workflows/synthetic/generalized-soundness-templates/unsound/
python3 generate_struct_unsound.py ../nets/workflows/synthetic/structural-soundness-templates/unsound/ -nc -s $NUMBERS
python3 generate_struct_sound.py ../nets/workflows/synthetic/structural-soundness-templates/sound/ -nc -s $NUMBERS
python3 generate_unreach.py ../nets/workflows/synthetic/structural-soundness-templates/unreach/ -nc -s $NUMBERS
python3 generate_gen_sound_modulo.py ../nets/workflows/synthetic/generalized-soundness-templates/unsound/ -nc -s $NUMBERS
