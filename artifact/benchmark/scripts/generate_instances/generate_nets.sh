#!/bin/bash
NUMBERS="1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70"

mkdir -p ../nets/workflows/synthetic/structural-soundness-templates/unsound/
mkdir -p ../nets/workflows/synthetic/structural-soundness-templates/sound/
mkdir -p ../nets/workflows/synthetic/structural-soundness-templates/unreach/
mkdir -p ../nets/workflows/synthetic/generalized-soundness-templates/unsound/
python3 generate_struct_unsound.py ../nets/workflows/synthetic/structural-soundness-templates/unsound/ -nc -s $NUMBERS
python3 generate_struct_sound.py ../nets/workflows/synthetic/structural-soundness-templates/sound/ -nc -s $NUMBERS
python3 generate_unreach.py ../nets/workflows/synthetic/structural-soundness-templates/unreach/ -nc -s $NUMBERS
python3 generate_gen_sound_modulo.py ../nets/workflows/synthetic/generalized-soundness-templates/unsound/ -nc -s $NUMBERS
