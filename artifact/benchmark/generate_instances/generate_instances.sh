#!/bin/bash
NUMBERS="2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40"

python3 generate_struct_unsound.py ../nets/workflows/synthetic/structural/unsound/ -s $NUMBERS
python3 generate_struct_sound.py ../nets/workflows/synthetic/structural/sound/ -s $NUMBERS
python3 generate_struct_unsound.py ../nets/workflows/synthetic/structural/unreach/ -s $NUMBERS
python3 generate_struct_unsound.py ../nets/workflows/synthetic/generalized/unsound -s $NUMBERS