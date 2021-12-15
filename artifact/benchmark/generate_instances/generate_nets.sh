#!/bin/bash
NUMBERS="2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40"

python3 generate_struct_unsound.py ../nets/workflows/synthetic/structural-soundness/lola/unsound/ -s $NUMBERS
python3 generate_struct_sound.py ../nets/workflows/synthetic/structural-soundness/lola/sound/ -s $NUMBERS
python3 generate_unreach.py ../nets/workflows/synthetic/structural-soundness/lola/unreach/ -s $NUMBERS
python3 generate_ge_sound_modulo.py ../nets/workflows/synthetic/generalized-soundness/lola/unsound/ -s $NUMBERS
