#!/bin/bash
cd generate_instances
./generate_nets_full.sh
./prepare_synthetic_gen_sound.sh
./prepare_synthetic_struct_sound.sh
./prepare_synthetic_struct_reach.sh

./generate_chained_full.sh