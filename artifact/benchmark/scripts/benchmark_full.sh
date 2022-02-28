#!/bin/bash
cd benchmarking
./benchmark_chained_full.sh
python3 benchmark_ascending-gen-soundness.py ../../instances/synthetic/generalized-soundness -o ../../results/generalized_soundness -t 120
python3 benchmark_structural-quasi-soundness.py ../../instances/synthetic/structural-reachability -o ../../results/structural-reachability -t 120