#!/bin/bash
cd benchmarking
./benchmark_chained.sh
python3 benchmark_ascending-gen-soundness.py ../../instances/synthetic/generalized-soundness -o ../../results/generalized_soundness -t 60
python3 benchmark_structural-quasi-soundness.py ../../instances/synthetic/structural-reachability -o ../../results/structural-reachability -t 60