for i in 1 21 41 .. 401; do
    python3 benchmark_freechoice-soundness.py ../../instances/chained/chained_workflows_$i/ -o ../../results/chained/$i
done