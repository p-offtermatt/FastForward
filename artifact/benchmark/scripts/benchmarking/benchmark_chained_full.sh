for i in 1 21 41 .. 401; do
    python3 benchmark_freechoice-soundness.py ../../instances/chained/chained_$i/ -o ../../results/chained/$i -t 120
done