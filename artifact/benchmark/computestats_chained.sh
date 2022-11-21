for i in {1..401..40}; do
    python3 benchmark_freechoice-soundness.py ../../instances/chained/chained_$i/ -o ../../results/chained/$i -t 60
done