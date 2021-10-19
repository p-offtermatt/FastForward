TIMEOUT=60


mkdir -p results/

echo "Running on coverability instances without prepruning..."
python3 benchmark.py old_nets/* -o results/safety_bfc.json -to $TIMEOUT -t Bfc -mode cover
echo "Done, showing some stats about results for \"coverability\"..."
python3 evaluate_result.py results/safety_bfc.json
sleep 5