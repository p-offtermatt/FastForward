TIMEOUT=60


mkdir -p results/

echo "Running on coverability instances without prepruning..."
python3 benchmark.py nets/coverability/* -o results/coverability_saturation_search.json -to $TIMEOUT -t FF-saturation-search -mode cover
echo "Done, showing some stats about results for \"coverability\"..."
python3 evaluate_result.py results/coverability_saturation_search.json
sleep 5

echo "Running on random walk instances..."
python3 benchmark.py nets/random_walk/ -o results/random_walk_saturation_search.json -to $TIMEOUT -t FF-saturation-search -mode reach
echo "Done, showing some stats about results for \"random_walk\"..."
python3 evaluate_result.py results/random_walk_saturation_search.json
sleep 5

echo "Running on sypet instances..."
python3 benchmark.py nets/sypet/ -o results/sypet_saturation_search.json -to $TIMEOUT -t FF-saturation-search -mode reach
echo "Done, showing some stats about results for \"sypet\"..."
python3 evaluate_result.py results/sypet_saturation_search.json
sleep 5