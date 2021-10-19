TIMEOUT=60

mkdir -p results/

echo "Running on coverability instances without prepruning..."
python3 benchmark.py nets/coverability/* -o results/coverability.json -to $TIMEOUT -t FF-astar-qmarkingeq FF-GBFS-qmarkingeq FF-astar-euclidean FF-GBFS-euclidean -mode cover
echo "Done, showing some stats about results for \"coverability\"..."
python3 evaluate_result.py results/coverability.json
sleep 5

echo "Running on random walk instances..."
python3 benchmark.py nets/random_walk/ -o results/random_walk.json -to $TIMEOUT -t FF-astar-qmarkingeq FF-GBFS-qmarkingeq FF-astar-euclidean FF-GBFS-euclidean -mode reach
echo "Done, showing some stats about results for \"random_walk\"..."
python3 evaluate_result.py results/random_walk.json
sleep 5

echo "Running on sypet instances..."
python3 benchmark.py nets/sypet/ -o results/sypet.json -to $TIMEOUT -t FF-astar-qmarkingeq FF-GBFS-qmarkingeq FF-astar-euclidean FF-GBFS-euclidean -mode reach
echo "Done, showing some stats about results for \"sypet\"..."
python3 evaluate_result.py results/sypet.json
sleep 5

./generate_plots.sh