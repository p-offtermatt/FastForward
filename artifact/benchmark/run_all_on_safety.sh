TIMEOUT=60


mkdir -p results/

echo "Running on coverability instances without prepruning..."
python3 benchmark.py old_nets/* -o results/coverability.json -to $TIMEOUT -t FF-GBFS-qmarkingeq FF-astar-qmarkingeq FF-Dijkstra LoLA ICover Bfc MIST -mode cover
echo "Done, showing some stats about results for \"coverability\"..."
python3 evaluate_result.py results/safety.json
sleep 5  