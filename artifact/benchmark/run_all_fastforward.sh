TIMEOUT=60

mkdir -p results/

echo "Running on coverability instances without prepruning..."
python3 benchmark.py nets/coverability/* -o results/coverability-fastforward-all.json -to $TIMEOUT -t FF-GBFS-qmarkingeq FF-astar-qmarkingeq FF-Dijkstra FF-GBFS-nmarkingeq FF-astar-nmarkingeq FF-astar-qreachability FF-GBFS-qreachability -mode cover
echo "Done, showing some stats about results for \"coverability\"..."
python3 evaluate_result.py results/coverability-fastforward-all.json
sleep 5

echo "Running on coverability instances with prepruning..."
python3 benchmark.py nets/coverability_prepruned/* -o results/coverability_prepruned-fastforward-all.json -to $TIMEOUT -t FF-GBFS-qmarkingeq-nopruning FF-astar-qmarkingeq-nopruning FF-Dijkstra-nopruning FF-GBFS-nmarkingeq-nopruning FF-astar-nmarkingeq-nopruning FF-astar-qreachability-nopruning FF-GBFS-qreachability-nopruning -mode cover
echo "Done, showing some stats about results for \"coverability\"..."
python3 evaluate_result.py results/coverability-fastforward-all.json
sleep 5

echo "Running on random walk instances..."
python3 benchmark.py nets/random_walk/ -o results/random_walk-fastforward-all.json -to $TIMEOUT -t FF-GBFS-qmarkingeq FF-astar-qmarkingeq FF-Dijkstra FF-GBFS-nmarkingeq FF-astar-nmarkingeq FF-astar-qreachability FF-GBFS-qreachability -mode reach
echo "Done, showing some stats about results for \"random_walk\"..."
python3 evaluate_result.py results/random_walk-fastforward-all.json
sleep 5

echo "Running on sypet instances..."
python3 benchmark.py nets/sypet/ -o results/sypet-fastforward-all.json -to $TIMEOUT -t FF-GBFS-qmarkingeq FF-astar-qmarkingeq FF-Dijkstra FF-GBFS-nmarkingeq FF-astar-nmarkingeq FF-astar-qreachability FF-GBFS-qreachability -mode reach
echo "Done, showing some stats about results for \"sypet\"..."
python3 evaluate_result.py results/sypet-fastforward-all.json
sleep 5