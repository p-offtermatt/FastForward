TIMEOUT=60
PROBABILITY=10
SEED=1

if [ -z "$1" ]; then echo "No seed provided, using seed $SEED"; else echo "Seed provided, using seed $1"; SEED=$1; fi
sleep 2


mkdir -p results/

echo "Running on coverability instances without prepruning..."
python3 benchmark.py nets/coverability/* -o results/coverability.json -to $TIMEOUT -t FF-GBFS-qmarkingeq FF-astar-qmarkingeq FF-Dijkstra LoLA ICover Bfc MIST -mode cover -p $PROBABILITY --seed $SEED 
echo "Done, showing some stats about results for \"coverability\"..."
python3 evaluate_result.py results/coverability.json
sleep 5

echo "Running on coverability instances with prepruning..."
python3 benchmark.py nets/coverability_prepruned/* -o results/coverability_prepruned.json -to $TIMEOUT -t FF-GBFS-qmarkingeq-nopruning FF-astar-qmarkingeq-nopruning FF-Dijkstra-nopruning LoLA ICover Bfc MIST -mode cover -p $PROBABILITY --seed $SEED
echo "Done, showing some stats about results for \"coverability_prepruned\"..."
python3 evaluate_result.py results/coverability_prepruned.json
sleep 5

echo "Running on random walk instances..."
python3 benchmark.py nets/random_walk/ -o results/random_walk.json -to $TIMEOUT -t FF-GBFS-qmarkingeq FF-astar-qmarkingeq FF-Dijkstra LoLA -mode reach -p $PROBABILITY --seed $SEED
echo "Done, showing some stats about results for \"random_walk\"..."
python3 evaluate_result.py results/random_walk.json
sleep 5

echo "Running on sypet instances..."
python3 benchmark.py nets/sypet/ -o results/sypet.json -to $TIMEOUT -t FF-GBFS-qmarkingeq FF-astar-qmarkingeq FF-Dijkstra LoLA -mode reach -p $PROBABILITY --seed $SEED
echo "Done, showing some stats about results for \"sypet\"..."
python3 evaluate_result.py results/sypet.json
sleep 5

./generate_plots.sh