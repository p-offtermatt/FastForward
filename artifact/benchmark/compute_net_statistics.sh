mkdir -p statistics
echo "Computing statistics..."
python3 compute_net_statistics.py nets/coverability/* -o statistics/coverability.json --prune
python3 compute_net_statistics.py nets/random_walk -o statistics/random_walk.json --prune
python3 compute_net_statistics.py nets/sypet/ -o statistics/sypet.json --prune

echo "Plotting results..."
python3 generate-preprocessing-plot.py statistics/*.json -ot plots/data/pruning_transitions_data.tex -op plots/data/pruning_places_data.tex

echo "Done"
