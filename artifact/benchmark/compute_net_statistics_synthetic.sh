mkdir -p statistics
echo "Computing statistics..."
python3 compute_net_statistics.py instances/synthetic/generalized-soundness/continuous/unsound -o statistics/synthetic-gensound.json -args="--int-deadlock --cont-deadlock --cont-sound"

echo "Done"
