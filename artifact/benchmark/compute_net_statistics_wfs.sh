mkdir -p statistics
echo "Computing statistics..."
# python3 compute_net_statistics.py nets/workflows/workflows_transformed/IBM -o statistics/cont-sound/IBM_transformed.json -args="--cont-sound"
# python3 compute_net_statistics.py nets/workflows/workflows_transformed/IBM -o statistics/cont-deadlock/IBM_transformed.json -args="--cont-deadlock"
# python3 compute_net_statistics.py nets/workflows/workflows_transformed/IBM -o statistics/int-deadlock/IBM_transformed.json -args="--int-deadlock"
# python3 compute_net_statistics.py nets/workflows/workflows_transformed/SAP -o statistics/SAP_transformed.json -args="--check-small-bound-properties"

# python3 compute_net_statistics.py nets/workflows/workflows_reduced/IBM -o statistics/cont-sound/IBM_reduced.json -args="--cont-sound"
# python3 compute_net_statistics.py nets/workflows/workflows_reduced/IBM -o statistics/cont-deadlock/IBM_reduced.json -args="--cont-deadlock"
# python3 compute_net_statistics.py nets/workflows/workflows_reduced/IBM -o statistics/int-deadlock/IBM_reduced.json -args="--int-deadlock"
# python3 compute_net_statistics.py nets/workflows/workflows_reduced/SAP -o statistics/SAP_reduced.json -args="--check-small-bound-properties"

echo "Done"
