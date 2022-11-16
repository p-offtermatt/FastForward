mkdir -p statistics
echo "Computing statistics..."
python3 compute_net_statistics.py nets/workflows/workflows_transformed/IBM -o statistics/IBM_transformed.json
python3 compute_net_statistics.py nets/workflows/workflows_transformed/SAP -o statistics/SAP_transformed.json

python3 compute_net_statistics.py nets/workflows/workflows_reduced/IBM -o statistics/IBM_reduced.json
python3 compute_net_statistics.py nets/workflows/workflows_reduced/SAP -o statistics/SAP_reduced.json

echo "Done"
