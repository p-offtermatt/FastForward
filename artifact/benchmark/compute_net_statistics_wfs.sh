mkdir -p statistics
echo "Computing statistics..."
python3 compute_net_statistics.py nets/workflows/workflows_original/IBM -o statistics/IBM_original.json
python3 compute_net_statistics.py nets/workflows/workflows_original/SAP -o statistics/SAP_original.json

python3 compute_net_statistics.py nets/workflows/workflows_transformed/IBM -o statistics/IBM_original.json
python3 compute_net_statistics.py nets/workflows/workflows_transformed/SAP -o statistics/SAP_original.json

echo "Done"
