(cd ..;
mkdir -p results/continuous-soundness;
mkdir -p results/integer-deadlocks;
mkdir -p results/continuous-deadlocks;
mkdir -p results/statistics;
mkdir -p results/small-bound-properties;
python3 compute_net_statistics.py nets/workflows/workflows_transformed/IBM -o results/continuous-soundness/IBM_transformed.json -args="--cont-sound";
python3 compute_net_statistics.py nets/workflows/workflows_transformed/IBM -o results/integer-deadlocks/IBM_transformed.json -args="--int-deadlock";
python3 compute_net_statistics.py nets/workflows/workflows_transformed/IBM -o results/continuous-deadlocks/IBM_transformed.json -args="--cont-deadlock";
python3 compute_net_statistics.py nets/workflows/workflows_transformed/IBM -o results/small-bound-properties/IBM_transformed.json -args="check-small-bound-properties";
python3 compute_net_statistics.py nets/workflows/workflows_reduced/IBM -o results/continuous-soundness/IBM_reduced.json -args="--cont-sound";
python3 compute_net_statistics.py nets/workflows/workflows_reduced/IBM -o results/integer-deadlocks/IBM_reduced.json -args="--int-deadlock";
python3 compute_net_statistics.py nets/workflows/workflows_reduced/IBM -o results/continuous-deadlocks/IBM_reduced.json -args="--cont-deadlock";
python3 compute_net_statistics.py nets/workflows/workflows_reduced/IBM -o results/small-bound-properties/IBM_reduced.json -args="check-small-bound-properties";
)