rm nets/generated_reachability_instances/*
mkdir -p nets/generated_reachability_instances
python3 generate_instances.py -o nets/generated_reachability_instances/ nets/largest_instances/*.lola -m Reachability -f Lola --prune
