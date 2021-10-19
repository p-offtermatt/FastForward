mkdir -p nets/generated_reachability_instances_notpruned/
python3 generate_instances.py -o nets/generated_reachability_instances_notpruned/ nets/largest_instances/*.lola -m Reachability -f Lola
