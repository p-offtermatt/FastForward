mkdir -p nets/random_walk_instances_prepruned
mkdir -p nets/random_walk_instances_notpruned
for file in $(cat kept_instances.txt); do cp nets/generated_reachability_instances/$file* nets/random_walk_instances_prepruned; done
for file in $(cat kept_instances.txt); do cp nets/generated_reachability_instances_notpruned/$file* nets/random_walk_instances_notpruned; done
