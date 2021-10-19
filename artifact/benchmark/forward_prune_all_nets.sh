rm nets/forward-pruned/*/*
mkdir -p nets/forward-pruned/wahl-kroening
mkdir -p nets/forward-pruned/soter
mkdir -p nets/forward-pruned/medical
mkdir -p nets/forward-pruned/bug_tracking
mkdir -p nets/forward-pruned/mist-PN
mkdir -p nets/forward-pruned/mist-boundedPN
python3 forward_prune_nets.py nets/wahl-kroening/ nets/forward-pruned/wahl-kroening/
python3 forward_prune_nets.py nets/soter/ nets/forward-pruned/soter/
python3 forward_prune_nets.py nets/medical/ nets/forward-pruned/medical/
python3 forward_prune_nets.py nets/bug_tracking/ nets/forward-pruned/bug_tracking/
python3 forward_prune_nets.py nets/mist/PN/ nets/forward-pruned/mist-PN/
python3 forward_prune_nets.py nets/mist/boundedPN/ nets/forward-pruned/mist-boundedPN/