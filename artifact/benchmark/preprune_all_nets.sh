echo "Removing old files..."
rm -R nets/*_prepruned/*

echo "Creating directories for coverability..."
mkdir -p nets/coverability_prepruned/wahl-kroening
mkdir -p nets/coverability_prepruned/soter
mkdir -p nets/coverability_prepruned/medical
mkdir -p nets/coverability_prepruned/bug_tracking
mkdir -p nets/coverability_prepruned/mist-PN
mkdir -p nets/coverability_prepruned/mist-boundedPN
echo "Prepruning coverability..."
python3 preprune_nets.py nets/coverability/wahl-kroening/ nets/coverability_prepruned/wahl-kroening/
python3 preprune_nets.py nets/coverability/soter/ nets/coverability_prepruned/soter/
python3 preprune_nets.py nets/coverability/medical/ nets/coverability_prepruned/medical/
python3 preprune_nets.py nets/coverability/bug_tracking/ nets/coverability_prepruned/bug_tracking/
python3 preprune_nets.py nets/coverability/mist/PN/ nets/coverability_prepruned/mist-PN/

echo "Creating directory for random walk..."
mkdir -p nets/random_walk_prepruned
echo "Prepruning coverability..."
python3 preprune_nets.py nets/random_walk/ nets/random_walk_prepruned/

echo "Creating directory for sypet..."
mkdir -p nets/sypet_prepruned
echo "Prepruning sypet..."
python3 preprune_nets.py nets/sypet/ nets/sypet_prepruned/