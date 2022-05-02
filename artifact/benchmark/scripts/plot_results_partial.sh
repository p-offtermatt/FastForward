cd plotting
mkdir -p ../../plots/data/struct-reach/sound/
mkdir -p ../../plots/data/struct-reach/unreach/
mkdir -p ../../plots/data/struct-reach/unsound/

mkdir -p ../../plots/data/gen-sound/

mkdir -p ../../plots/data/free-choice/

python3 summarize_struct_reach.py ../../results/structural-reachability_sound.json -t ../../plots/data/struct-reach/sound/ -to 60
python3 summarize_struct_reach.py ../../results/structural-reachability_unreach.json -t ../../plots/data/struct-reach/unreach/ -to 60
python3 summarize_struct_reach.py ../../results/structural-reachability_unsound.json -t ../../plots/data/struct-reach/unsound/ -to 60
python3 summarize_gensound.py ../../results/generalized_soundness_unsound.json -t ../../plots/data/gen-sound/ -to 60
python3 summarize_chained.py ../../results/chained/*.json -t ../../plots/data/free-choice/ -to 60
