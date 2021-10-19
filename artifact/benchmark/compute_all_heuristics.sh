mkdir -p heuristics
python3 calculate_initial_heuristics.py nets/coverability/* -o heuristics/coverability.json
python3 calculate_initial_heuristics.py nets/random_walk/ -o heuristics/random_walk.json
python3 calculate_initial_heuristics.py nets/sypet/ -o heuristics/sypet.json