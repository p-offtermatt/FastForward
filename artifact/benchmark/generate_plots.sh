echo "Generating plots..."
mkdir -p plots/data
rm plots/data/*
python3 generate-plot.py results/coverability.json -o plots/data/coverability_data.tex
python3 generate-plot.py results/coverability_prepruned.json -o plots/data/coverability_prepruned_data.tex
python3 generate-plot.py results/random_walk.json -o plots/data/random_walk_data.tex --logscale
python3 generate-plot.py results/sypet.json -o plots/data/sypet_data.tex

python3 generate-comparison-plot.py -o plots/data/a-star_dijkstra_data.tex -t1 "FastForward_a-starQMarkingEQGurobi+Pruning" -t2 FastForward_a-starzero+Pruning results/coverability.json results/random_walk.json results/sypet.json

python3 generate-comparison-plot.py -o plots/data/a-star_mist_data.tex -t1 "FastForward_a-starQMarkingEQGurobi+Pruning" -t2 MIST results/coverability.json results/random_walk.json results/sypet.json

python3 generate-comparison-plot.py -o plots/data/a-star_bfc_data.tex -t1 "FastForward_a-starQMarkingEQGurobi+Pruning" -t2 BFC results/coverability.json results/random_walk.json results/sypet.json

python3 generate-comparison-plot.py -o plots/data/a-star_icover_data.tex -t1 "FastForward_a-starQMarkingEQGurobi+Pruning" -t2 ICover results/coverability.json results/random_walk.json results/sypet.json

python3 generate-comparison-plot.py -o plots/data/a-star_gbfs_data.tex -t1 "FastForward_a-starQMarkingEQGurobi+Pruning" -t2 "FastForward_best-firstQMarkingEQGurobi+Pruning" results/coverability.json results/random_walk.json results/sypet.json

python3 generate-comparison-plot.py -o plots/data/a-star_lola_data.tex -t1 "FastForward_a-starQMarkingEQGurobi+Pruning" -t2 LoLA results/coverability.json results/random_walk.json results/sypet.json

python3 generate-comparison-plot.py -o plots/data/gbfs_dijkstra_data.tex -t1 "FastForward_best-firstQMarkingEQGurobi+Pruning" -t2 FastForward_a-starzero+Pruning results/coverability.json results/random_walk.json results/sypet.json

python3 generate-comparison-plot.py -o plots/data/gbfs_mist_data.tex -t1 "FastForward_best-firstQMarkingEQGurobi+Pruning" -t2 MIST results/coverability.json results/random_walk.json results/sypet.json

python3 generate-comparison-plot.py -o plots/data/gbfs_bfc_data.tex -t1 "FastForward_best-firstQMarkingEQGurobi+Pruning" -t2 BFC results/coverability.json results/random_walk.json results/sypet.json

python3 generate-comparison-plot.py -o plots/data/gbfs_icover_data.tex -t1 "FastForward_best-firstQMarkingEQGurobi+Pruning" -t2 ICover results/coverability.json results/random_walk.json results/sypet.json

python3 generate-comparison-plot.py -o plots/data/gbfs_a-star_data.tex -t1 "FastForward_best-firstQMarkingEQGurobi+Pruning" -t2 "FastForward_a-starQMarkingEQGurobi+Pruning" results/coverability.json results/random_walk.json results/sypet.json

python3 generate-comparison-plot.py -o plots/data/gbfs_lola_data.tex -t1 "FastForward_best-firstQMarkingEQGurobi+Pruning" -t2 LoLA results/coverability.json results/random_walk.json results/sypet.json

python3 generate-shortest-path-plot.py results/coverability.json results/random_walk.json results/sypet.json -o plots/data/shortest_path_data.tex -t BFC LoLA "FastForward_best-firstQMarkingEQGurobi+Pruning"
