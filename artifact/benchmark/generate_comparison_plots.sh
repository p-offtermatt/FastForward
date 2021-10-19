python3 compute_consensus.py new_results/*.json -o new_results/consensus.txt
python3 compute_consensus.py new_results/*.json sypet-results/*.json generated_instances_results_notpruned/*.json -o reachability_consensus.txt


echo "-- BFC --"
python3 generate-comparison-plot.py \
new_results/*.json \
--consensus new_results/consensus.txt \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-comparison-bfc.tex \
-t1 "FastForward_QMarkingEQGurobi+Pruning+Competitive" \
-t2 "BFC" \
--nofigure \
--caption "" \
-t1no

echo "-- MIST-Backward --"
python3 generate-comparison-plot.py \
new_results/*.json \
--consensus new_results/consensus.txt \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-comparison-mist.tex \
-t1 "FastForward_QMarkingEQGurobi+Pruning+Competitive" \
-t2 "MIST" \
--nofigure \
--caption "" \
-t1no

echo "-- ICover --"
python3 generate-comparison-plot.py \
new_results/*.json \
--consensus new_results/consensus.txt \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-comparison-icover.tex \
-t1 "FastForward_QMarkingEQGurobi+Pruning+Competitive" \
-t2 "ICover" \
--nofigure \
--caption "" \
-t1no

echo "-- Lola --"
python3 generate-comparison-plot.py \
new_results/*.json \
sypet-results/all-forwardpruning.json \
random_walk_results_notprepruned/all-forwardpruning.json \
--consensus results/consensus.txt \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-comparison-lola.tex \
-t1 "FastForward_QMarkingEQGurobi+Pruning+Competitive" \
-t2 "LoLA" \
--nofigure \
--caption "" \
-t1no

echo "-- Dijkstra --"
python3 generate-comparison-plot.py \
new_results/*.json \
sypet-results/all-forwardpruning.json \
random_walk_results_notprepruned/all-forwardpruning.json \
--consensus results/consensus.txt \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-comparison-dijkstra.tex \
-t1 "FastForward_QMarkingEQGurobi+Pruning+Competitive" \
-t2 "FastForward_zero+Pruning+Competitive" \
--nofigure \
--caption "" \
-t1no

echo "-- GBFS --"
python3 generate-comparison-plot.py \
new_results/*.json \
sypet-results/all-forwardpruning.json \
random_walk_results_notprepruned/all-forwardpruning.json \
--consensus results/consensus.txt \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-comparison-gbfs.tex \
-t1 "FastForward_QMarkingEQGurobi+Pruning+Competitive" \
-t2 "FastForward_GBFS_QMarkingEQGurobi+Pruning+Competitive" \
--nofigure \
--caption "" \
-t1no

### GBFS

echo "-- BFC --"
python3 generate-comparison-plot.py \
new_results/*.json \
--consensus new_results/consensus.txt \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-gbfs-comparison-bfc.tex \
-t1 "FastForward_GBFS_QMarkingEQGurobi+Pruning+Competitive" \
-t2 "BFC" \
--nofigure \
--caption "" \
-t1no

echo "-- MIST-Backward --"
python3 generate-comparison-plot.py \
new_results/*.json \
--consensus new_results/consensus.txt \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-gbfs-comparison-mist.tex \
-t1 "FastForward_GBFS_QMarkingEQGurobi+Pruning+Competitive" \
-t2 "MIST" \
--nofigure \
--caption "" \
-t1no

echo "-- ICover --"
python3 generate-comparison-plot.py \
new_results/*.json \
--consensus new_results/consensus.txt \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-gbfs-comparison-icover.tex \
-t1 "FastForward_GBFS_QMarkingEQGurobi+Pruning+Competitive" \
-t2 "ICover" \
--nofigure \
--caption "" \
-t1no

echo "-- Lola --"
python3 generate-comparison-plot.py \
new_results/*.json \
sypet-results/all-forwardpruning.json \
random_walk_results_notprepruned/all-forwardpruning.json \
--consensus results/consensus.txt \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-gbfs-comparison-lola.tex \
-t1 "FastForward_GBFS_QMarkingEQGurobi+Pruning+Competitive" \
-t2 "LoLA" \
--nofigure \
--caption "" \
-t1no

echo "-- Dijkstra --"
python3 generate-comparison-plot.py \
new_results/*.json \
sypet-results/all-forwardpruning.json \
random_walk_results_notprepruned/all-forwardpruning.json \
--consensus results/consensus.txt \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-gbfs-comparison-dijkstra.tex \
-t1 "FastForward_GBFS_QMarkingEQGurobi+Pruning+Competitive" \
-t2 "FastForward_zero+Pruning+Competitive" \
--nofigure \
--caption "" \
-t1no

echo "-- GBFS --"
python3 generate-comparison-plot.py \
new_results/*.json \
sypet-results/all-forwardpruning.json \
random_walk_results_notprepruned/all-forwardpruning.json \
--consensus results/consensus.txt \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-gbfs-comparison-gbfs.tex \
-t1 "FastForward_GBFS_QMarkingEQGurobi+Pruning+Competitive" \
-t2 "FastForward_QMarkingEQGurobi+Pruning+Competitive" \
--nofigure \
--caption "" \
-t1no