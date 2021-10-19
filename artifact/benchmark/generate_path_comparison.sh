python3 compute_consensus.py new_results/*.json sypet-results/*.json generated_instances_results_notpruned/*.json -o consensus.txt

python3 generate-shortest-path-plot.py \
new_results/*.json \
sypet-results/*.json \
generated_instances_results_notpruned/*.json \
-o ~/git/a_star_main/tacas21/benchmark-figs/fig-paths.tex --consensus consensus.txt \
--caption "Length of the returned witness per tool, compared to the length of a shortest witness."