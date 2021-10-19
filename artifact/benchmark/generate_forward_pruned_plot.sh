python3 compute_consensus.py forward_pruned_results/*.json -o forward_pruned_results/consensus.txt
python3 generate-plot.py -o forward_pruned_coverability.tex forward_pruned_results/*.json -f only-unsafe --consensus forward_pruned_results/consensus.txt
