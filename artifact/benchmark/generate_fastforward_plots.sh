echo "Generating plots..."
mkdir -p plots/data
rm plots/data/*
python3 generate-plot.py results/coverability-fastforward-all.json -o plots/data/coverability_data.tex
python3 generate-plot.py results/random_walk-fastforward-all.json -o plots/data/random_walk_data.tex --logscale
python3 generate-plot.py results/sypet-fastforward-all.json -o plots/data/sypet_data.tex
