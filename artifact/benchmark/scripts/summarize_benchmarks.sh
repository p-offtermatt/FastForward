(cd ..; 
python3 summarize.py -ts results/small-bound-properties/IBM_transformed.json -rs results/small-bound-properties/IBM_reduced.json -tcs results/continuous-soundness/IBM_transformed.json -rcs results/continuous-soundness/IBM_reduced.json -tcd results/continuous-deadlocks/IBM_transformed.json -rcd results/continuous-deadlocks/IBM_reduced.json -tid results/integer-deadlocks/IBM_transformed.json -rid results/integer-deadlocks/IBM_reduced.json)