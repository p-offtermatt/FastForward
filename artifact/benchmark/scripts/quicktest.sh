(cd ..; mkdir -p results/quicktest; python3 compute_net_statistics.py nets/quicktest -o results/quicktest/test.json -args="--cont-sound --cont-deadlock --int-deadlock  --check-small-bound-properties "; python3 check_quicktest.py -i results/quicktest/test.json)