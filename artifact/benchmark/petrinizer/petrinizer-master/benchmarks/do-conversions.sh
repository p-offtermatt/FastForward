#!/bin/bash

for spec_file in `find . -name "*.spec"`; do
  echo "$spec_file"
  cat $spec_file | ./spec-to-petri-net.sh > $spec_file.pnet
done
