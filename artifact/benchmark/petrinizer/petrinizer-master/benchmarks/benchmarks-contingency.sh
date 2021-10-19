#!/bin/bash

function sort_file {
  cat $1 | \
  sed -e 's/^[[:digit:]]* //' \
      -e 's/\.pl$//' \
      -e 's/\.pnet$//' \
      -e 's/\.spec$//' \
      -e 's/\.tts$//' \
      -e 's/\.lola$//' \
      -e 's/\.tpn$//' | \
  sort \
  >$1.sorted
}

results_our_tool=( positive dontknow error timeout )
results_other_tool=( positive negative error timeout )

our_tool=slapnet

#benchmark_dirs=( 'service-tech/ibm-soundness' 'service-tech/sap-reference' )
benchmark_dirs=( 'cav-benchmarks/mist' 'cav-benchmarks/wahl-kroening' 'cav-benchmarks/soter' )
benchmark_tools=( 'sara' 'sara' 'sara' ) # TODO: use positive/negative lists
for (( benchmark=0;benchmark<${#benchmark_dirs[@]};benchmark++)); do
  benchmark_dir=${benchmark_dirs[$benchmark]}
  other_tool=${benchmark_tools[$benchmark]}
  echo "$our_tool on $benchmark_dir compared with $other_tool"
  for result in "${results_our_tool[@]}"; do
    sort_file $benchmark_dir/$result-$our_tool.list
  done
  for result in "${results_other_tool[@]}"; do
    sort_file $benchmark_dir/$result-$other_tool.list
  done
  echo -n "other\\our| "
  for ((rour=0;rour<${#results_our_tool[@]};rour++)); do
    printf "%8s | " ${results_our_tool[$rour]}
    sums_our_tool[$rour]=0
  done
  echo
  echo -n "---------+-"
  for rour in "${results_our_tool[@]}"; do
    echo -n "---------+-"
  done
  echo "---------"
  for rother in "${results_other_tool[@]}"; do
    printf "%8s | " $rother
    sum_other_tool=0
    for ((rour=0;rour<${#results_our_tool[@]};rour++)); do
      n=`comm -12 $benchmark_dir/${results_our_tool[$rour]}-$our_tool.list.sorted $benchmark_dir/$rother-$other_tool.list.sorted | wc -l`
      printf "%8d | " $n
      sum_other_tool=$((sum_other_tool + n))
      sums_our_tool[$rour]=$((${sums_our_tool[$rour]} + n))
    done
    printf "%8d\n" $sum_other_tool
  done
  echo -n "---------+-"
  for rour in "${results_our_tool[@]}"; do
    echo -n "---------+-"
  done
  echo "---------"
  total_sum=0
  echo -n " our sum | "
  for ((rour=0;rour<${#results_our_tool[@]};rour++)); do
    printf "%8d | " ${sums_our_tool[$rour]}
    total_sum=$((total_sum + ${sums_our_tool[$rour]}))
  done
  printf "%8d\n" $total_sum
  echo -n "---------+-"
  for rour in "${results_our_tool[@]}"; do
    echo -n "---------+-"
  done
  echo "---------"
  total_time=0
  echo -n "our time | "
  for ((rour=0;rour<${#results_our_tool[@]};rour++)); do
    result=${results_our_tool[$rour]}
    result_time=0
    while read T file; do
      result_time=$((result_time + T))
    done <$benchmark_dir/$result-$our_tool.list
    printf "%1.2e | " $result_time
    total_time=$((total_time + $result_time))
  done
  printf "%1.2e\n" $total_time
  echo
done
