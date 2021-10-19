set terminal tikz latex size 12cm, 7cm
set output 'fig_performance_vs_tools.tex'

set datafile separator ' '

set logscale
set size ratio -1
set xrange [0.01:10000]
set yrange [0.01:10000]
set xtics nomirror scale 1, 0 format "$10^{%T}$"
set ytics nomirror scale 1, 0 format "$10^{%T}$"
set key outside right Left spacing 2

set xlabel "Petrinizer (s)"
set ylabel "SPIN (s)"
set arrow 1 from 0.01,7200 to 10000,7200 nohead lw 0.5 lc rgb '#880000'
set arrow 2 from 7200,0.01 to 7200,10000 nohead lw 0.5 lc rgb '#880000'
set label "time limit/out of memory" at 0.02,2500 tc rgb '#880000'
plot x lt 3 notitle, \
    'slapnet-vs-spin-leaderelection.data' using 'slapnet':'spin' \
    with lines lt 1 notitle, \
    'slapnet-vs-spin-leaderelection.data' using 'slapnet':'spin' \
    pt 7 ps 1.0 lt 1 title "Leader Election", \
    'slapnet-vs-spin-snapshot.data' using 'slapnet':'spin' \
    with lines lt 2 notitle, \
    'slapnet-vs-spin-snapshot.data' using 'slapnet':'spin' \
    pt 7 ps 1.0 lt 2 title "Snapshot", \
    'slapnet-vs-spin-lamport.data' using 'slapnet':'spin' \
    with lines lt 4 notitle, \
    'slapnet-vs-spin-lamport.data' using 'slapnet':'spin' \
    pt 7 ps 1.0 lt 4 title "Lamport", \
    'slapnet-vs-spin-peterson.data' using 'slapnet':'spin' \
    with lines lt 5 notitle, \
    'slapnet-vs-spin-peterson.data' using 'slapnet':'spin' \
    pt 7 ps 1.0 lt 5 title "Peterson", \
    'slapnet-vs-spin-syzmanski.data' using 'slapnet':'spin' \
    with lines lt 6 notitle, \
    'slapnet-vs-spin-syzmanski.data' using 'slapnet':'spin' \
    pt 7 ps 1.0 lt 6 title "Szymanski"

