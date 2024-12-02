set terminal pngcairo enhanced font "arial,10" fontscale 1.0 size 600, 400 
set title "Simple Plots" font ",20"
set key fixed left top vertical Right noreverse enhanced autotitle box lt black linewidth 1.000 dashtype solid
set xrange [ * : * ] noreverse writeback
set x2range [ * : * ] noreverse writeback
set yrange [ * : * ] noreverse writeback
set y2range [ * : * ] noreverse writeback
set zrange [ * : * ] noreverse writeback
set cbrange [ * : * ] noreverse writeback
set rrange [ * : * ] noreverse writeback
set colorbox vertical origin screen 0.9, 0.2 size screen 0.05, 0.6 front  noinvert bdefault
NO_ANIMATION = 1
set samples 400, 400

set xlabel "Iterations"
set ylabel "Time Taken (ns)"

set output './images/gnuplot_fortran_time_vs_iterations_graph.png'

f(x) = a*x + b     
g(x) = c*x + d         
fit f(x) './src/output/fortran_out.txt' using 2:3 via a,b
fit g(x) './src/output/fortran_out_multithread.txt' using 2:3 via c,d

plot './src/output/fortran_out.txt' using 2:3 w p title "FORTRAN" lc rgb '#1e90ff', \
f(x) title "" lc rgb '#1e90ff', \
'./src/output/fortran_out_multithread.txt' using 2:3 w p title "FORTRAN (Multithreaded)" lc rgb '#FF2054', \
g(x) title "" lc rgb '#FF2054'