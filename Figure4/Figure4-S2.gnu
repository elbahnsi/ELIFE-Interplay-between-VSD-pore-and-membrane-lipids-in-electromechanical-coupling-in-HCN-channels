#gnuplot -i Figure4-S2.gnu

set terminal pngcairo size 1500,1500 font "Arial,30"
set encoding iso_8859_1
set output "Figure4-S2.png"
set multiplot layout 3,2
set key font "Arial,25"

set xrange [0:2000]
set yrange [90:120]
set xlabel "Time (ns)"
set ylabel "Z ({\305})"
set ytics 20
set xtics 500


set xtics add ("250" 500)
set xtics add ("500" 1000)
set xtics add ("750" 1500)
set xtics add ("1000" 2000)

set title "Resting"

plot for [col=1:217] "./data/Resting/zpot.dat" u 0:col w p title '' 

set title "Activated"
plot for [col=1:217] "./data/Activated/zpot.dat" u 0:col w p title '' 

set title "A1"
plot for [col=1:217] "./data/A1/zpot.dat" u 0:col w p title '' 

set title "R1"
plot for [col=1:200] "./data/R1/zpot.dat" u 0:col w p title '' 

set title "A2"
plot for [col=1:217] "./data/A2/zpot.dat" u 0:col w p title ''

set title "R2"
plot for [col=1:200] "./data/R2/zpot.dat" u 0:col w p title ''


unset multiplot
unset output
unset terminal
q


