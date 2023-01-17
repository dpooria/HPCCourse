#!/bin/bash

cd outputs

#Mak movie (requires gnuplot and ffmpeg executables)
echo "Plotting the results..."
name=''
for i in {1..220}
do
    printf -v name 'diff.%6.6d' $i
    cat > .tmp <<-END
    set term jpeg;
    set xlabel 'x';
    set ylabel 'y';
    set zlabel 'T';
    sp '$name.dat' u 1:2:3 w l title 'time step: $i';
    repl;
    quit;
END
    gnuplot .tmp > $name.jpeg
done
echo "Making the movie..."
ffmpeg -hide_banner -loglevel error -y -i diff.%06d.jpeg diff.mpeg
# mkdir -p res
# mv diff*.dat diff*.jpeg res
mv diff.mpeg ..
rm .tmp
# Plot diagnostic file
cd ..
cat > .tmp <<-END
    set term jpeg;
    set xlabel 't';
    set ylabel 'Min(T)';
    set style line 1 lt 2 lw 2 pt 3 ps 1.0
    p 'diag.dat' u 1:2 w l ls 1 title 'Diagnostics';
    repl;
    quit;
END
gnuplot .tmp > diag.jpeg
rm .tmp
unset name
echo "Done!"