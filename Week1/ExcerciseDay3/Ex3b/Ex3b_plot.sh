#!/bin/bash

cd outputs

#Mak movie (requires gnuplot and ffmpeg executables)
echo "Plotting the results..."
name=''
for i in {1..200}
do
    printf -v name 'diff.%6.6d' $i
    cat > .tmp <<-END
    set term jpeg;
    sp '$name.dat' u 1:2:3 w l title 'time step: $i';
    set xlabel 'x';
    set ylabel 'y';
    set zlabel 'T';
    repl;
    quit;
END
    gnuplot .tmp > $name.jpeg
done
rm .tmp
echo "Making the movie..."
ffmpeg -hide_banner -loglevel error -y -i diff.%06d.jpeg diff.mpeg
# mkdir -p res
# mv diff*.dat diff*.jpeg res
mv diff.mpeg ..
echo "Done!"