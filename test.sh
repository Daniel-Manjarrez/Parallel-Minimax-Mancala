#!/bin/bash

# Loop through combinations of depth and parallelDepth
for depth in 4 5 6 7 8 9
do
    for parallelDepth in 1 2 4 8
    do
        echo "Running ParaMancala with depth=$depth, parallelDepth=$parallelDepth"
        paraMancala_time=$( { time ./ParaMancala $depth $parallelDepth > /dev/null 2>&1; } 2>&1 )

        echo "Running ParaMancala2 with depth=$depth, parallelDepth=$parallelDepth"
        paraMancala2_time=$( { time ./ParaMancala2 $depth $parallelDepth > /dev/null 2>&1; } 2>&1 )

        paraMancala_real_time=$(echo "$paraMancala_time" | grep real | awk '{print $2}')
        paraMancala2_real_time=$(echo "$paraMancala2_time" | grep real | awk '{print $2}')

        echo "ParaMancala time: $paraMancala_real_time"
        echo "ParaMancala2 time: $paraMancala2_real_time"
        echo "----------------------------------------"
    done
done
