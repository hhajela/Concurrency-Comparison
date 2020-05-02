# Concurrency-Comparison
Analysis and comparison of application performance in use cases involving heavy concurrency

# Team Members
1. Nikhil Jain(nija5462)
2. Harshit Hajela(haha4350)

For changing the thread count on Java application, please change "numOfThread" variable in solution.java

For running the Haskell application run the following commands from the haskell implementation root directory :-

1. stack build
2. stack exec sudoku-parallel-exe -- input.txt +RTS -N2 -s

For adjusting the degree of parallelization the -N argument can be supplied with a different value (up to a maximum of the total number of processors on the machine)


