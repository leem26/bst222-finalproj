#!/bin/bash

# read in NHANES file and perform basic analysis
# to get simulation set up parameters, and produce
# two figures for initial report
R CMD BATCH R/nhanes_analysis.R 
echo "NHANES analysis and figures/tables done!"


# create populations, for each misclassification case
# and population size
R CMD BATCH R/gen_populations.R 
echo "Simulated populations generated!"


# END, all complete
echo "All scripts finished running!"