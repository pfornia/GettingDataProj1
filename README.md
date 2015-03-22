# GettingDataProj1
Analysis performed for the project for Coursera's "Getting Data" course.

This repo contains one script called run_analysis.R. This script contains only one function, called main(). Main() reads in UCI HAR data, combines "Training" and "Test" data sets, and strips out all variables other than those calculating a mean or standard deviation. It then calculates the average value of each of the 68 variables grouped by subject and activity performed, and outputs these avereages in a table to UCI_summary_stats.txt.

This repo also contains a brief data codebook called CodeBook.md explaining these variables.
