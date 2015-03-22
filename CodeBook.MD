The functin main() from the script run_analysis.R outputs a table with the following information:

This table is a summarized version of the tables from the UCI HAR Dataset. Of the 500+ variables from the original data, only those with "mean()" or "std()" are kept.

A row in the output table shows -- for each variable -- the average value across a group of observations for the same activity and same subject.

Subject indicates the individual. Ranges from 1 to 30

Activity indicates the activity performed.

All other fields indicate averages grouped by subject and activity.

For instance, subject = 1; activity = "Standing"; and "Average of tBodyAcc.mean()" = 1 would indicate that of all observations in which the subject was 1 and activity was Standing, the avereage value of the tBodyAcc.mean() was 1.
