Course Project


Unzip the source (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) into a folder on your working directory, say C:\Users\yourname\Documents\R\

 ##Put run_analysis.R into C:\Users\yourname\Documents\R\UCI HAR Dataset\
 ##source("run_analysis.R") from the same directory (Assuming you are using linux)

Use data <- read.table("data_set_with_the_averages.txt") to read the data. It is 180x68 because there are 30 subjects and 6 activities, thus "for each activity and each subject" means 30 * 6 = 180 rows. Note that the provided R script has no assumptions on numbers of records, only on locations of files.
