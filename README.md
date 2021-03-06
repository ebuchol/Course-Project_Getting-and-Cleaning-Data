# Course-Project_Getting-and-Cleaning-Data
This is the repository for the peer-graded course project in the Getting and Cleaning Data course through Coursera

## Description of Repository Contents:
This repository contains all of the contents needed for the peer-graded assignment in Coursera's "Getting and Cleaning Data" course. Specifically, the purpose of this project is to demonstrate the ability to collect, manipulate, and clean a data set with the goal of preparing tidy data from the original data set.  
  
In this repository, you will find the following contents in addition to this Readme.md file:
### run_analysis.R:
The R script run_analysis.R is the primary script that was used to do the following tasks:
1. Download, extract, and read the Human Activity Recognition Data which contains accelerometer and gyroscopic measurements across 30 study participants for 6 activities being performed.
2. Manipulate the data in order to extract all of the mean and standard deviation calculations from the raw data which contains 561 difeent variables.
3. Provide descriptive names for the data variables and activities being measured.
4. Generate an independent, tidy data set that contains each variable extracted; these variables will be averaged by each activity measured and for each of the study participants.
5. Write a table containing the merged and tidy data frames. These files will be named "data_merged.txt" and "data_tidy.txt" respectively.

Table of functions:  

Function Name | Required Objects | Description
------------- | ---------------- | -----------
run_analysis | none | The main function that runs the analysis and calls all other functions.
is.installed | package name | Tests whether a packaged needed for the analysis is installed on your computer.
fileDownload | none | Downloads and unzips the raw data for analysis.
readActivityLabels | file path | Reads and returns the index numbers for each of the 6 activities being measured.
readFiles | file path | Reads specified raw data and returns a data frame for use in analysis. Specifically, this function is used to retrieve the index numbers for the variables being measured, for the activities being performed in the test and training data, and for the subject participants included in the test and training data.
readMeasures | file path, subject ids, activity indices | Reads either the test or training data. Then it uses the data frames of the subjects ids and activity indices provided to add 2 new columns to the data. Finally, it returns a data frame containing 563 columns.
indexMean_STD | variable names | From a data frame of the 561 variables being measured, it uses regular expressions to identify which variables correspond to measurement involving either the mean() or std() functions. From this, it provides descriptive names for each of the variables and returns an index of the column numbers containing each of the desired variables along with their descriptive names.
mergeData | test data, training data, variable indices, activity indices | Merges the test and training data, then selects only the subject id, activity id, mean, and standard deviation columns. This extracted data is sorted by subject id and activity id. Finally, the names of the activity indices are used to provide descriptive names for the 6 activities being performed and returns this data frame.
makeTidy | merged data frame | Makes a tidy data frame from the merged data frame. The tidy data is averaged for each of the subjects participatng in the study and for each of the activities being performed. This tidy data frame containing 180 rows (30 participants and 6 activities) and 68 columns is returned.

### CodeBook.md
The codebook is a description of the process used to manipulate and transform the raw data into the merged and tidy data frames generated by the run_analysis.R script. This file contains the descriptions of the variables provided in the data as well as a summary of the data contained in "data_tidy.txt"

### data_merged.txt
A data frame containing the merged test and training data with only the subject participants, activities measured, and the mean and standard deviation variables included. This data frame was generated using the write.table() function and has dimensions 10299 rows by 68 columns.  

You can read in the merged data frame by:  

    data <- read.table("data_merged.txt", header = TRUE)
    View(data)

### data_tidy.txt
A data frame containing the tidy data set generated from the run_analysis.R script. This data frame contains the mean and standard deviation variables averaged for each of the 30 study participants and 6 activities being measured. This results in a data frame that was generated using the write.table() function that has dimensions 180 rows by 68 columns.

You can read in the tidy data frame by:  

    data <- read.table("data_tidy.txt", header = TRUE)
    View(data)


