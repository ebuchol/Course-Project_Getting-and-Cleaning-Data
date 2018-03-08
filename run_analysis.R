## This is the analysis script for completing the peer-reviewed course
## project in the Getting and Cleaning Data course through Coursera.

is.installed <- function(package) {
  ## This function tests whether a needed package is installed on your machine.
    is.element(package, installed.packages()[,1])
}

fileDownload <- function() {
  ## This function downloads and unzips the data for analysis
    if(!file.exists("data")) {dir.create("data")}
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    zipFile <- "./data/Human-Activity-Recognition.zip"
    download.file(fileURL, destfile = zipFile)
    path <- sub(".zip", "", zipFile)
    unzip(zipFile, exdir = path)
}

readActivityLabels <- function(path) {
  ## This function reads and returns the index numbers for each of the activities measured.
    data <- read.table(path, stringsAsFactors = FALSE)
    activities <- as.list(data[,1])
    names(activities) <- tolower(data[,2])
    names(activities) <- sub("_", ".", names(activities))
    return(activities)
}

readFiles <- function(path) {
  ## This function reads and returns a data frame of the index number for either the
  ## features measured, the specific subject being tested, or the activity being performed.
  ## The features will be used as the column indices for the test and training data.
  ## The subjects will correspond to the rows with the test and training data.
  ## The activity values will tell us what activities are being measured in each row of
  ## the test and training data.
  read.table(path, stringsAsFactors = FALSE)
}

readMeasures <- function(path, subject, activity) {
  ## This function reads the test and training data from the dataset, adds a new column
  ## for the subject being tested and the activity being performed, and then returns
  ## the modified training set.
    data <- read.table(path, stringsAsFactors = FALSE)
    colnames(data) <- 1:ncol(data)
    data %>% mutate(subject = subject, activity = activity)
}

indexMean_STD <- function(feat) {
  ## This function identifies the column indices in the test and training data frames
  ## that correspond to the mean() and std() variables. The function will return a
  ## list of indices in which each elements name is the variable name in the data set.
  ## This function also changes the indexed variable names to be more descriptive
  ## and consistent.
    pattern <- "(.+)(mean|std)\\(\\)"
    index <- grep(pattern, feat[,2])
    names(index) <- grep(pattern, feat[,2], value = T)
    items <- list(c("^t", "time."), c("^f", "freq."), c("BodyBody", "Body"),
        c("-mean\\(\\)", ".Mean"), c("-std\\(\\)", ".STD"),
        c("-X", ".Xaxis"), c("-Y", ".Yaxis"), c("-Z", ".Zaxis"))
    for(each in items) {
        names(index) <- sub(each[1], each[2], names(index))
    }
    return(index)
}

mergeData <- function(test, train, index, activity) {
  ## This function merges the test and training data frames, selects only the subject,
  ## activity, mean, and std variables, and sorts the data by subject and activity.
  ## Finally, the integer values of the activity variable are converted to their
  ## corresponding activity, e.g. "walking" ... "laying"
    yes <- c(1:(ncol(test) - 2) %in% index, FALSE, FALSE)
    filterTest <- cbind(subject = test$subject,
        activity = test$activity, test[, yes])
    filterTrain <- cbind(subject = train$subject,
        activity = train$activity, train[, yes])
    mergeDF <- merge(filterTest, filterTrain, all = TRUE)
    mergeDF <- arrange(mergeDF, subject, activity)
    colnames(mergeDF) <- c('subject', 'activity', names(index))
    mergeDF$activity <- names(activity[mergeDF$activity])
    return(mergeDF)
}

makeTidy <- function(mergeDF) {
  ## This function makes a tidy data frame out of the merged data frame
  ## from the mergeData function.
    tidy <- mergeDF %>% group_by(activity, subject) %>%
        summarize_all(funs(mean)) %>%
        select(subject, everything())
    return(tidy)
}

run_analysis <- function() {
  ## This is the main function that runs the analysis and calls all other functions.
    if(!is.installed("dplyr")) {
        print("Installing dplyr package...")
        install.packages("dplyr")
    }
    library(dplyr)

  ## Download and extract data
    fileDownload()

  ## Read data from datasets
    path <- "./data/Human-Activity-Recognition/UCI HAR Dataset"
    activities <- readActivityLabels(file.path(path, "activity_labels.txt"))
    dfFeat <- readFiles(file.path(path, "features.txt"))
      #### Reading test data
    dfTest_sub <- readFiles(file.path(path, "test/subject_test.txt"))
    dfTest_act <- readFiles(file.path(path, "test/y_test.txt"))
    dfTest <- readMeasures(file.path(path, "test/X_test.txt"),
        subject = dfTest_sub[,1], activity = dfTest_act[,1])
      #### Reading training data
    dfTrain_sub <- readFiles(file.path(path, "train/subject_train.txt"))
    dfTrain_act <- readFiles(file.path(path, "train/y_train.txt"))
    dfTrain <- readMeasures(file.path(path, "train/X_train.txt"),
        subject = dfTrain_sub[,1], activity = dfTrain_act[,1])

  ## Identify column indices for mean() and std() variables
    index <- indexMean_STD(dfFeat)

  ## Filter out mean() and std() variables while merging the test and training data
    mergeDF <- mergeData(dfTest, dfTrain, index, activities)

  ## Make tidy data frame out of merged data
    tidyDF <- makeTidy(mergeDF)

  ## Write merged and tidy data frames into .txt files in the current working directory
    write.table(tidyDF, file = "data_tidy.txt", row.name = FALSE)
    write.table(mergeDF, file = "data_merged.txt", row.name = FALSE)
}

