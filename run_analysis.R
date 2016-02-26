# This is the run_analysis assignment wrok 
# This function downloads data and extracts the downloaded zip
download <- function () {
  print("Downloading data...")
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, destfile = "dataset.zip")
  unzip("dataset.zip")
}

# This is the Main run_analysis function. Gets the data set for both train and test, merges, extract tidy data and lebels the data set 
run.analysis <- function () {
  
  #set working directory to the dataset
  setwd('./UCI HAR Dataset/')
  
  # Get the features
  feature.set <- read.table('./features.txt', col.names = c('index', 'name'))
  features <- subset(feature.set, grepl('-(mean|std)[(]', feature.set$name))
  
  # Get the labels
  label <- read.table('./activity_labels.txt', col.names = c('level', 'label'))
  
  # Read train and test data sets
  traindataset <- loadData('train', features, label)
  testdataset <- loadData('test', features, label)
  
  # Merge data set train and test
  print("Combining data sets...")
  dataset <- rbind(traindataset, testdataset)
  
  # Generate the tidy data set from merged dataset
  print("Generating tidy datasets...")
  tidydataset <- dataset[, lapply(.SD, mean), by=list(label, subject)]
  
  # Replaces the untidy names to tidy varialble names as supplied in the arguments
  names <- names(tidydataset)
  names <- refactorSubject('-mean','Mean',names)
  names <- refactorSubject('-std', 'Std', names) 
  names <- refactorSubject('[()-]', '', names)
  names <- refactorSubject('BodyBody', 'Body', names) 
  
  setnames(tidydataset, names)
  
  # set working directory to earlier working directory location i.e parent directory to features file And write the result data on it
  print("Writing data to the current working directory...")
  setwd('..')
  write.csv(dataset, file = 'rawdataset.csv', row.names = FALSE)
  write.csv(tidydataset, file = 'tidydataset.csv',
            row.names = FALSE, quote = FALSE)
  
  # this will return and print the data on R console after calling run_analysis() function
  tidydataset
  
}

refactorSubject <- function(currval, toVal, names) {
  names <- gsub(currval,toVal,names)
  
}


# Load data from the current directory and prepares data table
loadData <- function (dataname, features, labels) {
  
  dataname <- trimws(dataname)
  print(paste("Loading Data Set..",dataname))
  
  # Constructing the file path
  prefix <- paste(dataname, '/', sep = '')
  filedata <- paste(prefix, 'X_', dataname, '.txt', sep = '')
  flabel <- paste(prefix, 'y_', dataname, '.txt', sep = '')
  subject <- paste(prefix, 'subject_', dataname, '.txt', sep = '')
  
  # Reading data to data frame and to data table
  data <- read.table(filedata)[, features$index]
  names(data) <- features$name
  
  label.set <- read.table(flabel)[, 1]
  data$label <- factor(label.set, levels=labels$level, labels=labels$label)
  
  subjectset <- read.table(subject)[, 1]
  data$subject <- factor(subjectset)
  
  # convert to data table
  data.table(data)
  
}
