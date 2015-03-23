## Getting and cleaning data
## Course Project

## run_analysis.R

# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set. 
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the
#    average of each variable for each activity and each subject.

## Assumes that the working directory is or contains the "UCI HAR dataset" folder

run_analysis <- function() {
  
  # If we are not already in the dataset folder, set our working directory there
  if (file.exists("UCI HAR dataset")) {
    setwd("UCI HAR dataset/")
  }
  
  # Read the features files, this will allow us to extract the colnames we're interested in
  features <- read.table("features.txt", header = FALSE)
  colnames(features) <- c("n", "Name")
  
  # Read the activities file, this will allow us to use descriptive activity names in the data set
  activities <- read.table("activity_labels.txt", header = FALSE)
  colnames(activities) <- c("id", "Activity")
  
  # Read the data sets, both test and training
  testX <- read.table("test/X_test.txt")
  testY <- read.table("test/y_test.txt")
  testSubj <- read.table("test/subject_test.txt")
  
  trainX <- read.table("train/X_train.txt")
  trainY <- read.table("train/y_train.txt")
  trainSubj <- read.table("train/subject_train.txt")
  
  # Concatenate the datasets, one after the other, test first
  completeXTmp <- rbind(testX, trainX)
  completeYTmp <- rbind(testY, trainY)
  completeSubj <- rbind(testSubj, trainSubj)
  
  # Set descriptive column names
  colnames(completeXTmp) <- features[,"Name"]
  colnames(completeYTmp) <- c("id")
  colnames(completeSubj) <- c("SubjectID")
  
  # Extract only the columns with mean and std data
  # First find those columns, they have either mean() or std() in the name
  cols <- grep("(mean|std)\\(\\)", features[,"Name"])
  
  # Extract these columns
  completeX <- completeXTmp[,cols]
  
  # Use descriptive activity names, by joining to the activity names
  completeY <- merge(completeYTmp, activities, by = "id")["Activity"]
  
  # Now bind this data into a single data frame for further processing
  complete <- cbind(completeSubj, completeY, completeX)
  
  # We use the reshape tool to complete step 5
  library(reshape2)
  
  # first melt the data ..
  meltedData <- melt(complete, id = c("SubjectID", "Activity"), measured = colnames(completeX))
  
  # then cast it such that each row corresponds to a pair of subject and activity, with mean of
  # the measured variables in the other columns
  meansBySubjectActivity <- dcast(meltedData, SubjectID + Activity ~ variable, mean)
 
  meansBySubjectActivity
}

