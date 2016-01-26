library(dplyr)
library(stringr)

### STEP 1: LOAD DATA. -----------------------------------
message("Loading data...")

## Read training dataset. 
traindata <- read.table(file = "./UCI HAR Dataset/train/X_train.txt")
trainlabels <- read.table(file = "./UCI HAR Dataset/train/y_train.txt")
trainsubject <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt")

## Read test dataset. 
testdata <-read.table(file = "./UCI HAR Dataset/test/X_test.txt")
testlabels <- read.table(file = "./UCI HAR Dataset/test/y_test.txt")
testsubject <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt")

# Read feature (column) name data. 
features <- read.table(file = "./UCI HAR Dataset/features.txt", sep = " ")

## Read activity labels
activitylabels <- read.table(file = "./UCI HAR Dataset/activity_labels.txt")

## Use data tables to make it easier to work with datasets. 
traindata <- tbl_dt(traindata)
trainlabels <- tbl_dt(trainlabels)
trainsubject <- tbl_dt(trainsubject)
testdata <- tbl_dt(testdata)
testlabels <- tbl_dt(testlabels)
testsubject <- tbl_dt(testsubject)
features <- tbl_dt(features)
activitylabels <- tbl_dt(activitylabels)

message("Loading successful.")


### STEP 2: DATA TRANSFORMATION -------------------------------
message("Starting data transformation...")

## 2.A: Apply descriptive column names to datasets using data from features.txt.

# Coerce feature into characters in order to use them for column names. 
features$V2 <- as.character(features$V2) 
# Remove parentesis from variable names to increase readability. 
features <- mutate(features, V3 = str_replace(features$V2, "\\(\\)", "")) 
# Apply feature as column names to training and test datasets. 
colnames(testdata) <- features$V3
colnames(traindata) <- features$V3


# 2.B: Extract measurements on the mean and standard deviation for each measurement. 
test2 <- select(testdata, contains("mean"), contains("std"))
train2 <- select(traindata, contains("mean"), contains("std"))

# 2.C: Add activity and subject variables into datasets
test2 <- mutate(test2, activity = testlabels, subject = testsubject)
train2 <- mutate(train2, activity = trainlabels, subject = trainsubject) 

# 2.D: Merge the training and the test sets to create one data set.
completedata <- rbind_list(test2, train2)

# 2.E: Replace activity id's with descriptive activity names.
activity3 <- character(0)
activitylabels$V2 <- as.character(activitylabels$V2)

for(i in 1:length(completedata$activity)) {
      activity3 <- c(activity3, activitylabels$V2[completedata$activity[i]])
}
completedata$activity <- activity3

message("Transformation complete.")

# 2.F: Write data into a text file tidydata.txt in the working directory. 
message("Writing data to file tidydataset.txt in working directory.")
write.table(completedata, file = "tidydataset.txt", row.names = FALSE)
message("tidydataset.txt file successfully created.")


### STEP 3: From final dataset, create a second, independent tidy data set with the average of each variable for each activity and each subject.

message("Creating new dataset with average of each variable for each activity and each subject.")

# Group data by activity and subject. 
completedata <- group_by(completedata, activity, subject)

# Get the average of each variable for each activity and each subject. 
meanbyactivityandsubject <- summarise_each(completedata, funs(mean))

# Write data into text file tidydataset-means.txt in working directory.
message("Writing data to file tidydataset-means.txt in working directory.")
write.table(meanbyactivityandsubject, file = "tidydataset-means.txt", row.names = FALSE)      
message("tidydataset-means.txt file successfully created.")



