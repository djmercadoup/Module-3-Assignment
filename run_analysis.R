## Peer-graded Assignment: Getting and Cleaning Data Course Project
## Submitted by: Danilo J. Mercado (djmercado@up.edu.ph)

## The purpose of this project is to demonstrate your ability to collect, work with, 
## and clean a data set.

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the 
##    average of each variable for each activity and each subject.

## Load Required Libraries
library(data.table)
library(dplyr)

## Set working directory
setwd("/Users/DJM/Documents/Data Science")

## Read in the datasets
## Assumes zipfiles were downloaded, unzipped and copied under working directory

activityLabels <- fread("./UCI HAR Dataset/activity_labels.txt",
                        col.names = c("classLabels", "activityName"))
features <- fread("./UCI HAR Dataset/features.txt",
                  col.names = c("featureId", "featureNames"))
train <- fread("./UCI HAR Dataset/train/X_train.txt")
trainSubject <- fread("./UCI HAR Dataset/train/subject_train.txt")
trainActivity <- fread("./UCI HAR Dataset/train/y_train.txt")
test <- fread("./UCI HAR Dataset/test/X_test.txt")
testSubject <- fread("./UCI HAR Dataset/test/subject_test.txt")
testActivity <- fread("./UCI HAR Dataset/test/y_test.txt")

## 1. Merge the train and test datasets
mergedt <- rbind(train, test)
subjectdt <- rbind(trainSubject, testSubject)
activitydt <- rbind(trainActivity, testActivity)
  
## 2. Extract only variables of mean or std measurements 
featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames])
mergedt <- select(mergedt, featuresWanted)

## 3. Use descriptive activity names for the activities in the data set
activitydt <- left_join(activitydt, activityLabels, by = c("V1" = "classLabels"))

## 4. Appropriately label the data sets with descriptive variable names then combine into
##    one data set
measurements <- features[featuresWanted, featureNames]
measurements <- gsub('[()]', '', measurements)
setnames(mergedt, colnames(mergedt), measurements)
setnames(subjectdt, "V1", "subjectId")
setnames(activitydt, "V1", "activityClass")

combinedt <- cbind(mergedt, subjectdt, activitydt)
View(combinedt)

## 5. Create an independent tidy data set with the 
##    average of each variable for each activity and each subject.

meandt <- combinedt %>% 
          group_by(subjectId, activityClass) %>%
          summarize_at(vars(1:66), mean)
View(meandt)
