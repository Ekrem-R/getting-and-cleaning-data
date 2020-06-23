# Getting and Cleaning Data Project
library(data.table)
library(dplyr)

setwd("C:/Users/CityAdmin/Documents/project/UCI HAR Dataset")
#  The supporting metadata in this data are the name of the features and 
#  the name of the activities. 
#  They are loaded into variables featureNames and activityLabels.
featureNames <- read.table("~/project/UCI HAR Dataset/features.txt")
activityLabels <- read.table("~/project/UCI HAR Dataset/activity_labels.txt", header = FALSE)

# Reading Test Data
test_subject <- read.table("~/project/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
test_activity <- read.table("~/project/UCI HAR Dataset/test/y_test.txt", header = FALSE)
test_feautures <- read.table("~/project/UCI HAR Dataset/test/X_test.txt", header = FALSE)

# Reading Training Data
train_subject <- read.table("~/project/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
train_activity <- read.table("~/project/UCI HAR Dataset/train/y_train.txt", header = FALSE)
train_features <- read.table("~/project/UCI HAR Dataset/train/X_train.txt", header = FALSE)

#  1 : Merges the training and the test sets to create one data set
Subject <- rbind(train_subject, test_subject)
Activity <- rbind(train_activity, test_activity)
Features <- rbind(train_features, test_feautures)
# Naming the columns
colnames(Features) <- t(featureNames[2])
# Merging the data
colnames(Activity) <- "Activity"
colnames(Subject) <- "Subject"
allData <- cbind(Features,Activity,Subject)

#  2 : Extracts only the measurements on the mean and standard deviation 
# for each measurement.
measurements<- grep(".*Mean.*|.*Std.*", names(allData), ignore.case=TRUE)
#  Add activity and subject columns
newcolumns <- c(measurements, 562, 563)
dim(allData)
extractedData <- allData[,measurements]
dim(extractedData)

#  3 : Use descriptive activity names to name the activities in the data set
# The activity field in extractedData is originally of numeric type. We need to 
# change its type to character so that it can accept activity names.

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
# We need to factor the activity variable, once the activity names are updated.

#  4 : Appropriately labels the data set with descriptive variable names.
# Names of the variables in extractedData
names(extractedData)
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
# New names of the variables in extractedData
names(extractedData)

#  5 : From the data set in step 4, creates a second, independent tidy data set,
# with the average of each variable for each activity and each subject.

# set Subject as a factor variable.
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

# Creating tidyData as a data set with average for each activity and subject. 
# Then, we order the enties in tidyData and write it into 
# data file Tidy.txt that contains the processed data.
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)







