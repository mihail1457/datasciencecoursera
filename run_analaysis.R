# Download Data
library(data.table)
fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists('./UCI HAR Dataset.zip')){
  download.file(fileurl,'./UCI HAR Dataset.zip', mode = 'wb')
  unzip("UCI HAR Dataset.zip")
}

# Read All Data
features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,"V2"])

x_train <- read.table('./UCI HAR Dataset/train/X_train.txt', header=F)
y_train <-  read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
subject_train <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

trainFrame <- data.frame(subject_train, x_train, y_train)
colnames(trainFrame) <- c("subject", "activity", features)

x_test <- read.table('./UCI HAR Dataset/train/X_train.txt', header=F)
y_test <-  read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
subject_test <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

testFrame <- data.frame(subject_test, x_test, y_test)
colnames(testFrame) <- c("subject", "activity", features)

# Add Together; Subtract Duplicate Columns;
addFrames <- rbind(trainFrame, testFrame)
idx = which(duplicated(names(addFrames)))
addFrames = addFrames[,-idx]

# Select only columns containing mean and standard deviation
extractedFrame <- select(addFrames, contains("subject"), contains("activity"), contains("mean()"), contains("std"), contains("meanFreq()"))

# Descriptive Activities
desc_act<- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
desc_act <- as.character(desc_act[,"V2"])

# Appropriate labels for the column names
copy_names <- colnames(extractedFrame)
copy_names <- gsub("[(][)]", "", copy_names)
copy_names <- gsub("-mean", "_Mean_", copy_names)
copy_names <- gsub("-std", "_Standard_Deviation_", copy_names)
copy_names <- gsub("^f", "Frequency_", copy_names)
copy_names <- gsub("^t", "Time_", copy_names)
copy_names <- gsub("Acc", "Accelerometer", copy_names)
copy_names <- gsub("Gyro", "Gyroscope", copy_names)
copy_names <- gsub("Mag", "Magnitude", copy_names)
copy_names <- gsub("-", "_", copy_names)
copy_names <- gsub("__", "_", copy_names)
colnames(extractedFrame) <- copy_names

tidy_data <- extractedFrame %>% group_by(activity, subject) %>% dplyr::summarise_all(funs(mean))
write.table(tidy_data, "tidy_data.txt", row.name=FALSE)
