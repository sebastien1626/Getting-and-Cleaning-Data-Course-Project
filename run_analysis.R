#libraries
library(dplyr)

#Download zip file
filename <- "projectfiles_FUCI_HAR_Dataset.zip"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile = filename,method = "curl")

#extract zip file
if(file.exists(filename)){unzip(filename)}

#create data frames
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activities"))
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("id", "functions"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


## Merges the training and the test sets to create one data set

#combines x_test and x_train as rows
x <-rbind(x_train, x_test)
#combines y_test and y_train as rows
y <-rbind(y_train, y_test)
#combines y_test and y_train as rows
subject <-rbind(subject_test,subject_train)

#combines the 3 objects as column
data <- cbind(subject, y, x)

## Extracts only the measurements on the mean and standard deviation for each measurement. 

tidy_data1 <- data %>% select(subject, code, contains("mean"), contains("std"))

## Uses descriptive activity names to name the activities in the data set. 

tidy_data1$code <- activity_labels[tidy_data1$code, 2]

## Appropriately labels the data set with descriptive variable names

names(tidy_data1)[match("code",names(tidy_data1))] <- "Activity"
names(tidy_data1)[match("subject",names(tidy_data1))] <- "Subject"
names(tidy_data1)<-gsub("Acc", "Accelerometer", names(tidy_data1))
names(tidy_data1)<-gsub("Gyro", "Gyroscope", names(tidy_data1))
names(tidy_data1)<-gsub("Mag", "Magnitude", names(tidy_data1))
names(tidy_data1)<-gsub("mean()", "Mean", names(tidy_data1))
names(tidy_data1)<-gsub("std()", "STD", names(tidy_data1))
names(tidy_data1)<-gsub("freq()", "Frequency", names(tidy_data1))
names(tidy_data1)<-gsub("tBody", "TimeBody", names(tidy_data1))
names(tidy_data1)<-gsub("angle", "Angle", names(tidy_data1))
names(tidy_data1)<-gsub("gravity", "Gravity", names(tidy_data1))
names(tidy_data1)<-gsub("^t", "Time", names(tidy_data1))
names(tidy_data1)<-gsub("^f", "Frequency", names(tidy_data1))

#result
names(tidy_data1)

## Creates a second independent tidy data set with the average of each variable for each activity and each subject.
tidy_data2 <- tidy_data1 %>% group_by(Activity,Subject) %>% summarise_all(funs(mean))
str(tidy_data2)

## write the data to a file
write.table(tidy_data2, file="tidy.txt", row.names = FALSE, quote = FALSE)

