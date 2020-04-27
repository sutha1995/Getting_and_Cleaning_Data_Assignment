#This is a peer graded assignment of Getting and Claening Data

#Step 1: Download the zip file

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL,destfile="assignment.zip")

#Step 2: Unzip and extract the files

unzip(zipfile="assignment.zip", exdir="data")
#there is a UCI HAR Dataset folder with two more folders containing the data, test and train folder

#step 3: Assign all data frames

features <- read.table("data/UCI HAR Dataset/features.txt")
activities <- read.table("data/UCI HAR Dataset/activity_labels.txt")
subject_test <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("data/UCI HAR Dataset/test/y_test.txt")
subject_train <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("data/UCI HAR Dataset/train/y_train.txt")

#Now upon viewing each data. There seems to be confusion on the names of the headers. Lets fix that by assigning them with some column names.

#Step 4: Assigning Column names for the datas

colnames(features) <- c("n","functions")
colnames(activities) <- c("code", "activity")
colnames(subject_test) <- c("subject")
colnames(x_test) <- features$functions #all names of the headers in the features files will replaced in the headers
colnames(y_test) <- c("code")
colnames(subject_train) <- c("subject")
colnames(x_train) <- features$functions #all names of the headers in the features files will replaced in the headers
colnames(y_train) <- c("code")

#Step 5: Let's merge the training and the test sets to create one data set.
#load the dplyr package

library("dplyr")

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged <- cbind(Subject, Y, X)

#Step 6: Extracts only the measurements on the mean and standard deviation for each measurement.

Merged %>% select(subject, code, contains("mean"), contains("std"))

#Step 7: Uses descriptive activity names to name the activities in the data set

Merged$code = factor(Merged$code, labels = as.character(activities[,2]))
names(Merged)[names(Merged) == "code"] <- "activity" #renaming the "code" column into "activity"

names(Merged)<-gsub("Acc", "Accelerometer", names(Merged))
names(Merged)<-gsub("Gyro", "Gyroscope", names(Merged))
names(Merged)<-gsub("BodyBody", "Body", names(Merged))
names(Merged)<-gsub("Mag", "Magnitude", names(Merged))
names(Merged)<-gsub("^t", "Time", names(Merged))
names(Merged)<-gsub("^f", "Frequency", names(Merged))
names(Merged)<-gsub("tBody", "TimeBody", names(Merged))
names(Merged)<-gsub("-mean()", "Mean", names(Merged), ignore.case = TRUE)
names(Merged)<-gsub("-std()", "STD", names(Merged), ignore.case = TRUE)
names(Merged)<-gsub("-freq()", "Frequency", names(Merged), ignore.case = TRUE)
names(Merged)<-gsub("angle", "Angle", names(Merged))
names(Merged)<-gsub("gravity", "Gravity", names(Merged))
        
#Step 8: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Merged %>% group_by(subject, activity) %>% summarise_each(funs(mean))
str(Merged)
write.table(Merged, "TidyDataset.txt", row.name=FALSE, col.name=TRUE)
