library(dplyr)
## Call our package dplyr

##A) Getting Data 
filename <- "C3_Assignment.zip"
## give a name to the file we are going to download

if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

## the code above check whether the file name exist, if not we can have them downloaded in 
## the given name.

if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

## the code above check whether a folder with name "UCI HAR Dataset"
## exist, if not, we can create them and unzip the downloaded data into it
## Now we get the data:
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
## Generally, this code called out 561 rows, 2 columns from feature.txt 
## The features selected for this database come from the accelerometer and gyroscope 
## 3-axial raw signals tAcc-XYZ and tGyro-XYZ.

activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
## 6 rows, 2 columns of activity_labels.txt is now called out and named activities
## List of activities performed when the corresponding measurements were taken and its codes (labels)

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
## subject_test contatin 2947 rows, 1 column 
## it contains test data of 9/30 volunteer test subjects being observed

x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
## recorded features test data contains  2947 rows, 561 columns 

y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
## test data of activities’code labels contains  2947 rows, 1 columns 

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

## train data of 21/30 volunteer subjects being observed includes 7352 rows, 1 column 

x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)

## 7352 rows, 561 columns of recorded features train data

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
## train data of activities’code labels has 7352 rows, 1 columns 


## Task1: Merge the data
X <- rbind(x_train, x_test)
##X (10299 rows, 561 columns) is created by merging x_train and x_test using rbind() function
Y <- rbind(y_train, y_test)
##Y (10299 rows, 1 column) is created by merging y_train and y_test using rbind() function
Subject <- rbind(subject_train, subject_test)

##Subject (10299 rows, 1 column) is created by merging subject_train and subject_test using rbind() function
Merged_Data <- cbind(Subject, Y, X)
##Merged_Data (10299 rows, 563 column) is created by merging Subject, Y and X using cbind() function

## Task 2: Extracts only the measurements on the mean and standard deviation for each measurement.

TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
## TidyData (10299 rows, 88 columns) is created by subsetting Merged_Data, selecting only columns: subject, code and the measurements 
##on the mean and standard deviation (std) for each measurement

## Task 3: Uses descriptive activity names to name the activities in the data set

TidyData$code <- activities[TidyData$code, 2]
##Entire numbers in code column of the TidyData replaced with corresponding activity taken 
##from second column of the  activities variable

## Task4: Appropriately labels the data set with descriptive variable names


names(TidyData)[2] = "activity"

## code column in TidyData renamed into activities
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
## All Acc in column’s name replaced by Accelerometer
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
##All Gyro in column’s name replaced by Gyroscope
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
##All BodyBody in column’s name replaced by Body
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
##All Mag in column’s name replaced by Magnitude
names(TidyData)<-gsub("^t", "Time", names(TidyData))
## All start with character t in column’s name replaced by Time
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
## All start with character f in column’s name replaced by Frequency
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
## All tBody in column’s name replaced by TimeBody
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
## All not mean in column’s name replaced by Mean
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
## All not std in column’s name replaced by STD
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
## All not frequent in column’s name replaced by Frequency
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
## All angle in column’s name replaced by Angle
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))
## All gravitt in column’s name replaced by Gravity

##Task 5: From the data set in step 4, creates a second, 
##independent tidy data set with the average of each variable for each activity and each subject

FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

##FinalData (180 rows, 88 columns) is created by sumarizing TidyData taking the means of each variable for each
##activity and each subject, after groupped by subject and activity.

View(FinalData)
## Test the final Data
str(FinalData)
##Checking the variable name 

write.table(FinalData, "FinalData.txt", row.name=FALSE)
##Export FinalData into FinalData.txt file.