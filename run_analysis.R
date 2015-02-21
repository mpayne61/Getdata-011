## 1 -  Download the Training and Test data sets
## Look at your current directory
## and set it to what you want it to be

getwd()
setwd("C:\\Users\\Michael\\Desktop\\Coursera\\Data_Cleaning\\project")
getwd()

path <- getwd()
path

if (!file.exists(path)) {dir.create(path)}

url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

ds <- "Dataset.zip"

require(downloader)
download(url, file.path(path, ds))

dateDownloaded <- date()
dateDownloaded


## create a vector file name we want to extract from

files.temp <- c("Dataset.zip")

## create a loop to extract the files to the directory set above
## will build the file name of the extracted file

unzip("Dataset.zip")
for (i in dir(pattern="Dataset.zip"))
unzip(i)

pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive = TRUE)

##Create data tables

require(data.table)

##Read the data files.

X_train <- read.table(file.path(pathIn, "train", "X_train.txt"))
X_test <- read.table(file.path(pathIn, "test", "X_test.txt"))

##Read the subject files.

Subject_train <- fread(file.path(pathIn, "train", "subject_train.txt"))
Subject_test <- fread(file.path(pathIn, "test", "subject_test.txt"))

##Read the activity files. 

Activity_train <- fread(file.path(pathIn, "train", "Y_train.txt"))
Activity_test <- fread(file.path(pathIn, "test", "Y_test.txt"))



##2 - Merge the training and the test sets to create one data set.
##Concatenate the tables and provide datalabels.

##Data Files
dt <- rbind(X_train, X_test)

##Subject files
dtSubject <- rbind(Subject_train, Subject_test)
setnames(dtSubject, "V1", "subject")

##Activity files
dtActivity <- rbind(Activity_train, Activity_test)
setnames(dtActivity, "V1", "activityNum")


##3 - Extracts only the measurements on the mean and standard deviation for each measurement. 

dtFeatures <- read.table(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

mean_sd_names <- subset(dtFeatures, grepl("mean|std", featureName))



##Select and subset these variables from main data table
require(dplyr)

dt1 <- select(dt,1:6,41:46,81:86,121:126,161:166,201,202,214,215,227,228,240,241,253,254,266:271,345:350,424:429,452:454,503,504,516,517,529,530,542,543)

##4 - Appropriately labels the data set with descriptive variable names. 

dt11 <- setnames(select(dt,1:6),c("tBodyAcc-mean()-X","tBodyAcc-mean()-Y","tBodyAcc-mean()-Z","tBodyAcc-std()-X","tBodyAcc-std()-Y","tBodyAcc-std()-Z"))
dt12 <- setnames(select(dt,41:46),c("tGravityAcc-mean()-X","tGravityAcc-mean()-Y","tGravityAcc-mean()-Z","tGravityAcc-std()-X","tGravityAcc-std()-Y"," tGravityAcc-std()-Z"))
dt13 <- setnames(select(dt,81:86),c("tBodyAccJerk-mean()-X","tBodyAccJerk-mean()-Y","tBodyAccJerk-mean()-Z","tBodyAccJerk-std()-X","tBodyAccJerk-std()-Y","tBodyAccJerk-std()-Z"))
dt14 <- setnames(select(dt,121:126),c("tBodyGyro-mean()-X","tBodyGyro-mean()-Y","tBodyGyro-mean()-Z","tBodyGyro-std()-X","tBodyGyro-std()-Y","tBodyGyro-std()-Z"))
dt15 <- setnames(select(dt,161:166),c("tBodyGyroJerk-mean()-X","tBodyGyroJerk-mean()-Y","tBodyGyroJerk-mean()-Z","tBodyGyroJerk-std()-X","tBodyGyroJerk-std()-Y","tBodyGyroJerk-std()-Z"))
dt16 <- setnames(select(dt,201:202),c("tBodyAccMag-mean()","tBodyAccMag-std()"))  
dt17 <- setnames(select(dt,214:215),c("tGravityAccMag-mean()","tGravityAccMag-std()"))
dt18 <- setnames(select(dt,227:228),c("tBodyAccJerkMag-mean()","tBodyAccJerkMag-std()"))
dt19 <- setnames(select(dt,240:241),c("tBodyGyroMag-mean()","tBodyGyroMag-std()"))
dt191 <- setnames(select(dt,253:254),c("tBodyGyroJerkMag-mean()","tBodyGyroJerkMag-std()"))
dt192 <- setnames(select(dt,266:271),c("fBodyAcc-mean()-X","fBodyAcc-mean()-Y","fBodyAcc-mean()-Z","fBodyAcc-std()-X","fBodyAcc-std()-Y","fBodyAcc-std()-Z"))
dt194 <- setnames(select(dt,345:350),c("fBodyAccJerk-mean()-X","fBodyAccJerk-mean()-Y","fBodyAccJerk-mean()-Z","fBodyAccJerk-std()-X","fBodyAccJerk-std()-Y","fBodyAccJerk-std()-Z")) 
dt196 <- setnames(select(dt,424:429),c("fBodyGyro-mean()-X","fBodyGyro-mean()-Y","fBodyGyro-mean()-Z","fBodyGyro-std()-X","fBodyGyro-std()-Y","fBodyGyro-meanFreq()-Z"))
dt197 <- setnames(select(dt,452:454),c("fBodyGyro-meanFreq()-X","fBodyGyro-meanFreq()-Y","fBodyGyro-meanFreq()-Z"))
dt198 <- setnames(select(dt,503:504),c("fBodyAccMag-mean()","fBodyAccMag-std()"))                 
dt199 <- setnames(select(dt,516:517),c("fBodyBodyAccJerkMag-mean()","fBodyBodyAccJerkMag-std()"))
dt1991 <- setnames(select(dt,529:530),c("fBodyBodyGyroMag-mean()","fBodyBodyGyroMag-std()"))
dt1992 <- setnames(select(dt,542:543),c("fBodyBodyGyroJerkMag-mean()","fBodyBodyGyroJerkMag-std()"))
  
  
##Merge coloumns

dt2 <- cbind(dtSubject,dtActivity,dt11,dt12,dt13,dt14,dt15,dt16,dt17,dt18,dt19,dt191,dt192,dt194,dt196,dt197,dt198,dt199,dt1991,dt1992)
##clean up reduntant tables 
rm(dt11,dt12,dt13,dt14,dt15,dt16,dt17,dt18,dt19,dt191,dt192,dt194,dt196,dt197,dt198,dt199,dt1991,dt1992)

##5 - Uses descriptive activity names to name the activities in the data set
##Use descriptive activity names
##Read activity_labels.txt file. 

dtActivityNames <- read.table(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

require(plyr)

dt3 <- list(dtActivityNames,dt2)
dt4 <- arrange(data.table(join_all(dt3)),subject,activityNum)

##remove activity number and rearrange data set 
dt5 <- subset(dt4, select=c(1,3,4:71))
dt5 <- arrange(dt5,subject,activityNum)
## Summarise table

dt6 <- aggregate(. ~ subject+activityNum, data = dt5, mean)

##Add activity description
dt7 <- data.table(join_all(list(dtActivityNames,dt6)))

## 6 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

dt8 <- arrange(subset(dt7,select=c(3,2,4:71)),subject,activityName)





