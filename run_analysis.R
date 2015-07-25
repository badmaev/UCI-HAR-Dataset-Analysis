#pre-requisite packages
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(dplyr)
if("reshape2" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape2")};library(reshape2)
# downloading and unzipping the file
fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists(DataDirectory)) {
     download.file(fileURL, destfile="file.zip", mode="wb")
     unzip("file.zip")
     file.remove("file.zip")
}
#---------------------------------------------------------------------------------------------------#
# Let's capture list values of "Activities" that will become one column "Activity" in the resulting dataset
tmp <- read.table("UCI HAR Dataset/activity_labels.txt", sep = "")
activityLabels <- as.factor(tmp$V2)
# Let's capture "Features" a.k.a measurements that will become 561 column names in the resulting dataset
tmp <- read.table("UCI HAR Dataset/features.txt", sep = "")
columnNames <- tmp$V2
#---------------------------------------------------------------------------------------------------#
# Let's read and construct the Training dataset

x_train <- read.table("UCI HAR Dataset/train/X_train.txt", sep = "")
names(x_train) <- columnNames
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", sep = "", col.names=c("activity"))
y_train$activity <- as.factor(y_train$activity)
levels(y_train$activity)<- activityLabels
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt", sep = "", col.names = c("subject"))
trainSubjects$subject <- as.factor(trainSubjects$subject)

# "Set" will represent a new column that will indicate if this observation came from Test or Training dataset
set<-rep("training", length(trainSubjects$subject))
set<-as.factor(set)
levels(set)<-c("training","test")
train <- cbind(x_train, trainSubjects, y_train, set)
rm(set)
#---------------------------------------------------------------------------------------------------#
# Let's read and construct the Test dataset
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", sep = "")
names(x_test) <- columnNames
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", sep = "",col.names=c("activity"))
y_test$activity <- as.factor(y_test$activity)
levels(y_test$activity)<-activityLabels
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt", sep = "", col.names = c("subject"))
testSubjects$subject <- as.factor(testSubjects$subject)
set<-rep("test", length(testSubjects$subject))
set<-as.factor(set)
levels(set)<-c("training","test")
test <- cbind(x_test, testSubjects, y_test, set)
#-------------------------------------------------------------------------#
#Let's merge two datasets together to create one data set

data<-rbind(train, test)
rm(set,train, test, tmp, x_train, x_test, y_train, y_test, trainSubjects, testSubjects, 
activityLabels, columnNames)
#-------------------------------------------------------------------------#
#Let's extract only the measurements on the mean and standard deviation for each measurement
data<-data[,grep("mean|std|subject|activity|set",names(data))]
data<-data[,-(grep("meanFreq",names(data)))]
#-------------------------------------------------------------------------#
#Let's appropriately label the data set with descriptive variable names
filtered_names <- names(data)
filtered_names <- gsub("\\(\\)", "", filtered_names)
filtered_names <- gsub("Acc", "-acceleration", filtered_names)
filtered_names <- gsub("^t(.*)$", "\\1-time", filtered_names)
filtered_names <- gsub("^f(.*)$", "\\1-frequency", filtered_names)
filtered_names <- gsub("(Jerk|Gyro)", "-\\1", filtered_names)
filtered_names <- gsub("BodyBody", "Body", filtered_names)
filtered_names <- gsub("mag", "-magnitude", filtered_names)
filtered_names <- tolower(filtered_names)
# assign names to column names
names(data) <- filtered_names
#-------------------------------------------------------------------------#
# Let's create a second, independent tidy data set with the average of each variable for each activity and each subject
melt_data<-melt(data, id=c("subject", "activity", "set") )
tidy_data<-dcast(melt_data, subject+activity~variable, mean)
# saving the resulting tidy data set into tydy_data.txt file
write.table(tidy_data, file = "./tidy_data.txt", row.names = FALSE)

