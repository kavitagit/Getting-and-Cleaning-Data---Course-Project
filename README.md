Getting-and-Cleaning-Data---Course-Project
==========================================

Coursera - Data Science Toolbox 2014

## Coursera Getting and Cleaning Data Course Project - June 2014

# 1. Merge the training and the test sets to create one data set.
#set working directory - where unzpped UCI HAR Dataset was stored
setwd("/Users/Kavita/Documents/Coursera - Data Science/Getting and Cleaning Data/Course Project/UCI HAR Dataset/")

# Read the data from files
# Read common files
features = read.table('./features.txt',header=FALSE)
activityType = read.table('./activity_labels.txt',header=FALSE)

#Read "train" files
subjectTrain = read.table('./train/subject_train.txt',header=FALSE)
xTrain = read.table('./train/x_train.txt',header=FALSE)
yTrain = read.table('./train/y_train.txt',header=FALSE)

#Read "test" files
subjectTest = read.table('./test/subject_test.txt',header=FALSE)
xTest = read.table('./test/x_test.txt',header=FALSE)
yTest = read.table('./test/y_test.txt',header=FALSE)

#Assign Column names to above files
colnames(features) = c('featureID','featureType')
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)= c('subjectId')
colnames(xTrain)= features[,2]
colnames(yTrain)= c('activityId')
colnames(subjectTest) = c('subjectId')
colnames(xTest) = features[,2] 
colnames(yTest) = c('activityId')

#Create Final training set - merging subjectTrain, xTrain and yTrain
trainData = cbind(cbind(xTrain,subjectTrain), yTrain)

#Create Final test set - merging subjectTest, xTest and yTest
testData = cbind(cbind(xTest,subjectTest),yTest)

# Combine Final data set - merging training and test data
finalData = rbind(trainData,testData)

# Label columns
finalDatalabels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2];
names(finalData) <- finalDatalabels

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
finalDataMeanStd <- finalData[,grepl("mean|std|Subject|ActivityId", names(finalData))]

# 3. Uses descriptive activity names to name the activities in the data set

#use the activity names to name the activities in the set
#first create the activity column for the entire dataset - test and train:
#assign a column name so we can merge on it
#join the activityLabels - we use join from the plyr package and not merge, because join preserves order
#add the column to the entire dataset

activityLabels <- read.table('./activity_labels.txt',stringsAsFactors=FALSE);
colnames(activityLabels) <- c("activityID","activityLabel");
allActivities <- rbind(yTest,yTrain);
colnames(allActivities)[1] <- "activityID";
activities <- join(allActivities,activityLabels,by="activityID");
finalDataMeanStd <- cbind(activity=activities[,"activityLabel"],finalDataMeanStd)

# 4. Appropriately labels the data set with descriptive variable names. 
colNames  = colnames(finalDataMeanStd)
for (i in 1:length(colNames)) {
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("BodyBody","Body",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("Bodyaccjerkmag","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])}
colnames(finalDataMeanStd) = colNames
# Cleaning up the variable names
# Reassigning the new descriptive column names to the finalData set

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidyData = ddply(finalDataMeanStd, c("Subject","ActivityId"), numcolwise(mean))
write.table(tidyData, file = "tidyData.txt")

