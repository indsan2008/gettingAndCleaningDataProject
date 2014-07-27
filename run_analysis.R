
##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## 27-07-2014
#Author: Sandeep Indraganti

# run_analysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################

# 1. Merge the training and the test sets to create one data set.
{
  #Read the data which is common for both training and testing
  features     = read.table("./UCI HAR Dataset/features.txt",header=FALSE); #imports features.txt
  activityType = read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE); #imports activity_labels.txt
  
  ##Read the files related to training data.
  subjectTrain = read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE); #imports subject_train.txt
  xTrain       = read.table("./UCI HAR Dataset/train/x_train.txt",header=FALSE); #imports x_train.txt
  yTrain       = read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE); #imports y_train.txt
  
  # Assigin column names to the training data imported above
  colnames(activityType)  = c('activityId','activityType');
  colnames(subjectTrain)  = "subjectId";
  colnames(xTrain)        = features[,2]; 
  colnames(yTrain)        = "activityId";
  
  #Create a final training dataset
  trainingDataSet<-cbind(yTrain,subjectTrain,xTrain);
  
  #Read the files related to test data.
  subjectTest = read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE); #imports subject_test.txt
  xTest       = read.table("./UCI HAR Dataset/test/x_test.txt",header=FALSE); #imports x_test.txt
  yTest       = read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE); #imports y_test.txt
  
  # Assign column names to the test data imported above
  colnames(subjectTest) = "subjectId";
  colnames(xTest)       = features[,2]; 
  colnames(yTest)       = "activityId";
  
  # Create the final test set by merging the xTest, yTest and subjectTest data
  testingDataSet = cbind(yTest,subjectTest,xTest);
  # Combine training and test data to create a final data set
  finalDataSet = rbind(trainingDataSet,testingDataSet);
  
} ##Step1 Completed



# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
{
  
  # Create a vector for the column names from the finalDataSet, which will be used
  # to extract the desired mean() & std() columns
  colNames  = colnames(finalDataSet); 
  
  # Create a logicalVector that contains TRUE values for the ID, mean() & std() columns and FALSE for others
  logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
  
  # Modify the finalDataSet using the above logical vector 
  extractedFinalDataSet = finalDataSet[logicalVector==TRUE];
  
}#step2 complted


# 3. Use descriptive activity names to name the activities in the data set
{  
  # Merge the finalData set with the acitivityType table to include descriptive activity names
  extractedFinalDataSet= merge(extractedFinalDataSet,activityType,by='activityId',all.x=TRUE);
  
  # Updating the colNames vector to include the new column names after merge
  colNames  = colnames(extractedFinalDataSet); 
  
}#step3 completed


##4.Appropriately label the data set with descriptive activity names. 
{
  # Cleaning up the variable names
  for (i in 1:length(colNames)) 
  {
    colNames[i] = gsub("\\()","",colNames[i])
    colNames[i] = gsub("-std$","StandardDeviation",colNames[i])
    colNames[i] = gsub("-mean","Mean",colNames[i])
    colNames[i] = gsub("^(t)","time",colNames[i])
    colNames[i] = gsub("^(f)","frequency",colNames[i])
    colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
  };
  
  # Reassigning the new descriptive column names to the finalData set
  colnames(extractedFinalDataSet) = colNames;
}#Step4 completed


# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
{
  # Create a new table, finalDataNoActivityType without the activityType column
  finalDataNoActivityType  = extractedFinalDataSet[,names(extractedFinalDataSet) != 'activityType'];
  
  # Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
  tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);
  
  # Merging the tidyData with activityType to include descriptive acitvity names
  tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE); 
  # Export the tidyData set 
  write.table(tidyData, './gettingAndcleaningData_tidydata.txt',row.names=TRUE,sep='\t');
}




