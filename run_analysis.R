##################################################################################################################
# Peer-graded Assignment: Getting and Cleaning Data 
##################################################################################################################
# The assignement is as follow: 
# Create one R script called run_analysis.R that does the following.
# 1.	Merges the training and the test sets to create one data set.
# 2.	Extracts only the measurements on the mean and standard deviation for each measurement.
# 3.	Uses descriptive activity names to name the activities in the data set
# 4.	Appropriately labels the data set with descriptive variable names.
# 5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##################################################################################################################
# 0. Initialise R and Retrieve the Data:
##################################################################################################################

# 0.0 Download and unzip the file from the adress:
#         https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 0.1 In RStudio, set working directory where you have downloaded the data on you computer

setwd("~/Documents/DataSc/Cleaning_Data/UCI HAR Dataset");

# 0.2 Import the input tables:

features <- read.table('./features.txt', header = FALSE);
activityType <- read.table('./activity_labels.txt',header=FALSE);
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE);
xTrain  <- read.table('./train/x_train.txt',header=FALSE);
yTrain <- read.table('./train/y_train.txt',header=FALSE);

##################################################################################################################
# 1. Merges the training and the test sets to create one data set.
##################################################################################################################
# 1.1. Rename the column of the input tables to give more insightfull names:

colnames(features) = c('featuresId', 'featuresType'); # just change the column names of features to make it more handy
colnames(activityType) = c('activityId','activityType'); # just change the column names of activityType to make it more handy
colnames(xTrain) = features[,2]; # Rename the column of xTrain by the varibales available in the second column of features
colnames(subjectTrain) = "subjectId"; # just change the column name of subjectTrain to make it more handy
colnames(yTrain) = "activityId"; # just change the column name of yTrain to make it more handy

# 1.2. Create on Data set with all information from the Training files: merge => yTrain, subjectTrain and xTrain:

trainingData = cbind(yTrain,subjectTrain,xTrain);

# 1.3. Read the data from the tests files:

subjectTest = read.table('./test/subject_test.txt',header=FALSE);
xTest = read.table('./test/x_test.txt',header=FALSE);
yTest = read.table('./test/y_test.txt',header=FALSE);

# 1.4. Rename the column of the Test files to give more insightfull names:

colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

# 1.5. Create one file with all test info

testData = cbind(yTest,subjectTest,xTest);

# 1.6. Create one complete file with train and test data:

finalData = rbind(trainingData,testData);

##################################################################################################################
# 2.	Extracts only the measurements on the mean and standard deviation for each measurement
##################################################################################################################

# 2.1 extract the names of the columns so that we will be able to select the std and mean or std info in it

colNames  = colnames(finalData); 

# 2.2. Create a vector "Selection" that will take TRUE if the coliumn names as mean

Selection = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

#2.3. Only keep the column in finaldata for which vector Selction is TRUE
finalData = finalData[logicalVector==TRUE];

##################################################################################################################
# 3.	Uses descriptive activity names to name the activities in the data set
##################################################################################################################

# 3.1. Use the information from activityType table to name the activities in finalData

finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# 3.2. put all the column names in a vector colNames 

colNames  = colnames(finalData); 

##################################################################################################################
# 4. Appropriately labels the data set with descriptive variable names.
##################################################################################################################
# 4.1. we will adjust the names of the columns we have now stored in colNames

for (i in 1:length(colNames))  {
     colNames[i] = gsub("\\()","",colNames[i])                                      # remove all the // and ()
     colNames[i] = gsub("-std$","StdDev",colNames[i])                               # replace -std by StdDev
     colNames[i] = gsub("-mean","Mean",colNames[i])                                 # replace -mean by Mean
     colNames[i] = gsub("^(t)","time",colNames[i])                                  # replace t by Time
     colNames[i] = gsub("^(f)","freq",colNames[i])                                  # replace f by freq
     colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])                       # replace gravity by Gravity
     colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])              # replace body by Body
     colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])                               # replace gyro by Gyro
     colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])                        # replace AccMac by AccMagnitude
     colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])   # replace bodyaccjermag by BodyAccJerkMagnitude
     colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])                      # replace JerkMag by JerkMagnitude
     colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])                      # replace GyroMag by Gyro Magnitude
 }
 
 # 4.2 replace the column names of the table finalData by the names in the vector colNames
 
 colnames(finalData) = colNames;
 
##################################################################################################################
# 5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##################################################################################################################
  
# 5.1. To make the mean of each columns, we have first to remove the last column "activityType" which is filled with characters
  
FinalData2  = finalData[,names(finalData) != 'activityType'];
  
#5.2. Keep onlmy the mean info in FinalOutput 

FinalOutput = aggregate(finalData2[,names(finalData2) != c('activityId','subjectId')],by=list(activityId=finalData2$activityId,subjectId = finalData2$subjectId),mean);
  
# 5.3. Re-introduce in teh FinalOutput file the name of the activities 

FinalOutput = merge(FinalOutput,activityType,by='activityId',all.x=TRUE);
 
# 5.4. Place this final table in the working directory

write.table(FinalOutput, './FinalOutput.txt',row.names=TRUE,sep='\t');
  
  
  
  
  
 
