## Assginment 3.




## First at all we load the library needed for the means
library(plyr)

## Load the datasets
testData <- read.table("./test/X_test.txt")
trainData<- read.table("./train/X_train.txt")

## We get the names of the variables from the file
newNames <- readLines("./features.txt")

## We are to extract and set the columns names
getSecond <- function(x){x[2]}
allNames <- sapply(strsplit(newNames, " "), getSecond)
names(testData) <- allNames
names(trainData) <- allNames

## We read the labels for the activity
y_test <- as.numeric (readLines("./test/y_test.txt"))
y_train <- as.numeric (readLines("./train/y_train.txt"))

## As before we extract the new names
activityLabels <- readLines("./activity_labels.txt")
activityLabels <- sapply(strsplit(activityLabels, " "), getSecond)

## And assing them
changeName <- function(x, names){names[x]}
testLabels <- sapply (y_test, changeName, names=activityLabels)
trainLabels <- sapply (y_train, changeName, names=activityLabels)

## We create new columns for the activity
testData$activity <-testLabels
trainData$activity <-trainLabels


## Now we read data for the subject
subjectTestData <- readLines("./test/subject_test.txt")
subjectTrainData <- readLines("./train/subject_train.txt")

## We create a new column for the Subject
testData$subject <-subjectTestData
trainData$subject<- subjectTrainData

#We create a vector with of the names for our tidyDataset
allNames <- c(allNames, "activity", "subject")

##We are just interested on mean and std values
tidyTest <- testData[grep ("mean\\(\\)|Mean|std\\(\\)|activity|subject", allNames)]
tidyTrain <- trainData[grep ("mean\\(\\)|Mean|std\\(\\)|activity|subject", allNames)]

tidyTest$activity =factor(tidyTest$activity, labels=activityLabels)
tidyTrain$activity =factor(tidyTrain$activity, labels=activityLabels)
tidyTest$subject =factor(tidyTest$subject)
tidyTrain$subject =factor(tidyTrain$subject)

##We put data together
tidyData<- rbind (tidyTrain, tidyTest)

##We made the mean for the variables
means <- ddply(tidyData, .(activity, subject), numcolwise(mean))

##We save data into a txt file
write.table(means, file="./result.txt", sep = "\t", append=F)
