#Tidy downloaded datasets


run_analysis <- function(getData = FALSE){
  #Download data
  if ((!file.exists("dataset.zip")) | (getData == TRUE))
  {
      if(!'downloader' %in% rownames(installed.packages()))
      {
        install.packages("downloader")
      }
    library("downloader")
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download(url, dest="dataset.zip", mode="wb") 
    unzip ("dataset.zip")
  }
  #Read data
  X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
  Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
  Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  columnHeadings <- read.table("UCI HAR Dataset/features.txt")
  
  #simplfy and name data
  columnHeadings <- subset(columnHeadings, grepl("mean", V2) | grepl("std", V2))
  X_test <- subset(X_test, select = columnHeadings$V1)
  X_train <- subset(X_train, select = columnHeadings$V1)
  colnames(X_test) <- columnHeadings$V2
  colnames(X_train) <- columnHeadings$V2
  X_test$Activity <- factor(Y_test$V1, levels = c("1", "2", "3", "4", "5", "6"), labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS","SITTING", "STANDING", "LAYING")) 
  X_train$Activity <- factor(Y_train$V1, levels = c("1", "2", "3", "4", "5", "6"), labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS","SITTING", "STANDING", "LAYING")) 
  X_test$Subject <- factor(subject_test$V1, levels = 1:30)
  X_train$Subject <- factor(subject_train$V1, levels = 1:30)
  X_test$Type <- 'test'
  X_train$Type <- 'train'
  dataSet <- rbind(X_test, X_train)
  dataSet$Type <- factor(dataSet$Type, levels = c("test", "train"))
  
  #Create new dataset of means for each subject and activity.
  dataSet2 <- aggregate(dataSet[,1:79], by = list(Subject = dataSet$Subject, Activity = dataSet$Activity), FUN = mean)
  colnames(dataSet2)[!(colnames(dataSet2) %in% c("Activity", "Subject"))] <- paste("Overall_Mean", colnames(dataSet2)[!(colnames(dataSet2) %in% c("Activity", "Subject"))], sep = "_")

  #Save each dataset
  write.table(dataSet, file = "Q4DataSet.txt", row.name=FALSE)
  write.table(dataSet2, file = "Q5DataSet.txt", row.name=FALSE)
}