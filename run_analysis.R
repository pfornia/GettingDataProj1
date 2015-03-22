library(plyr)

main <- function(){
  ## common labels  
  labelsFeatures <- read.table("C:/Users/pfornia/Desktop/Coursera/Getting_Data/project/UCI HAR Dataset/features.txt")
  labelsActivity <- read.table("C:/Users/pfornia/Desktop/Coursera/Getting_Data/project/UCI HAR Dataset/activity_labels.txt")
  
  ## Training Set  -------------
  xTrainData <- read.table("C:/Users/pfornia/Desktop/Coursera/Getting_Data/project/UCI HAR Dataset/train/X_train.txt")
  subjectTrain <- read.table("C:/Users/pfornia/Desktop/Coursera/Getting_Data/project/UCI HAR Dataset/train/subject_train.txt")  
  yTrainData <- read.table("C:/Users/pfornia/Desktop/Coursera/Getting_Data/project/UCI HAR Dataset/train/y_train.txt")

    ## Rename data using labelsFeatures 
  if(dim(labelsFeatures)[1] != dim(xTrainData)[2]) stop("Number of labels does not match dataframe.")
  names(xTrainData) <- labelsFeatures[,2]
  
    ## Add columns of subject and activities
  if(dim(subjectTrain)[1] != dim(xTrainData)[1] || dim(yTrainData)[1] != dim(xTrainData)[1]) stop("Number of labels does not match dataframe.")
  xTrainData <- cbind(subjectTrain, yTrainData, xTrainData)
  names(xTrainData)[1:2] <- c("subjectNumber", "activityNumber")


  ## Test Set  -------------
  xTestData <- read.table("C:/Users/pfornia/Desktop/Coursera/Getting_Data/project/UCI HAR Dataset/test/X_test.txt")
  subjectTest <- read.table("C:/Users/pfornia/Desktop/Coursera/Getting_Data/project/UCI HAR Dataset/test/subject_test.txt")  
  yTestData <- read.table("C:/Users/pfornia/Desktop/Coursera/Getting_Data/project/UCI HAR Dataset/test/y_test.txt")
  
    ## Rename data using labelsFeatures 
  if(dim(labelsFeatures)[1] != dim(xTestData)[2]) stop("Number of labels does not match dataframe.")
  names(xTestData) <- labelsFeatures[,2]
  
    ## Add columns of subject and activities
  if(dim(subjectTest)[1] != dim(xTestData)[1] || dim(yTestData)[1] != dim(xTestData)[1]) stop("Number of labels does not match dataframe.")
  xTestData <- cbind(subjectTest, yTestData, xTestData)
  names(xTestData)[1:2] <- c("subjectNumber", "activityNumber")
  
  
  
  ## Join Training and Test  -------------
  xFullData <- rbind(xTrainData, xTestData)
  
  ## Bring in activity name
  names(labelsActivity) <- c("activityNumber", "activityName")
  xFullData <- join(xFullData, labelsActivity, by = "activityNumber")
  
  ## Create data frame of only means and std
  fullDataMS <- data.frame(subject = xFullData$subjectNumber, activity = xFullData$activityName)
    ## Keep only mean() and std() columns
  for(i in 1:length(names(xFullData))){
    ## If the ith name in xFullData contains "mean()" or "std()", then add the ith column to the output data set.
    if(grepl("mean[(][)]|std[(][)]", names(xFullData)[i])){
      fullDataMS[[names(xFullData)[i]]] <- xFullData[[i]]  
    }
  }
  
  ## Average each of these column by person and activity. ---------------------
    ## Sort the fullData first by subject, then by activity.
  fullDataMS <- fullDataMS[order(fullDataMS$subject, fullDataMS$activity),]
    ## Initialize meansData as blank DF with same col names as fullDataMS
  meansData <- fullDataMS[0,]

    ## Initialize a temp DF that will only hold one subject/activity combination at a time.
  categoryData <- fullDataMS[0,]
  numRows <- dim(fullDataMS)[1]
  k=1
    ## Loop through all rows
  for(i in 1:numRows){
    #print(i)
    tempSubject <- fullDataMS$subject[i]
    tempActivity <- fullDataMS$activity[i]
    #categoryData <- rbind(categoryData, fullDataMS[i,])
    ## if the end is reached, or the subject/activity is about to change, then sum up what we've got and append it to the output.
    #if(i==numRows || tempSubject != fullDataMS$subject[i+1] || tempActivity != fullDataMS$activity[i+1]){
    if(i==numRows || tempSubject != fullDataMS$subject[i+1] || tempActivity != fullDataMS$activity[i+1]){
      #stop("wha-?!")
      categoryData <- fullDataMS[k:i,]
      categoryRow <- rep(NA, dim(categoryData)[2])
      categoryRow[1] <- tempSubject
      categoryRow[2] <- tempActivity
      ## for each variable (column), calculate the average
      for(j in 3:(dim(categoryData)[2])){
        categoryRow[j] <- mean(categoryData[,j])
      }
      #print(dim(categoryData))
      meansData <- rbind(meansData, categoryRow)
      ## fix the activity field to show string
      meansData[[2]][length(meansData[[2]])] <- tempActivity
      k <- i+1    
      print(dim(meansData))
    }
  }
  
  for(j in 1:(dim(meansData)[2])){
    names(meansData)[j] <- paste("Average of", names(fullDataMS)[j])
  }
  
  write.table(meansData, file = "C:/Users/pfornia/Desktop/Coursera/Getting_Data/project/meansData.txt", row.name=FALSE)
  
  meansData
}



