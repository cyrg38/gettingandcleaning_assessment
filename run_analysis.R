#You should create one R script called run_analysis.R that does the following. 
#1. Merges the training and the test sets to create one data set.
features <- read.delim("UCI HAR Dataset/features.txt", sep = " ", header=F)[2]
features <- as.factor(features[,1])
activity_labels <- read.delim("UCI HAR Dataset/activity_labels.txt", sep = " ", header=F)
names(activity_labels) <- c("index", "activityLabel")
subject_train <- read.delim("UCI HAR Dataset/train/subject_train.txt", sep = " ", header=F)
subject_train <- as.factor(subject_train[,1])
y_train <- read.delim("UCI HAR Dataset/train/y_train.txt", sep = " ", header=F)
y_train <- as.factor(y_train[,1])
X_train <- read.delim("UCI HAR Dataset/train/X_train.txt", sep = "", header=F)
names(X_train) <- features
traindataset <- cbind(X_train, subject_train, y_train)
#same with test dataset
names(traindataset)[562:563] <- c("subject","activity")
X_test <- read.delim("UCI HAR Dataset/test/X_test.txt", sep = "", header=F)
names(X_test) <- features
subject_test <- read.delim("UCI HAR Dataset/test/subject_test.txt", sep = " ", header=F)
subject_test <- as.factor(subject_test[,1])
y_test <- read.delim("UCI HAR Dataset/test/y_test.txt", sep = " ", header=F)
y_test <- as.factor(y_test[,1])
testdataset <- cbind(X_test, subject_test, y_test)
names(testdataset)[562:563] <- c("subject","activity")
#finally unify train and test datasets
totaldataset <- rbind(traindataset, testdataset)
rm(X_train, X_test, traindataset, testdataset)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
extractsIndex <- c(grep(pattern = "mean",names(totaldataset),value=F,ignore.case = T), grep(pattern = "std",names(totaldataset),value=F, ignore.case = T), 562,563)
reduceddataset <- totaldataset[,extractsIndex]
# vector of all means
#m <- colMeans(totaldataset[,1:561])
# vector of all standard deviations
#s <- sapply(totaldataset[, 1:561],sd)
#rm(m,s)
#3. Uses descriptive activity names to name the activities in the data set
x <- as.vector(reduceddataset[,88])
for (i in 1:6) {
  x <- gsub(activity_labels[i,1],activity_labels[i,2],x)
}
reduceddataset[,88] <- x
rm(x)
#4. Appropriately labels the data set with descriptive variable names. 
#already done
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subjects <- unique(reduceddataset$subject)
tidydataset <- c()
for (i in subjects) {
  for (j in activity_labels[,2]) {
    #print(paste("subject #",i," - activity #",j))
    tidydataset <- rbind(tidydataset, c(colMeans(reduceddataset[reduceddataset$subject == i & reduceddataset$activity==j,1:86]), i, j))
  }
}
tidydataset <- as.data.frame(tidydataset)
names(tidydataset)[87:88] <- c("subject","activity")
