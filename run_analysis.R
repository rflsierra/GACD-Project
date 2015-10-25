library(reshape2)

## Downloads and unzips file
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "dataset.zip", method = "curl")
unzip("dataset.zip")

## Sets WD in "test" folder
setwd(paste(getwd(),"/UCI HAR Dataset",sep=""))
setwd(paste(getwd(),"/test",sep=""))

## Reads main tables in test folder
Xtest <- read.table("X_test.txt")
ytest <- read.table("y_test.txt")
subjecttest <- read.table("subject_test.txt")

## Sets WD to "Internal Signals" in "test"
setwd(paste(getwd(),"/Inertial Signals",sep=""))

## Creates a list with the names of files in Internal Signal test
istest <- list.files(getwd())

## Creates a list of vectors with the content of the databases 
istestlist <- list()
t <- 1
for(i in 1:length(istest)) {
    istestlist[t] <- read.table(istest[t])
    t <- t + 1
}

setwd("../")
setwd("../")
setwd(paste(getwd(),"/train",sep=""))


## Reads main tables in train folder
Xtrain <- read.table("X_train.txt")
ytrain <- read.table("y_train.txt")
subjecttrain <- read.table("subject_train.txt")

## Sets WD to "Internal Signals" in "train"
setwd(paste(getwd(),"/Inertial Signals",sep=""))

## Creates a list with the names of files in Internal Signal train
istrain <- list.files(getwd())

## Creates a list of vectors with the content of the databases 
istrainlist <- list()
t <- 1
for(i in 1:length(istrain)) {
    istrainlist[t] <- read.table(istrain[t])
    t <- t + 1
}

setwd("../")
setwd("../")

## Gets label information
activities <- read.table("activity_labels.txt")
features <- read.table("features.txt")
features[1,1] <- 1

## Sets WD in the root
setwd("../")

## Sets names without .txt for Internal Signals
t <- 1
for(i in 1:length(istest)) {
    istest[t] <- gsub("_test.txt","",istest[t])
    istrain[t] <- gsub("_train.txt","",istrain[t])
    t <- t + 1
}

## Sets features names in data frames
colnames(Xtest) <- features[,2]
colnames(Xtrain) <- features[,2]

## Adds names of activities according to index
ytestl <- activities[ytest[,1],]
ytrainl <- activities[ytrain[,1],]
colnames(ytestl) <- c("id", "label")
colnames(ytrainl) <- c("id", "label")
colnames(subjecttest) <- c("subject")
colnames(subjecttrain) <- c("subject")

## Binds data from Internal Signal vectors
data.test <- istestlist[[1]]
data.train <- istrainlist[[1]]
t <- 2
for(i in 1:(length(istest)-1)) {
    data.test <- cbind(data.test, istestlist[[t]])
    data.train <- cbind(data.train, istrainlist[[t]])
    t <- t + 1
}

## Assign names to internal signals
colnames(data.test) <- istest
colnames(data.train) <- istrain

## Column-binds the subject, activities, data with features and internal data
data.test <- cbind(subjecttest, ytestl, Xtest, data.test)
data.train <- cbind(subjecttrain, ytrainl, Xtrain, data.train)

## Row-binds the test ans train data
data.total <- rbind(data.test, data.train)

## Melts all measure data by id (subject, index and activity)
data.final <- melt(data.total, id = names(data.total)[1:3], measure.vars = names(data.total)[4:length(names(data.total))])

## Creates a logical filter to select variables that contain "mean" or "std"
filter <- grepl("mean|std", data.final$variable)

## Creates tidy database using the filter for "mean" and "std"
data.tidy <- data.final[filter,]

## Generates average of each variable
data.tidy <- dcast(data.tidy, subject + label ~ variable, mean)

## Creates and output txt file with tidy data
write.table(data.tidy, file = "tidy_data.txt", row.names = FALSE)
