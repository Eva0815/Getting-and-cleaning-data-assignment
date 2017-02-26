require(dplyr)

# Create directory (if not existing) and set working directory
if (!file.exists("Assignment")) {
    dir.create("Assignment")
}
setwd("./Assignment")

# Download file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "dataset.zip"
download.file(fileUrl, destfile = fileName)
unzip(fileName)

# Load features and activities
features <- read.table(file = "./dataset/UCI HAR Dataset/features.txt")
activities <- read.table(file = "./dataset/UCI HAR Dataset/activity_labels.txt")

# Load training dataset
train <- read.table(file = "./dataset/UCI HAR Dataset/train/X_train.txt")
train.activities <- read.table(file = "./dataset/UCI HAR Dataset/train/y_train.txt")
train.subject <- read.table(file = "./dataset/UCI HAR Dataset/train/subject_train.txt")

# Load test dataset
test <- read.table(file = "./dataset/UCI HAR Dataset/test/X_test.txt")
test.activities <- read.table(file = "./dataset/UCI HAR Dataset/test/y_test.txt")
test.subject <- read.table(file = "./dataset/UCI HAR Dataset/test/subject_test.txt")

# Set column names in training and test dataset
names(train) <- features[,2]
names(test) <- features[,2]

# Add activities and subject information
train <- cbind(train.subject, train.activities, train)
names(train)[1] <- "Subject"
names(train)[2] <- "Activity"

test <- cbind(test.subject, test.activities, test)
names(test)[1] <- "Subject"
names(test)[2] <- "Activity"

# Check if the frequencies of measurements per subject and activities seems plausible
table(train[1:2])
table(test[1:2])

# Combine training and test data
all <- rbind(train, test)

# Add descriptive name of activities
all$Activity <- factor(all$Activity, labels = activities[,2])

# Select only mean and standard deviation variables
rel <- all[, c(1, 2, grep("mean\\(\\)|std\\(\\)", names(all)))]
# remove all special characters from variable names
names(rel) <- gsub("-", "", names(rel))
names(rel) <- gsub("\\()", "", names(rel))
# convert to lower case
names(rel) <- tolower(names(rel))

# Select only mean variables and calculate mean for each subject and activity
rel2 <- rel[, c(1, 2, grep("mean", names(rel)))]
summaryrel2 <- group_by(rel2, subject, activity) %>%
    summarize_each(funs(mean))
 
# Export data
write.table(summaryrel2, "tidyData.txt", row.names = FALSE)


