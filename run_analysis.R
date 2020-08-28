# Library include
library(dplyr)

# Download file
zip_file <- "Coursera_DS3_Final.zip"
zip_file

# Check if file exists
if (!file.exists(zip_file)){
  URL_file <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(URL_file, zip_file, method="curl")
}  


# Check if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(zip_file) 
}


#Read data
features_data <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities_data <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test_data <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test_data <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features_data$functions)
y_test_data <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train_data <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train_data <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features_data$functions)
y_train_data <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


#Merges the training and the test sets to create one data set.
x_data <- rbind(x_train_data, x_test_data)
y_data <- rbind(y_train_data, y_test_data)
subject_data <- rbind(subject_train_data, subject_test_data)
merged_data <- cbind(subject_data, y_data, x_data)


#Extracts only the measurements on the mean and standard deviation for each measurement.
tidy_data <- merged_data %>% select(subject, code, contains("mean"), contains("std"))


#Uses descriptive activity names to name the activities in the data set
tidy_data$code <- activities_data[tidy_data$code, 2]


#Appropriately labels the data set with descriptive variable names.
names(tidy_data)[2] = "activity"
names(tidy_data)<-gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data)<-gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data)<-gsub("BodyBody", "Body", names(tidy_data))
names(tidy_data)<-gsub("Mag", "Magnitude", names(tidy_data))
names(tidy_data)<-gsub("^t", "Time", names(tidy_data))
names(tidy_data)<-gsub("^f", "Frequency", names(tidy_data))
names(tidy_data)<-gsub("tBody", "TimeBody", names(tidy_data))
names(tidy_data)<-gsub("-mean()", "Mean", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("-std()", "STD", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("-freq()", "Frequency", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("angle", "Angle", names(tidy_data))
names(tidy_data)<-gsub("gravity", "Gravity", names(tidy_data))


#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
final_data <- tidy_data %>%
  group_by(subject, activity) %>%
  summarise_all(list(~mean(.)))
write.table(final_data, "final_data.txt", row.name=FALSE)


#View final data
final_data

str(final_data)

head(final_data)

