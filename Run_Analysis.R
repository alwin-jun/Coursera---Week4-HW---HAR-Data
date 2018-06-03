# Goal:
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names.
# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Step0: Getting Data
setwd('C:\\Users\\CJ\\Documents\\Coursera - John Hopkins - 1\\Data Clean up\\');
URL1 = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(URL1, "./HAR Dataset.zip")
unzip("./HAR Dataset.zip")

# Train Data
x_train = read.table("./UCI HAR Dataset/train/X_train.txt")
y_train = read.table("./UCI HAR Dataset/train/Y_train.txt")
sub_train = read.table("./UCI HAR Dataset/train/subject_train.txt")

# Test data
x_test = read.table("./UCI HAR Dataset/test/X_test.txt")
y_test = read.table("./UCI HAR Dataset/test/Y_test.txt")
sub_test = read.table("./UCI HAR Dataset/test/subject_test.txt")

# Loading Data Feature & Activity Label
feature = read.table("./UCI HAR Dataset/features.txt")
activity_label = read.table("./UCI HAR Dataset/activity_labels.txt")

# Step1: Merges the training and the test sets to create one data set.
x_all = rbind(x_train, x_test)
y_all = rbind(y_train, y_test)
sub_all = rbind(sub_train, sub_test)

colnames(x_all) = feature[,2]
colnames(y_all) = "activityId"
colnames(sub_all) = "subjectId"
colnames(activity_label) <- c('activityId','activityType')

# Checking
head(x_all)
head(y_all)
head(sub_all)
head(activity_label)

# combine all together
all = cbind(y_all, sub_all, x_all)
head(all[,1:15])

# Step2: Extracts only the measurements on the mean and standard deviation for each measurement.
Indi_MeanSd = (grepl("activityId" , colnames(all)) |
                        grepl("subjectId" , colnames(all)) |
                        grepl(".mean()." , colnames(all)) |
                        grepl(".std()." , colnames(all)) #&
                       # !grepl("meanFreq." , colnames(all))
               )
all_foruse = all[,Indi_MeanSd]
Indi_MeanSd2 = !grepl("meanFreq", colnames(all_foruse))
all_foruse2 = all_foruse[,Indi_MeanSd2]

# Step3: Uses descriptive activity names to name the activities in the data set
all_foruse3 = merge(all_foruse2, activity_label, by='activityId')
head(all_foruse3)

# Step4: Appropriately labels the data set with descriptive variable names.
colnames(all_foruse3) = gsub("\\()", "", colnames(all_foruse3))
table(colnames(all_foruse3))

# Step5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
result_mean <- all_foruse3 %>%
        group_by(activityType, subjectId) %>%
        summarise_all(funs(mean))

head(result_mean[,1:6])
dim(result_mean)
table(all_foruse3$activityType)
table(all_foruse3$subjectId)

write.table(result_mean, file = "./UCI HAR Dataset/HAR data - tidy.txt", row.names = FALSE, col.names = TRUE)

