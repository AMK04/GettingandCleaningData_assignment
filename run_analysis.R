# Unzip dataset
zipF   <- "C:\\Users\\amber\\Downloads\\getdata_projectfiles_UCI HAR Dataset.zip"
outDir <-"C:\\R projects\\GettingandCleaningData_assignment"
unzip(zipF,exdir=outDir)

# 1. Merge the training and the test sets to create one data set.
# 1.1 Read tables with features and labels
features <- read.table('C:\\R projects\\GettingandCleaningData_assignment\\UCI HAR Dataset\\features.txt')

activitylabels <- read.table('C:\\R projects\\GettingandCleaningData_assignment\\UCI HAR Dataset\\activity_labels.txt')
colnames(activitylabels) <- c('activityID','activitytype')

# 1.2 Read tables and add column names
x_train       <- read.table("C:\\R projects\\GettingandCleaningData_assignment\\UCI HAR Dataset\\train\\X_train.txt")
y_train       <- read.table("C:\\R projects\\GettingandCleaningData_assignment\\UCI HAR Dataset\\train\\y_train.txt")
subject_train <- read.table("C:\\R projects\\GettingandCleaningData_assignment\\UCI HAR Dataset\\train\\subject_train.txt")

colnames(x_train)       <- features[,2]
colnames(y_train)       <-"activityID"
colnames(subject_train) <- "subjectID"

x_test       <- read.table("C:\\R projects\\GettingandCleaningData_assignment\\UCI HAR Dataset\\test\\X_test.txt")
y_test       <- read.table("C:\\R projects\\GettingandCleaningData_assignment\\UCI HAR Dataset\\test\\y_test.txt")
subject_test <- read.table("C:\\R projects\\GettingandCleaningData_assignment\\UCI HAR Dataset\\test\\subject_test.txt")

colnames(x_test)       <- features[,2]
colnames(y_test)       <-"activityID"
colnames(subject_test) <- "subjectID"

# 1.3 Merge dataframes 
merge_train <- cbind(y_train, subject_train, x_train)
merge_test  <- cbind(y_test, subject_test, x_test)
merged_df   <- rbind(merge_train, merge_test)

# 2 Extracting only the measurements on the mean and standard deviation for each measurement.
# 2.1 Identify columns to keep
columns <- grepl("subject|activity|mean|std", colnames(merged_df))

# 2.2 Select columns from df
df_mean_std <- merged_df[, columns]

# 3. Using descriptive activity names to name the activities in the data set
df_mean_std$activityID <- factor(df_mean_std$activityID, 
                                 levels = activitylabels[, 1], labels = activitylabels[, 2])

# 4. Appropriately labeling the data set with descriptive variable names.
# 4.1 Replace/delete part of column names
df_colnames <- gsub("[\\(\\)-]", "", colnames(df_mean_std)) 
df_colnames <- gsub("^f", "frequency", df_colnames)
df_colnames <- gsub("^t", "time", df_colnames)
df_colnames <- gsub("BodyBody", "Body", df_colnames)

# 4.2 Replace column names in df    
colnames(df_mean_std) <- df_colnames

# 5. Creating a second, independent tidy data set from the data set in step 4 with the average of each variable for each activity and each subject.
library('dplyr')
tidy_data_set <- df_mean_std %>%
      group_by(activityID, subjectID) %>%
      summarise_each(funs = mean)