# Load the dplyr package

library(dplyr)

# Get the list of column names from features.txt 

dtNames <- read.table("~/Coursera/Getting_and_Cleansing_Data/Week_4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt", header = FALSE)

# Read the file X_test.txt into datatable dtTest
# Read the test subject details in to dtSubTest from the file subject_test.txt
# Read the test activity details in to dtActTest from the file y_test.txt

dtTest <- read.table("~/Coursera/Getting_and_Cleansing_Data/Week_4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", header = FALSE)

dtSubTest <- read.table("~/Coursera/Getting_and_Cleansing_Data/Week_4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", header = FALSE)

dtActTest <- read.table("~/Coursera/Getting_and_Cleansing_Data/Week_4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", header = FALSE)

# Combine the 3 tables together and add a column to label this data as "Test" data

dtTest <- cbind(dtTest, "Test", dtSubTest, dtActTest)

names(dtTest)[-3] <- "Type"

# Read the file X_train.txt into datatable dtTrain
# Read the train subject details in to dtSubTrain from the file subject_train.txt
# Read the train activity details in to dtActTrain from the file y_train.txt


dtTrain <- read.table("~/Coursera/Getting_and_Cleansing_Data/Week_4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", header = FALSE)

dtSubTrain <- read.table("~/Coursera/Getting_and_Cleansing_Data/Week_4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", header = FALSE)

dtActTrain <- read.table("~/Coursera/Getting_and_Cleansing_Data/Week_4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", header = FALSE)

# Combine the 3 tables together and add a column to label this data as "Train" data

dtTrain <- cbind(dtTrain, "Train", dtSubTrain, dtActTrain)

names(dtTrain)[-3] <- "Type"

# Merge the training and test data sets into one data sets

dtMerged <- rbind(dtTest, dtTrain)

# add the  titles of the "Type", "Subject" and "Activity" columns to the dtNames 

dtNames[nrow(dtNames)+1,2] <- "Type"
dtNames[nrow(dtNames)+1,2] <- "Subject"
dtNames[nrow(dtNames)+1,2] <- "Activity"

# Assign the column names for dtMerged from dtNames

names(dtMerged) <- dtNames[,2]

# Extract the mean and standard deviation for each measurement
# Put them in a data table called dtMeanSD

dtMeanSD <- select(dtMerged, contains("mean()") | contains("std()") | matches("Type") | matches("Subject") | matches("Activity"))

# Read in the activity names table

dtActivity <- read.table("~/Coursera/Getting_and_Cleansing_Data/Week_4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", header = FALSE)

# Create a column for the activity names 

dtMeanSD['Activity_Name'] <- NA

# Populate the activity names by looping through each row and looking them up in the activity names table (match the activity index)

for (i in 1:nrow(dtMeanSD)){

	dtMeanSD[i, ncol(dtMeanSD)] <- dtActivity[match(dtMeanSD[i,ncol(dtMeanSD)-1],dtActivity[,1]),2]

}

# Create a second, independently tidy data set with the average of each variable for
# each activity and each subject

# Group the data by Subject and Activity_Name and find the mean of each variable. 
# Output this to dtSumm

dtSumm <- dtMeanSD %>%
	group_by(Subject, Activity_Name) %>%
	summarise(across(where(is.numeric), mean)) 

write.table(dtSumm, file = "Summary_Grouped_Data.txt", sep = "")