
# The code should have a file run_analysis.R in the main directory that can be run as long as the Samsung data is in your working directory
# Hence set the directory to where your data are
setwd("./Coursera_R/Getting_and_Cleaning_Data_lectures/project")
getwd()



# You will be required to submit: 
#1) a tidy data set; # Please upload your data set as a txt file created with write.table() using row.name=FALSE  
#2) a link to a Github repository with your script for performing the analysis, 
#3) a code book that describes the variables, the data, 
# and any transformations or work that you performed to clean up 
# the data called CodeBook.md. 
#4) You should also include a README.md in 
# the repo with your scripts. 
# This repo explains how all of the scripts work and how they are connected.


#WHAT TO DO: under run_analysis.R,

# 1. MERGE the training and the test sets to CREATE ONE DATASET.
# 2. EXTRACT only the measurements on the MEAN and STANDARD DEVIATION for each measurement. 
# 3. Use descriptive activity NAMES to NAME the activities in the data set 
# 4. Appropriately LABEL the data set with descriptive VARIABLE names  
# 5. From the data set in step 4, creates a second, independent TIDY data set with 
# the AVERAGE (=mean) of EACH VARIABLE for EACH ACTIVITY and EACH SUBJECT.


####################################################################
# 1. merge the TRAINING and the TEST sets to create a dataset
####################################################################


# First overview of the data 
# Collection of all the files needed 

# Training set.
list.files("./UCI HAR Dataset/train/")
X_tr <- read.table("./UCI HAR Dataset/train/X_train.txt")
subject_tr <- read.table("./UCI HAR Dataset/train/subject_train.txt")
y_tr <- read.table("./UCI HAR Dataset/train/y_train.txt")



# Test set.
list.files("./UCI HAR Dataset/test/")
X_te <- read.table("./UCI HAR Dataset/test/X_test.txt")
subject_te <- read.table("./UCI HAR Dataset/test/subject_test.txt")
y_te <- read.table("./UCI HAR Dataset/test/y_test.txt")


# At this stage columns can already be better arranged; names to the variables can also already be assigned
# (which will become handly for task 4)


subject <- rbind(subject_te, subject_tr)
str(subject)
activities <- rbind(y_te, y_tr)
str(activities)
raw_sets <- rbind(X_te, X_tr)
str(raw_sets)

names(subject) <- c("subject")
names(activities) <- c("activities") 

# At this stage names of variables taken from features.txt are introduced and assigned to the sub frame raw_sets
featuresNames <- read.table("./UCI HAR Dataset/features.txt", head = F)
names(raw_sets) <- featuresNames$V2
head(raw_sets)

prep_dataset <- cbind(subject, activities)
dim(prep_dataset)

dataset <- cbind(raw_sets, prep_dataset)
dim(dataset)

# Using dplyr for better printing of the dataset and hence assigning the dataset a new name

library(dplyr) # -- making sure to have version 0.4. or above

fancy_dataset <- tbl_df(dataset)
fancy_dataset 

#############################################################################################
# 2. EXTRACT only the measurements on the MEAN and STANDARD DEVIATION for each measurement. 
#############################################################################################

# these values can be found in features.txt, already used above

head(featuresNames, 20)

# The focus for the task is only on mean(tBodyAcc-mean()) and standard deviation (tBodyAcc-std())
# These can be found in the second colum of featuresNames, featuresNames$V2
# For filtering I use the grep() function
str(grep)


features_mean  <- featuresNames[grep("-mean\\(\\)", featuresNames$V2), ]
features_mean

features_sd <- featuresNames[grep("-std\\(\\)", featuresNames$V2), ]
features_sd

# features_parsed just contains measurements ON the mean and the sd for each measurement

features_parsed <- bind_rows(features_mean, features_sd)
features_parsed
View(features_parsed)



###########################################################################
# 3. Use descriptive activity NAMES to NAME the activities in the data set 
###########################################################################

# For this task the file 'activity_labels.txt' is needed, which links the class labels with their activity name.

list.files("./UCI HAR Dataset/")
activity_label <- read.table("./UCI HAR Dataset/activity_labels.txt")

# The function lapply enables to get different *unique* names of the activities in the data frame 
activity_label <- lapply(activity_label, unique)
activity_label

# the result is 6 levels: 
#[1] WALKING            WALKING_UPSTAIRS   WALKING_DOWNSTAIRS SITTING     
#[5] STANDING           LAYING           

#######################################################################
# 4. Appropriately LABEL the data set with descriptive VARIABLE names
#######################################################################

# for this task, the gsub function is used
str(gsub)
# function (pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) 

# to determine which variables to consider I borrow the list from the features.txt file and 
# work on it 

# tBodyAcc-XYZ 't' = 'Time', 'Acc' = 'Accelerometer' 
# tBodyGyro-XYZ 'Gyro' = 'Gyroscope'
# tBodyAccMag 'Mag' = 'Magnitude'
# "tBodyAcc-arCoeff()-X,2" 'arCoeff' = 'Autoregression_Coefficient'
# tBodyGyro-iqr() 'iqr' = 'interquantile'
# tGravityAccMag-sma() 'sma' = 'Simple_Moving_Average"

# I make a copy of fancy_dataset (called fancy_renamed) and work on that 

fancy_renamed <- fancy_dataset
dim(fancy_renamed)

# first, the parentheses from the names of the dataset are removed
names(fancy_renamed) <-gsub('\\(|\\)', "", names(fancy_renamed), perl = T) 
head(fancy_renamed)

# the names of the columns are compacted in a more readeable way
names(fancy_renamed) <- make.names(names(fancy_renamed))
names(fancy_renamed)

# the dataset is labeled with descriptive variable names 
names(fancy_renamed) <- gsub('^t', "Time", names(fancy_renamed))
names(fancy_renamed) <- gsub('Acc', "Acceleration", names(fancy_renamed))
names(fancy_renamed) <- gsub('Gyro', "Gyroscope", names(fancy_renamed))
names(fancy_renamed) <- gsub('Mag', "Magnitude", names(fancy_renamed))
names(fancy_renamed) <- gsub('arCoeff', "Autoregression_Coefficient", names(fancy_renamed))
names(fancy_renamed) <- gsub('iqr', "interquantile", names(fancy_renamed))
names(fancy_renamed) <- gsub('sma', "Simple_Moving_Average", names(fancy_renamed))

names(fancy_renamed)
fancy_renamed



##################################################################################
# 5.From the data set in step 4 (fancy_renamed), create a second, independent TIDY data set with 
# the AVERAGE (= mean) of EACH VARIABLE for EACH ACTIVITY and EACH SUBJECT.
##################################################################################

# for this task, the aggregate() function is used
str(aggregate)

tidy_dataset <- aggregate(. ~ subject + activities, fancy_renamed, mean)
dim(tidy_dataset)

# A txt. table of the tidy dataset is generated

write.table(tidy_dataset, file = "tidy.dataset.txt", row.name = F)


# Getting rid of temporary files to save memory
rm(X_te, X_tr, subject_re, subject_tr, y_te, y_tr, prep_dataset)


