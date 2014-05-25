readData <- function(prefix) {
  # read the 'y' data
  fpath <- file.path(prefix, paste0("y_", prefix, ".txt"))
  y_data <- read.table(fpath, header=F, col.names=c("ActivityID"))
  
  # read the 'subject' data
  fpath <- file.path(prefix, paste0("subject_", prefix, ".txt"))
  subject_data <- read.table(fpath, header=F, col.names=c("SubjectID"))
  
  # read the column names
  data_cols <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
  
  # read the 'x' data 
  fpath <- file.path(prefix, paste0("X_", prefix, ".txt"))
  data <- read.table(fpath, header=F, col.names=data_cols$MeasureName)
  
  # names of subset columns required
  subset_data_cols <- grep(".*mean\\(\\)|.*std\\(\\)", data_cols$MeasureName)
  data <- data[,subset_data_cols]
  
  # append the activity id and subject id columns
  data$ActivityID <- y_data$ActivityID
  data$SubjectID <- subject_data$SubjectID
  
  # return the data
  data
}

# read test data set
readTestData <- function() {readData("test")}

# read train data set
readTrainData <- function() {readData("train")}

# Merge both train and test data sets
# Also make the column names nicer
mergeData <- function() {
  data <- rbind(readTestData(), readTrainData())
  cnames <- colnames(data)
  cnames <- gsub("\\.+mean\\.+", cnames, replacement="Mean")
  cnames <- gsub("\\.+std\\.+",  cnames, replacement="Std")
  colnames(data) <- cnames
  data
}

# Add the activity names as another column
applyActivityLabel <- function(data) {
  activity_labels <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
  activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)
  data_labeled <- merge(data, activity_labels)
  data_labeled
}

# Combine training and test data sets and add the activity label as another column
getMergedLabeledData <- function() {
  applyActivityLabel(mergeData())
}

# Create a tidy data set that has the average of each variable for each activity and each subject.
getTidyData <- function(merged_labeled_data) {
  library(reshape2)
  
  # melt the dataset
  id_vars = c("ActivityID", "ActivityName", "SubjectID")
  measure_vars = setdiff(colnames(merged_labeled_data), id_vars)
  melted_data <- melt(merged_labeled_data, id=id_vars, measure.vars=measure_vars)
  
  # recast 
  dcast(melted_data, ActivityName + SubjectID ~ variable, mean)    
}

# Create the tidy data set and save it on to the named file
createTidyDataFile <- function(fname) {
  tidy_data <- getTidyData(getMergedLabeledData())
  write.table(tidy_data, fname)
}

print("Creating tidy dataset as tidy.txt")
createTidyDataFile("tidy.txt")
print("Completed")
