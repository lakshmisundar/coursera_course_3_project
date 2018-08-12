# Loading the required libraries
library("dplyr")
library("reshape2")

# including a file from the file system
include_file <- function(file, dir, subdir="") {
  file.path(dir, subdir, file) %>% read.table(header = FALSE)
}

# Main data source directory
main_dir <- "UCI HAR Dataset"

# Downloading the data sets and extracting its contents
if (!file.exists(main_dir)) {
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  save_file <- "datafiles.zip"
  download.file(url, destfile = save_file)
  unzip(save_file)
}

# user defined columns
subject_column_name <- "Subject"
activity_column_name <- "Activity"

# Loading features
features <- include_file("features.txt", main_dir)

# Loaidng activity labels
activity_labels <- include_file("activity_labels.txt", main_dir)

#Llist for test & train dta labels
map_label <- function(data) {
  names(data) <- activity_column_name
  data$Activity <- factor(data$Activity, levels = activity_labels$V1, labels = activity_labels$V2)
  data
}

# Mapping columns to features
map_action <- function(data) {
  column_names <- gsub("-", "_", features$V2)
  column_names <- gsub("[^a-zA-Z\\d_]", "", column_names)
  names(data) <- make.names(names = column_names, unique = TRUE, allow_ = TRUE)
  data
}

# Map column name to subject
map_subject <- function(data) {
  names(data) <- subject_column_name
  data
}

# Loading training data
train_dataset <- include_file("x_train.txt", main_dir, "train") %>% map_action
train_labelset <- include_file("y_train.txt", main_dir, "train") %>% map_label
train_subjectset <- include_file("subject_train.txt", main_dir, "train") %>% map_subject

# Loading test data
test_dataset <- include_file("x_test.txt", main_dir, "test") %>% map_action
test_labelset <- include_file("y_test.txt", main_dir, "test") %>% map_label
test_subjectset <- include_file("subject_test.txt", main_dir, "test") %>% map_subject

# Merge training an dtest date to make them a single dataset and retrieve only the mean & std. dev.
single_data <- rbind(
  cbind(train_dataset, train_labelset, train_subjectset),
  cbind(test_dataset, test_labelset, test_subjectset)
) %>%
  select(matches("mean|std"),one_of(subject_column_name, activity_column_name))

# prettying the data set with avg. of each variable / activity / subject
id_cols <- c(subject_column_name, activity_column_name)
pretty_data <- melt(single_data, id = id_cols,
                    measure.vars = setdiff(colnames(single_data), id_cols)
) %>% dcast(Subject + Activity ~ variable, mean)

# save the result in .txt formt
write.table(pretty_data, file = "pretty_data.txt", sep = ",", row.names = FALSE)