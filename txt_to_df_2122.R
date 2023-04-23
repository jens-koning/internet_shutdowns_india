# Clear working environment
rm(list=ls(all = TRUE))

# Load needed paackages
library(dplyr)
library(tidyr)

# Set the path to the text file
file_path <- "/Users/jenskoning/Documents/R_Projects/Shutdowns_data/shutdowns_india_nourl.txt"

# Read in the text file as a character vector
file_lines <- readLines(file_path)

# Split each line of the text file by a specific delimiter (e.g., tab)
split_lines <- strsplit(file_lines, "\t")

# Convert the split lines into a tabular data frame
tabular_data <- data.frame(do.call(rbind, split_lines), stringsAsFactors = FALSE)
rm(split_lines)

# Set the column names of the tabular data
colnames(tabular_data) <- "all"

# create empty data frames to hold the new columns
new_col1 <- data.frame()
new_col2 <- data.frame()
new_col3 <- data.frame()

# loop through every group of 3 rows in the original column
for(i in seq(1, nrow(tabular_data), by = 3)) {
  # get the values of the current group of 3 rows
  values <- tabular_data$all[i:(i+2)]
  # create a new data frame with the current values
  new_df <- data.frame(col1 = values[1], col2 = values[2], col3 = values[3])
  # add the new data frame to the corresponding new column
  new_col1 <- rbind(new_col1, new_df$col1)
  new_col2 <- rbind(new_col2, new_df$col2)
  new_col3 <- rbind(new_col3, new_df$col3)
}

# Merge columns
tabular_data <- data.frame(new_col1, new_col2, new_col3)
rm(new_col1, new_col2, new_col3, new_df)

# Rename columns
colnames(tabular_data) <- c("date", "description", "location")

# Export data frame to rds file
saveRDS(tabular_data, "tabular_data.rds")


