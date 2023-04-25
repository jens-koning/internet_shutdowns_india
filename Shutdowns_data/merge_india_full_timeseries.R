####    \\    Merge relevant columns of different datasets    //    ####

# Clear working environment and install needed packages
rm(list=ls(all = TRUE))

# Load needed packages
my_packages <- c("dplyr", "tidyr", "sf", "ggplot2", "viridis")
lapply(my_packages, require, character.only = TRUE) 
