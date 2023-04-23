# Clear working environment
rm(list=ls(all = TRUE))

# Install haven and load data set
library(readxl)
shutdowns2020 <- read_excel("MASTERFILE.xlsx")

# Filter events in 2020
shutdowns2020 <- filter(shutdowns2020, year == 2020)

# Shutdowns occuring within state
shutdowns2020 <- filter(shutdowns2020, shutdown_state == 1)

