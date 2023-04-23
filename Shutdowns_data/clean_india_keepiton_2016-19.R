####    \\    Clean India Shutdowns Data from the Access Now project, 2016 to 2019   //    ####

# Clear working environment and install needed packages
rm(list=ls(all = TRUE))

# Load needed packages
my_packages <- c("dplyr", "tidyr", "stringr", "readxl", "sf", "ggplot2", "viridis")
lapply(my_packages, require, character.only = TRUE) 

#### Load data and merge data sets ####
# Load other data source; access now global data base (CODEBOOK: https://www.accessnow.org/wp-content/uploads/2023/03/Read-Me_STOP_data_methodology.pdf)
# The data for India is collected by https://internetshutdowns.in 
keepiton_2019 <- read_excel("Keepiton_raw_dataset.xlsx",
                           sheet = "2019")
keepiton_2016_18 <- read_excel("Keepiton_raw_dataset.xlsx",
                               sheet = "2016+2017+2018 ")

# Filter out Indian shutdowns
keepiton_2016_18 <- keepiton_2016_18 %>%
  filter(country == "India")

#### Create state variable ####
# for loop through area_name variable, separating states from the overall location/or single locations from state
matched_values <- c()

# adding states, but also irregular names (districts instead of states) in keepiton data set 
match_search <- "Andhra Pradesh|Arunachal Pradesh|Assam|Bihar|Chhattisgarh|Delhi|Goa|Gujarat|Haryana|Himachal Pradesh|Jammu and Kashmir|Jammu & Kashmir|South Kashmir|Jharkhand|
Karnataka|Kerala|Madhya Pradesh|Maharashtra|Manipur|Meghalaya|Mizoram|Nagaland|Odisha|Punjab|Rajasthan|Sikkim|Tamil Nadu|Telangana|Tripura|Uttar Pradesh|Uttarakhand|Jammu|Kashmir Valley|Kashmir|West Bengal"

# loop through each string in the column of interest
for (i in 1:nrow(keepiton_2016_18)) {
  # use grep function to check for matches with specified pattern
  if(grepl(match_search,
           keepiton_2016_18$area_name[i])) {
    # if there is a match, add the matched value to the vector
    matched_values[i] <- str_extract(keepiton_2016_18$area_name[i], match_search)
  } else {
    # if there is no match, add NA to the vector
    matched_values[i] <- NA
  }
}
# add the matched values vector as a new column to the scarped_data data frame
keepiton_2016_18$state <- matched_values

# harmonize irregular names (districts) to actual province name

# change idiosyncatic naming of Jammu and Kashmir
keepiton_2016_18$state <- gsub("Jammu & Kashmir|Jammu|South Kashmir|Kashmir Valley|Kashmir", "Jammu and Kashmir",  keepiton_2016_18$state)
# other disctricts discovered earlier that belong to Jammu and Kashmir
# district belonging to Arunachal Pradesh
keepiton_2016_18$state <- gsub("Itanagar", "Arunachal Pradesh", keepiton_2016_18$state)

#### Create string with all indian districts ####

# load list of all Indian districts from GADM
india_shapes_disctricts <- st_read("/Users/jenskoning/Documents/R_Projects/internet_shutdowns_india/Shutdowns_data/gadm41_IND_2.json")
# make a vector with all Indian districts based on NAME_2 variable
input_string <- as.character(india_shapes_disctricts$NAME_2)

# transform to single string with "|" divider
transform_to_string <- function(input_string) {
  # Remove unwanted characters
  cleaned_string <- gsub("\\s*chr\\s*\\[[0-9]+:[0-9]+\\]\\s*\"", "", input_string)
  cleaned_string <- gsub("\"", "", cleaned_string)
  # Split the string into a vector using the space delimiter
  temp_vector <- unlist(strsplit(cleaned_string, " "))
  # Join the vector elements into a single character string using "|" delimiter
  result_string <- paste(temp_vector, collapse = "|")
  return(result_string)
}
# implement function on districts
india_districts <- transform_to_string(input_string)

#### Create district variable ####

# modify source coloumn based on known anomaly 
keepiton_2016_18$area_name <- gsub('Leh(ladakh)', 'Leh(Ladakh)', keepiton_2016_18$area_name)
keepiton_2016_18$area_name <- gsub('Shopian', 'Shupiyan', keepiton_2016_18$area_name)
keepiton_2016_18$area_name <- gsub('Budgam', 'Badgam', keepiton_2016_18$area_name)

# remove white space 
keepiton_2016_18$area_name <- gsub(" ", "",keepiton_2016_18$area_name)

# for loop through area_name variable, separating districts into district variable 
matched_districts <- c()
district_search <- india_districts

# loop through each string in the column of interest
for (i in 1:nrow(keepiton_2016_18)) {
  # use grep function to check for matches with specified pattern
  if(grepl(district_search,
           keepiton_2016_18$area_name[i])) {
    # if there is a match, add the matched value to the vector
    matched_districts[i] <- str_extract_all(keepiton_2016_18$area_name[i], district_search)
  } else {
    # if there is no match, add NA to the vector
    matched_districts[i] <- NA
  }
}
# add matched results to new varible
keepiton_2016_18$districts <- matched_districts

#### Manual verification and cleaning of districts variable #### 

# change data format of keepiton_2016_18$districts to chr
keepiton_2016_18$districts <- as.character(keepiton_2016_18$districts)

# removing unneeded vector notation 
keepiton_2016_18$districts <- gsub('c\\("', "", keepiton_2016_18$districts)
keepiton_2016_18$districts <- gsub('")', "", keepiton_2016_18$districts)
keepiton_2016_18$districts <- gsub('"', "", keepiton_2016_18$districts)

# removing all words related to state, i.e. 'Jammu', 'West' etc.
keepiton_2016_18$districts <- gsub(', Jammu', "", keepiton_2016_18$districts)
keepiton_2016_18$districts <- gsub(', West', "", keepiton_2016_18$districts)
keepiton_2016_18$districts <- gsub('Jammu', NA, keepiton_2016_18$districts)

# add states to NA
keepiton_2016_18$state[4] <- "Gujarat"
keepiton_2016_18$state[8] <- "Jharkhand"
keepiton_2016_18$state[13] <- "Jammu and Kashmir"
keepiton_2016_18$state[71] <- "Rajasthan"
keepiton_2016_18$state[81] <- "Tripura"
keepiton_2016_18$state[85] <- "Jammu and Kashmir"
keepiton_2016_18$state[86] <- "Rajasthan"
keepiton_2016_18$state[90] <- "Rajasthan"
keepiton_2016_18$state[92] <- "Rajasthan"
keepiton_2016_18$state[113] <- "Uttar Pradesh"
keepiton_2016_18$state[117] <- "Jammu and Kashmir"
keepiton_2016_18$state[129] <- "Bihar"
keepiton_2016_18$state[130] <- "Rajasthan"
keepiton_2016_18$state[138] <- "Uttar Pradesh"
keepiton_2016_18$state[150] <- "Uttar Pradesh"
keepiton_2016_18$state[154] <- "Uttarakhand"
keepiton_2016_18$state[158] <- "Tamil Nadu"
keepiton_2016_18$state[166] <- "Meghalaya"
keepiton_2016_18$state[192] <- "Gujarat"
keepiton_2016_18$state[201] <- "Jammu and Kashmir"
keepiton_2016_18$state[221] <- "Rajasthan"

# for loop for Kashmir Valley
for (i in 1:nrow(keepiton_2016_18)) {
  if (keepiton_2016_18$area_name[i] == "KashmirValley") {
    keepiton_2016_18$districts[i] <- "Anantnag, Kulgam, Pulwama, Shupiyan, Badgam, Srinagar, Ganderbal, Bandipore, Baramulla, Kupwara"
  }
}

# shorter data set for manual check
temp_df <- keepiton_2016_18 %>% select(start_date, area_name, state, districts)
rm(temp_df)

# manual check of districts
keepiton_2016_18$districts[] <- ""
keepiton_2016_18$districts[] <- ""
keepiton_2016_18$districts[] <- ""
keepiton_2016_18$districts[] <- ""
