####    \\    Clean India Shutdowns Data from the Access Now project, 2016 to 2018   //    ####

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
# Filter out Indian shutdowns
keepiton_2019 <- keepiton_2019 %>%
  filter(country == "India")

#### Create state variable ####
# for loop through area_name variable, separating states from the overall location/or single locations from state
matched_values <- c()

# adding states, but also irregular names (districts instead of states) in keepiton data set 
match_search <- "Police district of Awantipora (Pulwama), in Jammu Kashmir|District Pulwama|Pulwama|Leh (ladakh)|Kulgam|Kulgam and Shupiyan|Badgam|Anantnag and Kulgam districts|Kulgam and Shupiyan|Anantnag|Koregaon Bhima, Perne village in Pune district|Kashmir Valley districts|Shupiyan and Pulwama districts|Kashmir Valley|Itanagar|Andhra Pradesh|Arunachal Pradesh|Assam|Bihar|Chhattisgarh|Delhi|Goa|Gujarat|Haryana|Himachal Pradesh|Jammu and Kashmir|Jammu & Kashmir|South Kashmir|Jharkhand|
Karnataka|Kerala|Madhya Pradesh|Maharashtra|Manipur|Meghalaya|Mizoram|Nagaland|Odisha|Punjab|Rajasthan|Sikkim|Tamil Nadu|Telangana|Tripura|Uttar Pradesh|Uttarakhand|West Bengal"

# loop through each string in the column of interest
for (i in 1:nrow(keepiton_2019)) {
  # use grep function to check for matches with specified pattern
  if(grepl(match_search,
           keepiton_2019$area_name[i])) {
    # if there is a match, add the matched value to the vector
    matched_values[i] <- str_extract(keepiton_2019$area_name[i], match_search)
  } else {
    # if there is no match, add NA to the vector
    matched_values[i] <- NA
  }
}
# add the matched values vector as a new column to the scarped_data data frame
keepiton_2019$state <- matched_values

# harmonize irregular names (districts) to actual province name
keepiton_2019$state <- gsub("Jammu & Kashmir", "Jammu and Kashmir", keepiton_2019$state)
keepiton_2019$state <- gsub("South Kashmir", "Jammu and Kashmir", keepiton_2019$state)
# other disctricts discovered earlier that belong to Jammu and Kashmir
keepiton_2019$state <- gsub("Police district of Awantipora (Pulwama), in Jammu Kashmir|District Pulwama|Pulwama|Leh (ladakh)|Kulgam|Kulgam and Shupiyan|Badgam|Anantnag and Kulgam districts|Kulgam and Shupiyan|Anantnag|Koregaon Bhima, Perne village in Pune district|Kashmir Valley districts|Shupiyan and Pulwama districts|Kashmir Valley", "Jammu and Kashmir", keepiton_2019$state) # CHECK
# district belonging to Arunachal Pradesh
keepiton_2019$state <- gsub("Itanagar", "Arunachal Pradesh", keepiton_2019$state)

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
keepiton_2019$area_name <- gsub('Leh(ladakh)', 'Leh(Ladakh)', keepiton_2019$area_name)
keepiton_2019$area_name <- gsub('Shopian', 'Shupiyan', keepiton_2019$area_name)
keepiton_2019$area_name <- gsub('Budgam', 'Badgam', keepiton_2019$area_name)

# remove white space 
keepiton_2019$area_name <- gsub(" ", "",keepiton_2019$area_name)

# for loop through area_name variable, separating districts into district variable 
matched_districts <- c()
district_search <- india_districts

# loop through each string in the column of interest
for (i in 1:nrow(keepiton_2019)) {
  # use grep function to check for matches with specified pattern
  if(grepl(district_search,
           keepiton_2019$area_name[i])) {
    # if there is a match, add the matched value to the vector
    matched_districts[i] <- str_extract_all(keepiton_2019$area_name[i], district_search)
  } else {
    # if there is no match, add NA to the vector
    matched_districts[i] <- NA
  }
}
# add matched results to new varible
keepiton_2019$districts <- matched_districts

#### Manual verification and cleaning of districts variable #### 

# change data format of keepiton_2019$districts to chr
keepiton_2019$districts <- as.character(keepiton_2019$districts)

# removing unneeded vector notation 
keepiton_2019$districts <- gsub('c\\("', "", keepiton_2019$districts)
keepiton_2019$districts <- gsub('")', "", keepiton_2019$districts)
keepiton_2019$districts <- gsub('"', "", keepiton_2019$districts)

# removing all words related to state, i.e. 'Jammu', 'West' etc.
keepiton_2019$districts <- gsub(', Jammu', "", keepiton_2019$districts)
keepiton_2019$districts <- gsub(', West', "", keepiton_2019$districts)
keepiton_2019$districts <- gsub('Jammu', NA, keepiton_2019$districts)

# for loop for Kashmir Valley
for (i in 1:nrow(keepiton_2019)) {
  if (keepiton_2019$area_name[i] == "KashmirValley,") {
    keepiton_2019$districts[i] <- "Anantnag, Kulgam, Pulwama, Shupiyan, Badgam, Srinagar, Ganderbal, Bandipore, Baramulla, Kupwara"
  }
}

# manual check of states
keepiton_2019$state[9] <- "Jammu and Kashmir"
keepiton_2019$state[12] <- "Jammu and Kashmir" 
keepiton_2019$state[14] <- "Jammu and Kashmir" 
keepiton_2019$state[21] <- "Uttar Pradesh"
keepiton_2019$state[29] <- "Jammu and Kashmir"
keepiton_2019$state[117] <- "Karnataka"

# manual check of districts
keepiton_2019$districts[5] <- "Dhalai, Gomati, Khowai, North Tripura, Sipahijala, South Tripura, Unokoti, West Tripura"
keepiton_2019$districts[17] <- "ImphalEast, ImphalWest"
keepiton_2019$districts[23] <- "Jammu"
keepiton_2019$districts[23] <- "Jammu"
keepiton_2019$districts[25] <- "Baramulla"
keepiton_2019$districts[26] <- "PapumPare"
keepiton_2019$districts[29] <- "Baramulla"
keepiton_2019$districts[30] <- "Srinagar"
keepiton_2019$districts[31] <- "Kupwara"
keepiton_2019$districts[32] <- "Pulwama"
keepiton_2019$districts[37] <- "Baramulla"
keepiton_2019$districts[42] <- "Amritsar, Barnala, Bathinda, Faridkot, FatehgarhSahib, Fazilka, Firozpur, Gurdaspur, Hoshiarpur, Jalandhar, Kapurthala, Ludhiana, Mansa, Moga, Muktsar, Pathankot, Patiala, Rupnagar, SahibzadaAjitSinghNagar, Sangrur, ShahidBhagatSinghNagar, TarnTaran"
keepiton_2019$districts[50] <- "Barddhaman"
keepiton_2019$districts[53] <- "Anantnag, Kulgam, Pulwama, Shupiyan, Badgam, Srinagar, Ganderbal, Bandipore, Baramulla, Kupwara"
keepiton_2019$districts[56:57] <- "Anantnag, Kulgam, Pulwama, Shupiyan, Badgam, Srinagar, Ganderbal, Bandipore, Baramulla, Kupwara"
keepiton_2019$districts[62] <- "Bandipore"
keepiton_2019$districts[65] <- "Baramulla"
keepiton_2019$districts[68] <- "Pulwama"
keepiton_2019$districts[70] <- "Baramulla"
keepiton_2019$districts[77] <- "North24Parganas" 
keepiton_2019$districts[93] <- "Anantnag, Kulgam, Pulwama, Shupiyan, Badgam, Srinagar, Ganderbal, Bandipore, Baramulla, Kupwara"
keepiton_2019$districts[98] <- "Barddhaman"
keepiton_2019$districts[102] <- "Baramulla"
keepiton_2019$districts[103] <- "Anantnag, Kulgam, Pulwama, Shupiyan, Badgam, Srinagar, Ganderbal, Bandipore, Baramulla, Kupwara"
keepiton_2019$districts[110] <- "Anjaw, Changlang, Dibang Valley, East Kameng, East Siang, East Siang, Kurung Kumey, Lohit, Lohit, Longding, Lower Dibang Valley, Lower Dibang Valley, Lower Subansiri, Namsai, Papum Pare, Tawang, Tirap, Upper Siang, Upper Subansiri"
keepiton_2019$districts[111] <- "Dhalai, Gomati, Khowai, North Tripura, Sipahijala, South Tripura, Unokoti, West Tripura"
keepiton_2019$districts[112] <- "Lakhimpur, Dhemaji, Tinsukia, Dibrugarh, Sivasagar, Jorhat, Golaghat, Kamrup, KamrupMetropolitan"
keepiton_2019$districts[113] <- "East Khasi Hills, West Khasi Hills, South West Khasi Hills, Eastern West Khasi Hills, RiBhoi, EastGaroHills, JaintiaHills, NorthGaroHills, SouthGaroHills, SouthWestGaroHills, WestGaroHills"
keepiton_2019$districts[115] <- "Aligarh, Meerut"
keepiton_2019$districts[116] <-"Allahabad, Lucknow, Bareilly, Aligarh, Ghaziabad, Sambhal, Mau, Meerut, Kanpur Dehat, Kanpur Nagar"
keepiton_2019$districts[118] <- "Jabalpur, Bhopal, Indore"
keepiton_2019$districts[119] <- "Allahabad, Firozabad"
keepiton_2019$districts[120] <- "Jaipur"
keepiton_2019$districts[121] <- "Bijnor, Muzaffarnagar, Meerut, Agra, Firozabad, Sambhal, Aligarh, Ghaziabad, Rampur, Sitapur, KanpurDehat, KanpurNagar"

# maunal check with temp object
temp_df <- keepiton_2019 %>% select(start_date, area_name, state, districts, news_link)
rm(temp_df)

#### Creating new rows (obs) for district shutdown ####
# use separate_rows() to split keepiton_2019$districts
keepiton_2019 <- keepiton_2019 %>%
  # Separate location values into rows
  separate_rows(districts, sep = ", ") 
# maunal check with temp object
temp_df <- keepiton_2019 %>% select(start_date, area_name, state, districts)
rm(temp_df)

#### Export to .rds ####

# state level / all related to communal violence
district_event_shutdown_2019 <- keepiton_2019
saveRDS(district_event_shutdown_2019, file = "district_event_shutdown_2019.rds")
district_event_shutdown_communal_2019 <- district_event_shutdown_2019 %>%
  filter(actual_cause == "Communal Violence")
saveRDS(district_event_shutdown_communal_2019, file = "district_event_shutdown_communal_2019.rds")

# remove temp objects
rm(list=c("district_event_shutdown_communal_2019", "india_shapes_disctricts", "district_event_shutdown_2019", "matched_districts"))


