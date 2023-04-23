####    \\    Clean India Shutdowns Data from the Access Now project, 2020 to 2022   //    ####

# Clear working environment and install needed packages
rm(list=ls(all = TRUE))

# Load needed packages
my_packages <- c("dplyr", "tidyr", "stringr", "readxl", "sf", "ggplot2", "viridis")
lapply(my_packages, require, character.only = TRUE) 

#### Load data and merge data sets ####
# Load other data source; access now global data base (CODEBOOK: https://www.accessnow.org/wp-content/uploads/2023/03/Read-Me_STOP_data_methodology.pdf)
# The data for India is collected by https://internetshutdowns.in 
keepiton_2022 <- read_excel("Keepiton_raw_dataset.xlsx",
                            sheet = "2022 Data") 
keepiton_2021 <- read_excel("Keepiton_raw_dataset.xlsx",
                            sheet = "2021 Data")
keepiton_2020 <- read_excel("Keepiton_raw_dataset.xlsx",
                            sheet = "2020 Data")
keepiton_2019<- read_excel("Keepiton_raw_dataset.xlsx",
                            sheet = "2019")
keepiton_2016_18 <- read_excel("Keepiton_raw_dataset.xlsx",
                            sheet = "2016+2017+2018 ")
keepiton_full <- bind_rows(keepiton_2022, keepiton_2021, keepiton_2020)

# Filter out Indian shutdowns
keepiton_india <- keepiton_full %>%
  filter(country == "India")

# remove temp objects 
rm(list=c("keepiton_2022", "keepiton_2021", "keepiton_2020"))

#### Create state variable ####
# for loop through area_name variable, separating states from the overall location/or single locations from state
matched_values <- c()

# adding states, but also irregular names (districts instead of states) in keepiton data set 
match_search <- "Police district of Awantipora (Pulwama), in Jammu Kashmir|District Pulwama|Pulwama|Leh (ladakh)|Kulgam|Kulgam and Shupiyan|Badgam|Anantnag and Kulgam districts|Kulgam and Shupiyan|Anantnag|Koregaon Bhima, Perne village in Pune district|Kashmir Valley districts|Shupiyan and Pulwama districts|Kashmir Valley|Itanagar|Andhra Pradesh|Arunachal Pradesh|Assam|Bihar|Chhattisgarh|Delhi|Goa|Gujarat|Haryana|Himachal Pradesh|Jammu and Kashmir|Jammu & Kashmir|South Kashmir|Jharkhand|
Karnataka|Kerala|Madhya Pradesh|Maharashtra|Manipur|Meghalaya|Mizoram|Nagaland|Odisha|Punjab|Rajasthan|Sikkim|Tamil Nadu|Telangana|Tripura|Uttar Pradesh|Uttarakhand|West Bengal"

# loop through each string in the column of interest
for (i in 1:nrow(keepiton_india)) {
  # use grep function to check for matches with specified pattern
  if(grepl(match_search,
           keepiton_india$area_name[i])) {
    # if there is a match, add the matched value to the vector
    matched_values[i] <- str_extract(keepiton_india$area_name[i], match_search)
  } else {
    # if there is no match, add NA to the vector
    matched_values[i] <- NA
  }
}
# add the matched values vector as a new column to the scarped_data data frame
keepiton_india$state <- matched_values

# harmonize irregular names (districts) to actual province name
keepiton_india$state <- gsub("Jammu & Kashmir", "Jammu and Kashmir", keepiton_india$state)
keepiton_india$state <- gsub("South Kashmir", "Jammu and Kashmir", keepiton_india$state)
# other disctricts discovered earlier that belong to Jammu and Kashmir
keepiton_india$state <- gsub("Police district of Awantipora (Pulwama), in Jammu Kashmir|District Pulwama|Pulwama|Leh (ladakh)|Kulgam|Kulgam and Shupiyan|Badgam|Anantnag and Kulgam districts|Kulgam and Shupiyan|Anantnag|Koregaon Bhima, Perne village in Pune district|Kashmir Valley districts|Shupiyan and Pulwama districts|Kashmir Valley", "Jammu and Kashmir", keepiton_india$state) # CHECK
# district belonging to Arunachal Pradesh
keepiton_india$state <- gsub("Itanagar", "Arunachal Pradesh", keepiton_india$state)

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
keepiton_india$area_name <- gsub('Leh(ladakh)', 'Leh(Ladakh)', keepiton_india$area_name)
keepiton_india$area_name <- gsub('Shopian', 'Shupiyan', keepiton_india$area_name)
keepiton_india$area_name <- gsub('Budgam', 'Badgam', keepiton_india$area_name)

# remove white space 
keepiton_india$area_name <- gsub(" ", "",keepiton_india$area_name)

# for loop through area_name variable, separating districts into district variable 
matched_districts <- c()
district_search <- india_districts

# loop through each string in the column of interest
for (i in 1:nrow(keepiton_india)) {
  # use grep function to check for matches with specified pattern
  if(grepl(district_search,
           keepiton_india$area_name[i])) {
    # if there is a match, add the matched value to the vector
    matched_districts[i] <- str_extract_all(keepiton_india$area_name[i], district_search)
  } else {
    # if there is no match, add NA to the vector
    matched_districts[i] <- NA
  }
}
# add matched results to new varible
keepiton_india$districts <- matched_districts

#### Manual verification and cleaning of districts variable #### 

# change data format of keepiton_india$districts to chr
keepiton_india$districts <- as.character(keepiton_india$districts)

# removing unneeded vector notation 
keepiton_india$districts <- gsub('c\\("', "", keepiton_india$districts)
keepiton_india$districts <- gsub('")', "", keepiton_india$districts)
keepiton_india$districts <- gsub('"', "", keepiton_india$districts)

# removing all words related to state, i.e. 'Jammu', 'West' etc.
keepiton_india$districts <- gsub(', Jammu', "", keepiton_india$districts)
keepiton_india$districts <- gsub(', West', "", keepiton_india$districts)
keepiton_india$districts <- gsub('Jammu', NA, keepiton_india$districts)

# add states to NA
keepiton_india$state[239] <- "Jammu and Kashmir"
keepiton_india$state[242] <- "Jammu and Kashmir"
keepiton_india$state[264] <- "Jammu and Kashmir"

# manually adding districts where needed, referencing online searches and GADM ADM2 level list
keepiton_india$districts[7] <- "Itanagar"
keepiton_india$districts[13] <- "Anantnag, Kulgam, Pulwama, Shupiyan, Badgam, Srinagar, Ganderbal, Bandipore, Baramulla, Kupwara"
keepiton_india$districts[26] <- "Shupiyan"
keepiton_india$districts[27] <- "Maldah, Murshidabad, UttarDinajpur, KochBihar, Jalpaiguri, Birbhum, Darjiling"
keepiton_india$districts[30] <- "Shupiyan"
keepiton_india$districts[29] <- "Jhunjhunun"
keepiton_india$districts[34] <- "Kendujhar"
keepiton_india$districts[36] <- "Pulwama"
keepiton_india$districts[42] <- "Shupiyan"
keepiton_india$districts[46] <- "Shupiyan"
keepiton_india$districts[48] <- "Konaseema"
keepiton_india$districts[57] <- "Howrah"
keepiton_india$districts[68] <- "Ajmer, Alwar, Banswara, Baran, Barmer, Bharatpur, Bhilwara, Bikaner, Bundi, Chittorgarh, Churu, Dausa, Dhaulpur, Dungarpur, Hanumangarh, Jaipur, Jaisalmer, Jalor, Jhalawar, Jhunjhunun, Jodhpur, Karauli, Kota, Nagaur, Pali, Pratapgarh, Rajsamand, SawaiMadhopur, Sikar, Sirohi, Ganganagar, Tonk, Udaipur"
keepiton_india$districts[76] <- "Jammu, Rajouri"
keepiton_india$districts[80] <- "PashchimiSinghbhum"
keepiton_india$districts[88] <- "Anantnag, Badgam, Bandipore, Doda, Kargil, Kathua, Kishtwar, Kulgam, Kupwara, Leh(Ladakh), Poonch, Pulwama, Rajouri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_india$districts[89] <- "Anantnag, Baramulla, Badgam, Bandipore, Ganderbal, Kupwara, Kulgam, Pulwama, Shupiyan, Srinagar"
keepiton_india$districts[90] <- "East Delhi"
keepiton_india$districts[92] <- "Pulwama"
keepiton_india$districts[94] <- "West Delhi, East Delhi"
keepiton_india$districts[97] <- "North West Delhi, East Delhi, West Delhi"
keepiton_india$districts[98] <- "Shupiyan"
keepiton_india$districts[100] <- "Nirmal"
keepiton_india$districts[102:104] <- "Shupiyan"
keepiton_india$districts[107] <- "Shupiyan"
keepiton_india$districts[108] <- "Pulwama"
keepiton_india$districts[111] <- "Shupiyan"
keepiton_india$districts[114] <- "Shupiyan"
keepiton_india$districts[116] <- "Shupiyan"
keepiton_india$districts[124] <- "Anantnag, Kulgam, Pulwama, Shupiyan, Badgam, Srinagar, Ganderbal, Bandipore, Baramulla, Kupwara"
keepiton_india$districts[129] <- "Shupiyan"
keepiton_india$districts[131] <- "Baramulla"
keepiton_india$districts[132] <- "Bandipore"
keepiton_india$districts[133] <- "Anjaw, Changlang, DibangValley, EastKameng, EastSiang, KurungKumey, KurungKumey, Lohit, Longding, LowerDibangValley, WestSiang, LowerSubansiri, Namsai, PapumPare, Tawang, Tirap, UpperSiang, UpperSubansiri, WestKameng"
keepiton_india$districts[134] <- "Badgam"
keepiton_india$districts[135] <- "East Khasi Hills, West Khasi Hills, South West Khasi Hills, RiBhoi"
keepiton_india$districts[136] <- "Baramulla"
keepiton_india$districts[139] <- "Anantnag, Kulgam, Pulwama, Shupiyan, Badgam, Srinagar, Ganderbal, Bandipore, Baramulla, Kupwara"
keepiton_india$districts[144] <- "Kabeerdham, Rajnandgaon, Bemetara"
keepiton_india$districts[145] <- "Pulwama"
keepiton_india$districts[152] <- "Sawai Madhopur, Jaipur, Bharatpur, Hanumangarh, Ajmer, Bikaner"
keepiton_india$districts[178] <- "Shupiyan"
keepiton_india$districts[187] <- "Shupiyan"
keepiton_india$districts[191] <- "Anantnag, Kulgam, Pulwama, Shupiyan, Badgam, Srinagar, Ganderbal, Bandipore, Baramulla, Kupwara"
keepiton_india$districts[194] <- "North24Parganas"
keepiton_india$districts[197] <- "Anantnag, Kulgam, Pulwama, Shupiyan, Badgam, Srinagar, Ganderbal, Bandipore, Baramulla, Kupwara"
keepiton_india$districts[200:201] <- "Anantnag, Badgam, Bandipore, Baramulla, Doda, Ganderbal, Jammu, Kargil, Kathua, Kishtwar, Kulgam, Kupwara, Leh(Ladakh), Poonch, Pulwama, Rajouri, Ramban, Reasi, Samba, Shupiyan, Srinagar, Udhampur"
keepiton_india$districts[202] <- "Murshidabad, Maldah, DakshinDinajpur, PashchimMedinipur, Birbhum, Jalpaiguri"
keepiton_india$districts[206] <- "East Khasi Hills, RiBhoi, West Khasi Hills, Jaintia Hills"
keepiton_india$districts[207] <- "Kulgam, Shupiyan"
keepiton_india$districts[211] <- "Baramulla"
keepiton_india$districts[226] <- "Pulwama"
keepiton_india$districts[227] <- "Hugli"
keepiton_india$districts[228] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_india$districts[242] <- "Leh(Ladakh)"
keepiton_india$districts[248] <- "Baramulla"
keepiton_india$districts[256] <- "Baramulla"
keepiton_india$districts[266] <- "Srinagar"
keepiton_india$districts[266] <- "Srinagar"
keepiton_india$districts[274] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_india$districts[283] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_india$districts[284] <- "Pulwama"
keepiton_india$districts[286] <- "LowerSubansiri, UpperSubansiri, LowerDibangValley, Lohit, Tirap, Changlang, PapumPare, Tawang, EastKameng, WestKameng, EastSiang, WestSiang, UpperSiang"
keepiton_india$districts[290] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_india$districts[293] <- "Pulwama, Anantnag, Kulgam, Shupiyan"
keepiton_india$districts[294] <- "Pulwama, Anantnag, Kulgam, Shupiyan"

#### Creating new rows (obs) for district shutdown ####
# use separate_rows() to split keepiton_india$districts
keepiton_india <- keepiton_india %>%
  # Separate location values into rows
  separate_rows(districts, sep = ", ") 
# maunal check with temp object
temp_short_df <- keepiton_india %>% select(start_date, area_name, districts)
rm(temp_short_df)

#### Make map showing shutdowns by district, add descriptive stats ####

# count number of shutdown events in each district
event_count <- as.data.frame(table(keepiton_india$districts))
colnames(event_count) <- c("districts", "count")
event_count$districts <- as.vector(event_count$districts)
event_count$count<- as.numeric(event_count$count)

# change names to fit shape file naming convention
for (i in event_count$districts) {
  event_count$districts <- gsub(' ', '', event_count$districts)
}

# merge with shape file
india_data_viz <- left_join(india_shapes_disctricts, event_count, by = c("NAME_2" = "districts"))

# Plotting 
map_districts <- ggplot(india_data_viz) +
  geom_sf(aes(fill = count), size = 0.1) +
  scale_fill_viridis_c(direction = -1, na.value = "lightgrey", name = "Obs.") +
#  scale_fill_gradient(low = "", high = "white") +
  ggtitle("Internet Shutdown Count by District in India, 2020 to 2022") +
  theme_minimal()
# save as jpg
ggsave("districts_shutdown_map.jpg", plot = map_districts, width = 11, height = 10, dpi = 300)

# Histogram, showing most affected districts
# Order the districts by the number of shutdowns
event_count$districts <- reorder(event_count$districts, -event_count$count)

# Filter the top 20 districts with the most observations
top_20_districts <- event_count %>% top_n(20, count)

# Create a histogram with ggplot2
hist_top_districts <- ggplot(top_20_districts, aes(x = districts, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 20 Districts with Most Internet Shutdowns in India, 2020 to 2022",
       x = "District",
       y = "Number of Shutdowns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10))
# save as jpg
ggsave("districts_shutdown_hist.jpg", plot = hist_top_districts, width = 12, height = 7, dpi = 300)

#### Make map showing shutdowns by state, add descriptive stats ####

# count number of shutdown events in each state
event_count <- as.data.frame(table(keepiton_india$state))
colnames(event_count) <- c("state", "count")
event_count$state <- as.vector(event_count$state)
event_count$count<- as.numeric(event_count$count)

# change names to fit shape file naming convention
for (i in event_count$state) {
  event_count$state <- gsub(' ', '', event_count$state)
}
event_count$state <- gsub('Delhi', 'NCTofDelhi', event_count$state)
# merge with shape file
# load shape file for states
india_shapes_state <- st_read("/Users/jenskoning/Documents/R_Projects/internet_shutdowns_india/Shutdowns_data/gadm41_IND_1.json")
india_data_viz <- left_join(india_shapes_state, event_count, by = c("NAME_1" = "state"))

# Plotting 
map_states <- ggplot(india_data_viz) +
  geom_sf(aes(fill = count), size = 0.1) +
  scale_fill_viridis_c(direction = -1, na.value = "lightgrey", name = "Obs.") +
  #  scale_fill_gradient(low = "", high = "white") +
  ggtitle("Internet Shutdown Count by State in India, 2020 to 2022") +
  theme_minimal()
# save as jpg
ggsave("states_shutdown_map.jpg", plot = map_states, width = 11, height = 10, dpi = 300)

# Histogram, counts of shutdowns by state
# Reordering events by number of shutdowns
event_count$state <- reorder(event_count$state, -event_count$count)

# Plotting
hist_states <- ggplot(event_count, aes(x = state, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Internet Shutdown Count by State in India, 2020 to 2022",
       x = "State",
       y = "Number of Shutdowns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("states_shutdown_hist.jpg", plot = hist_states, width = 10, height = 7, dpi = 300)
# Plotting, but dropping Jammu and Kashmir
event_count_filtered <- subset(event_count, state != "JammuandKashmir")
# Order the remaining states by the number of shutdowns
event_count_filtered$state <- reorder(event_count_filtered$state, -event_count_filtered$count)
# Create a histogram with ggplot2
hist_states_noJK <- ggplot(event_count_filtered, aes(x = state, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Internet Shutdown Count by State in India (Excluding Jammu and Kashmir), 2020 to 2022",
       x = "State",
       y = "Number of Shutdowns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# save as jpg
ggsave("states_shutdown_hist_noJK.jpg", plot = hist_states_noJK, width = 12, height = 10, dpi = 300)

#### Export to .rds ####

# state level / all related to communal violence
district_event_shutdown <- keepiton_india
saveRDS(district_event_shutdown, file = "district_event_shutdown.rds")
district_event_shutdown_communal <- district_event_shutdown %>%
  filter(actual_cause == "Communal violence")
saveRDS(district_event_shutdown_communal, file = "district_event_shutdown_communal.rds")

# remove temp objects
rm(list=c("top_20_districts", "matched_districts", "event_count", "event_count_filtered", "india_data_viz", "india_shapes_disctricts", "india_shapes_state",
          "hist_states", "hist_states_noJK", "hist_top_districts", "map_districts", "map_states"))






