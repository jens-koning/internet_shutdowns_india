####    \\    Clean India Shutdowns Data from the Access Now project, 2016 to 2018   //    ####

# Clear working environment and install needed packages
rm(list=ls(all = TRUE))

# Load needed packages
my_packages <- c("dplyr", "tidyr", "stringr", "readxl", "sf", "ggplot2", "viridis")
lapply(my_packages, require, character.only = TRUE) 

#### Load data and merge data sets ####
# Load other data source; access now global data base (CODEBOOK: https://www.accessnow.org/wp-content/uploads/2023/03/Read-Me_STOP_data_methodology.pdf)
# The data for India is collected by https://internetshutdowns.in 
keepiton_2016_18 <- read_excel("Keepiton_raw_dataset.xlsx",
                               sheet = "2016+2017+2018 ")

# Filter out Indian shutdowns
keepiton_2016_18 <- keepiton_2016_18 %>%
  filter(country == "India")

#### Create state variable for 2016 to 2018 data ####
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

#### Create district variable for 2016 to 2018 data ####

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
keepiton_2016_18$state[168:170] <- "Jammu and Kashmir"
keepiton_2016_18$state[192] <- "Gujarat"
keepiton_2016_18$state[201] <- "Jammu and Kashmir"
keepiton_2016_18$state[221] <- "Rajasthan"

# for loop for Kashmir Valley
for (i in 1:nrow(keepiton_2016_18)) {
  if (keepiton_2016_18$area_name[i] == "KashmirValley") {
    keepiton_2016_18$districts[i] <- "Anantnag, Kulgam, Pulwama, Shupiyan, Badgam, Srinagar, Ganderbal, Bandipore, Baramulla, Kupwara"
  }
}

# manual check of districts
keepiton_2016_18$districts[4] <- "Mahesana"
keepiton_2016_18$districts[5] <-"Ahmadabad, Amreli, Anand, Aravalli, BanasKantha, Bharuch, Bhavnagar, Botad, ChhotaUdaipur, Dahod, DevbhumiDwarka, Gandhinagar, GirSomnath, Jamnagar, Junagadh, Kachchh, Kheda, Mahesana, Mahisagar, Morbi, Narmada, Navsari, PanchMahals, Patan, Porbandar, Rajkot, SabarKantha, Surat, Surendranagar, Tapi, TheDangs, Vadodara, Valsad"
keepiton_2016_18$districts[6] <- "Rohtak, Jhajjar"
keepiton_2016_18$districts[12] <- "Jammu"
keepiton_2016_18$districts[14] <- "Jammu"
keepiton_2016_18$districts[18] <- "Jammu"
keepiton_2016_18$districts[20] <- "Anantnag, Badgam, Bandipore, Baramulla, Doda, Ganderbal, Jammu, Kargil, Kathua, Kishtwar, Kulgam, Kupwara, Leh(Ladakh), Poonch, Pulwama, Rajouri, Ramban, Reasi, Samba, Shupiyan, Srinagar, Udhampur"
keepiton_2016_18$districts[21] <- "Anantnag, Badgam, Bandipore, Baramulla, Doda, Ganderbal, Jammu, Kargil, Kathua, Kishtwar, Kulgam, Kupwara, Leh(Ladakh), Poonch, Pulwama, Rajouri, Ramban, Reasi, Samba, Shupiyan, Srinagar, Udhampur"
keepiton_2016_18$districts[25] <- "Gopalganj, Bhojpur, Madhubani, Purba Champaran, Madhepura, Kishanganj"
keepiton_2016_18$districts[28] <-"ImphalEast, ImphalWest"
keepiton_2016_18$districts[33] <- "Dimapur, Kiphire, Kohima, Longleng, Mokokchung, Mon, Peren, Phek, Tuensang, Wokha, Zunheboto"
keepiton_2016_18$districts[39] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_2016_18$districts[41] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_2016_18$districts[40] <- "Bhadrak"
keepiton_2016_18$districts[40] <- "Bhadrak"
keepiton_2016_18$districts[52] <- "Darjiling"
keepiton_2016_18$districts[55] <- "North24Parganas"
keepiton_2016_18$districts[57] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_2016_18$districts[61] <- "Dhalai, Gomati, Khowai, North Tripura, Sipahijala, South Tripura, Unokoti, West Tripura"
keepiton_2016_18$districts[63] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_2016_18$districts[66] <- "PapumPare"
keepiton_2016_18$districts[76] <- "Madhepura, Supaul, Saharsa, Araria, Kishanganj, Katihar"
keepiton_2016_18$districts[79] <- "Baramulla"
keepiton_2016_18$districts[81] <- "West Tripura"
keepiton_2016_18$districts[87] <- "South West Delhi" 
keepiton_2016_18$districts[104] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_2016_18$districts[106] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_2016_18$districts[115] <- "Bandipore"
keepiton_2016_18$districts[117] <- "Baramulla"
keepiton_2016_18$districts[126] <- "Barddhaman"
keepiton_2016_18$districts[130] <- "Pali"
keepiton_2016_18$districts[131] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_2016_18$districts[132] <-"Amritsar, Barnala, Bathinda, Faridkot, Fatehgarh Sahib, Fazilka, Firozpur, Gurdaspur, Hoshiarpur, Jalandhar, Kapurthala, Ludhiana, Mansa, Moga, Muktsar, Pathankot, Patiala, Rupnagar, Sahibzada Ajit Singh Nagar, Sangrur, Shahid Bhagat Singh Nagar, Tarn Taran"
keepiton_2016_18$districts[134] <- NA
keepiton_2016_18$districts[141] <- "Kapurthala, Jalandhar, Hoshiarpur, ShaheedBhagatSinghNagar"
keepiton_2016_18$districts[149] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_2016_18$districts[152] <- "Saharanpur"
keepiton_2016_18$districts[154] <- "Hardwar"
keepiton_2016_18$districts[160] <- "JaintiaHills"
keepiton_2016_18$districts[163] <- "EastGaroHills, NorthGaroHills, SouthGaroHills, SouthWestGaroHills"
keepiton_2016_18$districts[164:165] <- "KamrupMetropolitan"
keepiton_2016_18$districts[166] <- "JaintiaHills, EastKhasiHills, SouthWestKhasiHills, WestKhasiHills"
keepiton_2016_18$districts[174] <- "Anantnag, Badgam, Bandipore, Baramulla, Doda, Ganderbal, Jammu, Kargil, Kathua, Kishtwar, Kulgam, Kupwara, Leh(Ladakh), Poonch, Pulwama, Rajouri, Ramban, Reasi, Samba, Shupiyan, Srinagar, Udhampur"
keepiton_2016_18$districts[176] <- "Anantnag, Badgam, Bandipore, Baramulla, Doda, Ganderbal, Jammu, Kargil, Kathua, Kishtwar, Kulgam, Kupwara, Leh(Ladakh), Poonch, Pulwama, Rajouri, Ramban, Reasi, Samba, Shupiyan, Srinagar, Udhampur"
keepiton_2016_18$districts[177] <- "Ajmer, Alwar, Banswara, Baran, Barmer, Bharatpur, Bhilwara, Bikaner, Bundi, Chittaurgarh, Churu, Dausa, Dhaulpur, Dungarpur, Ganganagar, Hanumangarh, Jaipur, Jaisalmer, Jalor, Jhalawar, Jhunjhunun, Jodhpur, Karauli, Kota, Nagaur, Pali, Pratapgarh, Rajsamand, Sawai Madhopur, Sikar, Sirohi, Tonk, Udaipur"
keepiton_2016_18$districts[171] <- "Dhalai, Gomati, Khowai, North Tripura, Sipahijala, South Tripura, Unokoti, West Tripura"
keepiton_2016_18$districts[184] <- "Anjaw, Changlang, Dibang Valley, East Kameng, East Siang, East Siang, Kurung Kumey, Lohit, Lohit, Longding, Lower Dibang Valley, Lower Dibang Valley, Lower Subansiri, Namsai, Papum Pare, Tawang, Tirap, Upper Siang, Upper Subansiri."
keepiton_2016_18$districts[183] <- "Thane, Raigad"
keepiton_2016_18$districts[186] <- "Ajmer, Alwar, Dausa, Jhunjhunun, Sikar, Bharatpur, Jodhpur"
keepiton_2016_18$districts[187] <- "Pune, Ratnagiri"
keepiton_2016_18$districts[189] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_2016_18$districts[190] <- "Bandipore" 
keepiton_2016_18$districts[192] <- "Mahisagar"
keepiton_2016_18$districts[198] <- "Udaipur, Banswara"
keepiton_2016_18$districts[201] <- "Kupwara"
keepiton_2016_18$districts[204] <- "Bishnupur, Chandel, Churachandpur, Imphal East, Imphal West, Senapati, Tamenglong, Thoubal, Ukhrul"
keepiton_2016_18$districts[205] <- "Shupiyan, Pulwama"
keepiton_2016_18$districts[207:209] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_2016_18$districts[211] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_2016_18$districts[212] <- "Anantnag, Bandipore, Baramulla, Badgam, Doda, Jammu, Kathua, Kishtwar, Kulgam, Kupwara, Poonch, Pulwama, Rajauri, Ramban, Reasi, Samba, Shupiyan, Srinagar"
keepiton_2016_18$districts[221] <- "Pali"
keepiton_2016_18$districts[230] <- "Baramulla"

keepiton_2016_18$state <- gsub("Jammu and Kashmir and Jammu and Kashmir", "Jammu and Kashmir", keepiton_2016_18$state)
# check
temp_df <- keepiton_2016_18 %>% select(start_date, area_name, state, districts, news_link)
rm(temp_df)

#### Creating new rows (obs) for district shutdown ####
# use separate_rows() to split keepiton_2016_18$districts
keepiton_2016_18 <- keepiton_2016_18 %>%
  # Separate location values into rows
  separate_rows(districts, sep = ", ") 
# maunal check with temp object
temp_df <- keepiton_2016_18 %>% select(start_date, area_name, districts)
rm(temp_df)

#### Export to .rds ####

# state level / all related to communal violence
district_event_shutdown_2016_18 <- keepiton_2016_18
saveRDS(district_event_shutdown_2016_18, file = "district_event_shutdown_2016_18.rds")
district_event_shutdown_communal_2016_18 <- district_event_shutdown_2016_18 %>%
  filter(actual_cause == "Communal Violence")
saveRDS(district_event_shutdown_communal_2016_18, file = "district_event_shutdown_communal_2016_18.rds")

# remove temp objects
rm(list=c("district_event_shutdown_communal_2016_18", "india_shapes_disctricts", "district_event_shutdown_2016_18"))







