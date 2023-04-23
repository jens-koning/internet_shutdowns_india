####    \\    Clean Scraped Internet Data   //    ####

# Clear working environment and install needed packages
rm(list=ls(all = TRUE))
install.packages("lubridate")

# Load needed packages
my_packages <- c("dplyr", "tidyr", "stringr", "lubridate", "readxl", "sf", "ggplot2")
lapply(my_packages, require, character.only = TRUE) 

# Load other data sources; rds for 2021 and 2022 (tabular_ data.rds) and xlsx for 2020 (shutdowns_2020_dw.xlsx)
shutdowns_2020_dw <- read_excel("shutdowns_2020_dw.xlsx") # data from Deutche Welle
tabular_data <- readRDS("tabular_data.rds") # data from Waybackmachine for 2021 and 2022 

#### From text to data frame 2020 ####

# Set the path to the text file
file_path <- "/Users/jenskoning/Documents/R_Projects/internet_shutdowns_india/Shutdowns_data/shutdowns_india_2020_chatGPT.txt"

# Read in the text file as a character vector
file_lines <- readLines(file_path)

# Split each line of the text file by a specific delimiter (e.g., tab)
split_lines <- strsplit(file_lines, "\t")

# Convert the split lines into a tabular data frame
tabular_data_2020 <- data.frame(do.call(rbind, split_lines), stringsAsFactors = FALSE)
rm(split_lines)

# Set the column names of the tabular data
colnames(tabular_data_2020) <- "all"

# create empty data frames to hold the new columns
new_col1 <- data.frame()
new_col2 <- data.frame()
new_col3 <- data.frame()

# loop through every group of 3 rows in the original column
for(i in seq(1, nrow(tabular_data_2020), by = 3)) {
  # get the values of the current group of 3 rows
  values <- tabular_data_2020$all[i:(i+2)]
  # create a new data frame with the current values
  new_df <- data.frame(col1 = values[1], col2 = values[2], col3 = values[3])
  # add the new data frame to the corresponding new column
  new_col1 <- rbind(new_col1, new_df$col1)
  new_col2 <- rbind(new_col2, new_df$col2)
  new_col3 <- rbind(new_col3, new_df$col3)
}

# Merge columns
tabular_data_2020 <- data.frame(new_col1, new_col2, new_col3)
rm(new_col1, new_col2, new_col3, new_df)

# Rename columns
colnames(tabular_data_2020) <- c("date", "description", "location")

# Reverse order
tabular_data_2020 <- tabular_data_2020[order(-seq_len(nrow(tabular_data_2020))), ]

#### Clean and merge with data from 2021-22 ####
scraped_data <- bind_rows(tabular_data, tabular_data_2020)

# for loop through location variable, separating states from the overall location
matched_values <- c()
match_search <- "Andhra Pradesh|Arunachal Pradesh|Assam|Bihar|Chhattisgarh|Delhi|Goa|Gujarat|Haryana|Himachal Pradesh|Jammu and Kashmir|Jharkhand|
Karnataka|Kerala|Madhya Pradesh|Maharashtra|Manipur|Meghalaya|Mizoram|Nagaland|Odisha|Punjab|Rajasthan|Sikkim|Tamil Nadu|Telangana|Tripura|Uttar Pradesh|Uttarakhand|West Bengal"

# loop through each string in the column of interest
for (i in 1:nrow(scraped_data)) {
  # use grep function to check for matches with specified pattern
  if(grepl(match_search,
           scraped_data$location[i])) {
    # if there is a match, add the matched value to the vector
    matched_values[i] <- str_extract(scraped_data$location[i], match_search)
  } else {
    # if there is no match, add NA to the vector
    matched_values[i] <- NA
  }
}

# add the matched values vector as a new column to the scarped_data data frame
scraped_data$state <- matched_values

#### Adding sources to observations ####

# for loop searching for https in descriptions for adding to source column
http_values <- c()
http_search <- "https://\\S+"

for (i in 1:nrow(scraped_data)) {
  if(grepl(http_search,
            scraped_data$description[i])) {
    http_values[i] <- str_extract(scraped_data$description[i], http_search) 
  } else {
    http_values[i] <- NA 
  }
}

# add the matched http-values vector as a new column to the scraped_data data frame
scraped_data$source <- http_values

# add relevant URLs to scraped data from https://internetshutdowns.in
scraped_data[1:88, "source"] <- "https://internetshutdowns.in [Wayback Machine]"
scraped_data[97:105, "source"] <- "https://internetshutdowns.in [Wayback Machine]"

# add relevant events and URLs from internet search triangulated sources
scraped_data[106, "source"] <- "https://www.trtworld.com/asia/indian-farmers-undeterred-by-coldest-delhi-night-as-they-continue-sit-in-42871"
scraped_data[117, "description"] <- "State Government suspended internet services in Kotputli, Patwa, Shahpura, Viratnagar, Jamwa Ramgarh, Madhorajpura, Dudu, and Mozamabad for 24 hours in response to the Gurjar reservation protests."
scraped_data[117, "source"] <- "https://internetfreedom.in/rajasthan-govt-internet-shutdown-representation/"
scraped_data[117, "date"] <- "02-11-2020"

# removing sources where sources could not be found

# added events not mentioned by ChatGPT using manual internt search
scraped_data[nrow(scraped_data) + 1,] <- c("17-10-2020", "The State Government had suspended internet services in several districts of Rajasthan inresponse to the Gurjar reservation agitation.",
                                           "Rajasthan", "Rajasthan", "https://www.businessworld.in/article/Rajasthan-Internet-services-suspended-in-Bharatpur-in-wake-of-Gujjar-Mahapanchayat/17-10-2020-332469/")
scraped_data[nrow(scraped_data) + 1,] <- c("30-10-2020", "The State Government had suspended internet services in several districts of Rajasthan inresponse to the Gurjar reservation agitation.",
                                           "Rajasthan", "Rajasthan", "https://www.newsnationtv.com/states/rajasthan/gurjar-agitation-internet-service-stopped-in-karauli-and-bharatpur-close-watch-on-every-activity-163884.html")
scraped_data[nrow(scraped_data) + 1,] <- c("01-11-2020", "Internet services in fifteen districts of Arunachal Pradesh were temporarily suspended during the Arunachal Pradesh Public Service Combined Competitive (Prelims) Examination (APPSCCE) on 1 November 2020.",
                                           "Arunachal Pradesh", "Arunachal Pradesh", "https://freespeechcollectivedotin.files.wordpress.com/2022/09/sflc-petition-internet-shutdowns.pdf")
scraped_data[nrow(scraped_data) + 1,] <- c("01-02-2020", "Mobile internet services suspended by West Bengal government to prevent cheating in exams. Suspension was imposed on each day on which the Madhyamik Exam [10th Standard Board Exam] was conducted",
                                           "West Bengal", "West Bengal", "https://freespeechcollectivedotin.files.wordpress.com/2022/09/sflc-petition-internet-shutdowns.pdf")
scraped_data[nrow(scraped_data) + 1,] <- c("04-01-2020", "Protests against the Citizenship Amendment Act (CAA)",
                                           "Hyderabad", "Andhra Pradesh", "https://internetshutdowns.in/static-page/caa-protest/")
scraped_data[nrow(scraped_data) + 1,] <- c("31-01-2020", "Protests against the Citizenship Amendment Act (CAA)",
                                           "Jabalpur", "Madhya Pradesh", "https://internetshutdowns.in/static-page/caa-protest/")
scraped_data[nrow(scraped_data) + 1,] <- c("23-02-2020", "Protests against the Citizenship Amendment Act (CAA)",
                                           "Aligarh", "Uttar Pradesh", "https://internetshutdowns.in/static-page/caa-protest/")
scraped_data[nrow(scraped_data) + 1,] <- c("28-02-2020", "Protests against the Citizenship Amendment Act (CAA)",
                                           "East Khasi Hills, ri Bhoi, West Khasi Hills and Jaintia Hills", "Meghalaya", "https://internetshutdowns.in/static-page/caa-protest/")
scraped_data[nrow(scraped_data) + 1,] <- c("03-02-2021", "Farmers' protest against new farm law",
                                           "14 of 22 districts in Haryana", "Haryana", "https://edition.cnn.com/2021/02/01/asia/india-internet-cut-farmers-intl-hnk/index.html")
scraped_data[nrow(scraped_data) + 1,] <- c("16-05-2020", "Internet shutdown ordered recently in Hooghly following communal violence in the area.",
                                           "Hugli-Chinsurah", "West Bengal", "https://www.barandbench.com/news/litigation/hoogly-internet-shutdown-to-end-tomorrow-state-tells-calcutta-hc-court-asks-state-to-justify-shutdown-in-the-first-place-read-order")
scraped_data[nrow(scraped_data) + 1,] <- c("26-09-2020", "Internet was suspended in four tribal districts of southern Rajasthan following violent protests in Dungarpur district by tribal youth over teacher recruitment examination",
                                           "Udaipur, Pratapgarh, Banswara and Dungarpur districts", "Rajasthan", "https://www.hindustantimes.com/india-news/internet-suspended-section-144-imposed-in-four-rajasthan-districts-after-violent-protest/story-8gInP2Gx4W95e4nHCfq5YM.html")
scraped_data[nrow(scraped_data) + 1,] <- c("09-08-2020", "Internet and text messaging services have been suspended in 14 districts of Uttar Pradesh, including Lucknow, following Thursday’s large-scale violence as protests against the Citizenship Amendment Act (CAA) spun out of control in these areas.",
                                           "Lucknow, Saharanpur, Meerut, Shamli, Muzzaffarnagar, Ghaziabad, Bareilly, Mau, Sambhal, Azamgarh, Agra, Kanpur , Unnao and Moradabad.", "Uttar Pradesh", "https://www.hindustantimes.com/india-news/no-internet-in-10-up-districts-including-lucknow-after-violent-caa-protests/story-KR2GoHodCTbgbqLAdLqaGJ.html")
scraped_data[nrow(scraped_data) + 1,] <- c("08-12-2020", "In its order the government stated its fear that some “anti-national elements” might disrupt elections.",
                                           "Awantipora, Shopian, Kulgam, Anantnag and Pulwama.", "Jammu and Kashmir", "https://article-14.com/post/140-changing-reasons-for-j-k-s-high-speed-internet-ban")
scraped_data[nrow(scraped_data) + 1,] <- c("03-04-2020", "Order: The ruling Bharatiya Janata Party feared its controversial new domicile law, introduced in March, might trigger protests. The home ministry’s order claimed that in the past, many instances of the misuse of data had been noticed",
                                           "Jammu and Kashmir", "Jammu and Kashmir", "https://article-14.com/post/140-changing-reasons-for-j-k-s-high-speed-internet-ban")
scraped_data[nrow(scraped_data) + 1,] <- c("27-05-2020", "Order said that “reports suggest rise in infiltration of terrorists during the coming weeks due to the onset of summer and melting of snow, which gets facilitated through use of Voice on Internet Protocol and encrypted mobile communication, 
                                           being used by the operatives/ anti-national elements to communicate with their handlers from across the border.",
                                           "Jammu and Kashmir", "Jammu and Kashmir", "https://article-14.com/post/140-changing-reasons-for-j-k-s-high-speed-internet-ban")
scraped_data[nrow(scraped_data) + 1,] <- c("12-11-2020", "Order: Ahead of the 28 November District Development Council (DDC) elections across the newly-formed UT—the first such political exercise since the abrogation of Article 370—the government said a “the high level of interest” in the elections made it likely that “terrorist and separatist elements shall make every possible attempt to disrupt the democratic process”",
                                           "Jammu and Kashmir", "Jammu and Kashmir", "https://article-14.com/post/140-changing-reasons-for-j-k-s-high-speed-internet-ban")
scraped_data[nrow(scraped_data) + 1,] <- c("26-11-2020", "Order: Weeks after saying that 4G Internet can play spoilsport in conducting DDC elections, the government issued another order stating that the elections had resulted in “intense political activity” with “extensive campaigning” by candidates.",
                                           "Jammu and Kashmir", "Jammu and Kashmir", "https://article-14.com/post/140-changing-reasons-for-j-k-s-high-speed-internet-ban")
scraped_data[nrow(scraped_data) + 1,] <- c("07-12-2020", "Government shut down 2G internet services in South Kashmir during the fourth phase of polling of the District Development Council elections.",
                                           "South Kashmir", "Jammu and Kashmir", "https://www.outlookindia.com/website/story/india-news-on-polling-day-govt-snaps-even-2g-internet-services-in-south-kashmir/366519")
scraped_data[nrow(scraped_data) + 1,] <- c("11-12-2020", "Order: “Due to the likelihood of the misuse of data services by anti-national elements to disrupt the democratic process by creating a scare among the voters, carrying out attacks on security forces, targeting of contesting candidates and workers,” the order said.",
                                           "Jammu and Kashmir", "Jammu and Kashmir", "https://article-14.com/post/140-changing-reasons-for-j-k-s-high-speed-internet-ban")
scraped_data[nrow(scraped_data) + 1,] <- c("07-05-2020", "Internet suspension in an attempt to quell farmers' protest",
                                           "Punjab", "Punjab", "https://www.indiatoday.in/india/story/india-internet-shutdowns-looking-beyond-j-k-rajasthan-new-hotbed-1969664-2022-07-03") 
scraped_data[nrow(scraped_data) + 1,] <- c("06-05-2020", "Internet suspension in an attempt to quell farmers' protest",
                                           "Haryana", "Haryana", "https://www.indiatoday.in/india/story/india-internet-shutdowns-looking-beyond-j-k-rajasthan-new-hotbed-1969664-2022-07-03")
scraped_data[nrow(scraped_data) + 1,] <- c("06-05-2020", "On Wednesday, May 6, authorities suspended mobile internet service in Kashmir amid clashes with militants in Awantipora and Khrew.",
                                           "Jammu and Kashmir", "Jammu and Kashmir", "https://crisis24.garda.com/alerts/2020/05/india-mobile-internet-suspended-in-kashmir-amidst-clashes-with-militants-may-6")
scraped_data[nrow(scraped_data) + 1,] <- c("08-11-2020", "Protests by the Gujjar community demanding 5% quota in MBC category for ongoing recruitments and backlog vacancies, and reservation in jobs and education. Internet suspended to maintain law and order, and public safety keeping in mind communal tensions and spread of rumours by anti-social elements on Facebook and WhatsApp.",
                                           "Naranpur, Thanagazi, Rajgarh,Mala Khera, Siliserh, Umrain, Bakhat Pura, Seeradas, Dadhikar ,Hajipur, Shahpur, Dehlawas, Dausa", "Rajasthan", "https://internetfreedom.in/revealed-jaipur-internet-shutdown-orders-cut-copy-paste-keep-it-on/")
scraped_data[nrow(scraped_data) + 1,] <- c("30-10-2020", "Protests by the Gujjar community demanding 5% quota in MBC category for ongoing recruitments and backlog vacancies, and reservation in jobs and education. Internet suspended to maintain law and order, and public safety keeping in mind communal tensions and spread of rumours by anti-social elements on Facebook and WhatsApp.",
                                           "Kotputli, Pawata, Shahpura,Viratnagar, Jamwa Ramgarh, Dausa, ", "Rajasthan", "https://internetfreedom.in/revealed-jaipur-internet-shutdown-orders-cut-copy-paste-keep-it-on/")
scraped_data[nrow(scraped_data) + 1,] <- c("13-01-2020", "The suspension orders came from the Department of Telecommunications (DoT) after clashes broke out between two communities on, January 12, in Bhainsa town of Nirmal district.",
                                           "Nirmal, Adilabad, Mancherial and Asifabad", "Telangana", "https://www.medianama.com/2020/01/223-internet-shutdown-parts-of-telangana/")
scraped_data[nrow(scraped_data) + 1,] <- c("22-05-2020", "Cyclone Amphan causes temporary internet outage",
                                           "West Bengal", "West Bengal", "https://kuenselonline.com/cyclone-amphan-disrupts-internet-connectivity/")
scraped_data[nrow(scraped_data) + 1,] <- c("03-06-2020", "Cyclone Nisarga causes temporary internet outage",
                                           "Odisha", "Odisha", "https://www.livemint.com/technology/tech-reviews/how-to-chat-if-cyclone-nisarga-knocks-out-your-internet-11591173903800.html")
scraped_data[nrow(scraped_data) + 1,] <- c("03-06-2020", "Cyclone Nisarga causes temporary internet outage",
                                           "West Bengal", "West Bengal", "https://www.livemint.com/technology/tech-reviews/how-to-chat-if-cyclone-nisarga-knocks-out-your-internet-11591173903800.html")

#### Remove GPT 'hallucinations' from data frame ####
scraped_data <- scraped_data %>% slice(-(107:116))
scraped_data <- scraped_data %>% slice(-(108:139))
scraped_data <- scraped_data %>% slice(-(89:90))
scraped_data <- scraped_data %>% slice(-(90:92))

#### Clean and merge data from Deutsche Welle (DW) to df ####
# change date to character
shutdowns_2020_dw$date <- as.character(shutdowns_2020_dw$date)

# for loop removing <p>|</p>|<br> expression from "description" string
to_delete <- "<p>|</p>|<br>"

for (i in 1:7) { 
  shutdowns_2020_dw$description[i] <- gsub(to_delete, "", shutdowns_2020_dw$description[i])  
} # CHECK

# join DW data with scraped_data
scraped_data <- full_join(scraped_data, shutdowns_2020_dw)

#### Make dates similar, and add year and week variable ####

# addding year variable
scraped_data$year[1:58] <- 2022 # CHECK
scraped_data$year[59:102] <- 2021
scraped_data$year[103:156] <- 2020

# converting raw dates to same year-mm-dd format
converted_date_temp1 <-  as.Date(scraped_data$date, format = "%b. %d, %Y")
converted_date_temp2 <- as.Date(scraped_data$date, format = "%b %d, %Y")
converted_date_temp2 <- as

# reorder date columns

#### Make maps showing yearly events using sf #### 

# load shape file
india_shapes <- st_read("/Users/jenskoning/Documents/R_Projects/internet_shutdowns_india/Shutdowns_data/gadm41_IND_1.json")

# count number of shutdown events in each state
event_count <- as.data.frame(table(scraped_data$state))
colnames(event_count) <- c("state", "count")
event_count$state <- as.vector(event_count$state)
event_count$count<- as.numeric(event_count$count)

# change names to fit shape file naming convention
for (i in event_count$state) {
  event_count$state <- gsub(' ', '', event_count$state)
}
event_count$state <- gsub('Delhi', 'NCTofDelhi', event_count$state)

# merge with shape file
india_data <- left_join(india_shapes, event_count, by = c("NAME_1" = "state"))

# Plotting
ggplot(india_data) +
  geom_sf(aes(fill = count)) +
  scale_fill_gradient(low = "white", high = "red") +
  ggtitle("Shutdown Count by State in India 2020 to 2022") +
  theme_minimal()

# remove temp objects
rm(list=c("india_shapes", "shutdowns_2020_dw", "tabular_data", "tabular_data_2020"))









