####    \\    Merge data sets into unified district-event time series  //    ####

# Clear working environment and install needed packages
rm(list=ls(all = TRUE))

# Load needed packages and data
my_packages <- c("dplyr", "tidyr", "lubridate", "sf", "ggplot2", "viridis")
lapply(my_packages, require, character.only = TRUE) 

# Loading data
rds_files <- c("district_event_shutdown_2019.rds", "district_event_shutdown_2016_18.rds", "district_event_shutdown_2020_22.rds")
object_names <- c("shutdown_2019", "shutdown_2016_18", "shutdown_2020_22")
# Use a for loop to read each RDS file and assign the data to separate objects
for (i in 1:length(rds_files)) {
  data_temp <- readRDS(rds_files[i])
  assign(object_names[i], data_temp)
  rm(data_temp)
}

#### Shorten and merge data sets ####
shorten_data <- function(data) {
  column_names <- c("start_date", "end_date", "duration", "duration_days", "duration_hours", "actual_cause", "country", "ordered_by", "area_name", "state", "districts", "gov_justification", "official_just", "event",
           "affected_network", "news_link", "info_source_link", "gov_ack_source")
  data %>%
    select_if(names(.) %in% column_names) %>%
    mutate(
      start_date = as.Date(start_date, format = "%Y/%m/%d"),
      end_date = as.Date(end_date, format = "%Y/%m/%d")
    ) %>%
    {if ("duration" %in% colnames(.)) mutate(., duration = purrr::map_dbl(duration, ~if (is.numeric(.x)) as.numeric(.x) else NA_real_)) else .}
}

# Initializing an empty data frame to store the merged data
shutdowns_ts <- data.frame()

# Use a for loop to apply the function to each data set and merge them
for (data_set in list(shutdown_2019, shutdown_2016_18, shutdown_2020_22)) {
  shutdowns_ts <- bind_rows(shutdowns_ts, shorten_data(data_set))
}
rm(data_set)

# Make duration same format 
shutdowns_ts$duration_days <- as.numeric(shutdowns_ts$duration_days)
shutdowns_ts$duration <- as.numeric(shutdowns_ts$duration)

#### Merge columns with similar values and change names ####
shutdowns_ts <- shutdowns_ts %>%
  mutate(
    justification = coalesce(gov_justification, official_just),
    source_link = coalesce(news_link, info_source_link),
    duration_day = coalesce(duration_days, duration)
  ) %>%
  select(-gov_justification, -official_just, -news_link, -info_source_link, -duration, -duration_days)

# Reordering columns and changing names 
colnames(shutdowns_ts)
shutdowns_ts <- shutdowns_ts[, c(1,2,15,11,3,9,10,8,4,5,13,6,7,14,12)]
shutdowns_ts <- shutdowns_ts %>%
  rename(
    gov_justification = justification,
    duration_days = duration_day,
    area_name_string = area_name
  )

#### Adding duration_days and _hours where missing ####
shutdowns_ts <- shutdowns_ts %>%
  mutate(duration_days = ifelse(is.na(duration_days), as.numeric(difftime(end_date, start_date, units = "days")), duration_days),
         duration_hours = ifelse(is.na(duration_hours), as.numeric(difftime(end_date, start_date, units = "hours")), duration_hours))

#### Clean relevant values ####
shutdowns_ts$actual_cause <- gsub("Communal violence", "Communal Violence" ,shutdowns_ts$actual_cause)

#### Map  ####

# load list of all Indian districts and states from GADM
india_shapes_disctricts <- st_read("/Users/jenskoning/Documents/R_Projects/internet_shutdowns_india/Shutdowns_data/gadm41_IND_2.json")
india_shapes_state <- st_read("/Users/jenskoning/Documents/R_Projects/internet_shutdowns_india/Shutdowns_data/gadm41_IND_1.json")

# count number of shutdown events in each district
event_count <- as.data.frame(table(shutdowns_ts$districts))
colnames(event_count) <- c("districts", "count")
event_count$districts <- as.vector(event_count$districts)
event_count$count<- as.numeric(event_count$count)

# change names to fit shape file naming convention
for (i in event_count$districts) {
  event_count$districts <- gsub(' ', '', event_count$districts)
}

# merge with shape file
india_data_viz <- left_join(india_shapes_disctricts, event_count, by = c("NAME_2" = "districts"))

# Plotting, base-10 logarithmic legend
map_districts_log <- ggplot(india_data_viz) +
  geom_sf(aes(fill = count), size = 0.1) +
  scale_fill_viridis_c(direction = -1, na.value = "lightgrey", name = "Count", trans = "log10") +
  ggtitle("Internet Shutdown Count by District in India, 2016 to 2022") +
  theme_minimal()
# save map as png
ggsave("districts_shutdown_map_full_ts_log.jpg", plot = map_districts_log, width = 11, height = 10, dpi = 300)

#### Histograms ####
# Histogram featuring state-event-count, with and without Jammu and Kashmir
# count number of shutdown events in each state
event_count <- as.data.frame(table(shutdowns_ts$state))
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
india_data_viz <- left_join(india_shapes_state, event_count, by = c("NAME_1" = "state"))
# Reordering events by number of shutdowns
event_count$state <- reorder(event_count$state, -event_count$count)


# Plotting 
map_states <- ggplot(india_data_viz) +
  geom_sf(aes(fill = count), size = 0.1) +
  scale_fill_viridis_c(direction = -1, na.value = "lightgrey",  name = "Count", trans = "log10") +
  #  scale_fill_gradient(low = "", high = "white") +
  ggtitle("Internet Shutdown Count by State in India, 2016 to 2022") +
  theme_minimal()
# save as jpg
ggsave("states_shutdown_map.jpg", plot = map_states, width = 11, height = 10, dpi = 300)

# Histogram, counts of shutdowns by state
# Reordering events by number of shutdowns
event_count$state <- reorder(event_count$state, -event_count$count)

# Plotting
hist_states <- ggplot(event_count, aes(x = state, y = count)) +
  geom_bar(stat = "identity", fill = "#20B252") +
  labs(title = "Internet Shutdown Count by State in India, 2016 to 2022",
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
  geom_bar(stat = "identity", fill = "#20B252") +
  labs(title = "Internet Shutdown Count by State in India (Excluding Jammu and Kashmir), 2016 to 2022",
       x = "State",
       y = "Number of Shutdowns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# save as jpg
ggsave("states_shutdown_hist_noJK.jpg", plot = hist_states_noJK, width = 12, height = 10, dpi = 300)

#### Time Series ####
# Aggregate data by start_date
event_count_daily <- shutdowns_ts %>%
  group_by(start_date) %>%
  summarize(count = n())

# Time series plot based on start_date
time_series_plot <- ggplot(event_count_daily, aes(x = start_date, y = count)) +
  geom_bar(stat = "identity", fill = "#20B252", width = 6) + 
  labs(title = "Internet Shutdown Count by Start Date in India, 2016 to 2022",
       x = "Start Date",
       y = "Number of Shutdowns") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%b") + # Add quarter indicators on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
# Save the plot as a .jpg file
ggsave("time_series_plot.jpg", plot = time_series_plot, width = 12, height = 7, dpi = 300)

time_series_plot <- ggplot(event_count_daily, aes(x = start_date, y = count, fill = count)) +
  geom_bar(stat = "identity", width = 7) +
  labs(title = "Internet Shutdown Count by Start Date in India, 2016 to 2022",
       x = "Start Date of Shutdown",
       y = "Number of Shutdowns") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%b") +
  scale_fill_viridis(direction = 1, option = "D", name = "Count", trans = "log10") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot as a .jpg file
ggsave("time_series_plot_viridis.jpg", plot = time_series_plot, width = 12, height = 7, dpi = 300)

#### Export to .rds ####
# state level / all related to communal violence
shutdowns_india_2016_22 <- shutdowns_ts
saveRDS(shutdowns_india_2016_22, file = "shutdowns_india_2016_22.rds")

# remove temp objects 
rm(list=c("event_count", "event_count_daily", "event_count_filtered", "hist_states", "hist_states_noJK", "india_data_viz",
          "india_shapes_disctricts", "india_shapes_state", "map_districts_log", "map_states", "shutdown_2016_18", "shutdown_2019",
          "shutdown_2020_22", "shutdowns_ts", "time_series_plot"))

