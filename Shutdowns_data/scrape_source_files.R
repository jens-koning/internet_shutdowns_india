####    \\    Save source files on internet shutdowns   //    ####

# Clear working environment and install needed packages
rm(list=ls(all = TRUE))

#### Install and load required packages/data ####
install.packages("rvest")
install.packages("httr")
install.packages("pdftools")

my_packages <- c("rvest", "httr", "pdftools", "dplyr")
lapply(my_packages, require, character.only = TRUE) 

# load needed data 
url <- readRDS("state_event_shutdown.rds") %>%
  select(info_source_link)
url <- as.vector(url$info_source_link)

#### Function to extract PDF or HTML content and save to a folder ####
save_content <- function(url, output_folder) {
  tryCatch({
    response <- GET(url)
    
    file_name <- basename(url)
    file_path <- file.path(output_folder, file_name)
    
    if (http_type(response) == "application/pdf") {
      # Save PDF to folder
      writeBin(content(response, "raw"), file_path)
    } else if (http_type(response) == "text/html") {
      # Save HTML content to folder
      html_content <- content(response, "text")
      write(html_text(read_html(html_content)), file_path)
    } else {
      return(NA)
    }
  }, error = function(e) {
    return(NA)
  })
}

# Create output folder if it doesn't exist
output_folder <- "/Users/jenskoning/Documents/R_Projects/Shutdowns_data/Sources"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Apply the function to the dataframe
url %>%
  mutate(saved = sapply(url, save_content, output_folder))
