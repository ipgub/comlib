# Common libraries and functions/modules used for Gradings Notebook workflow
# Written in R

#######################################################################################
#
# Libraries
#
required_packages <- c("googledrive", "dplyr", "ggplot2", "readr","readxl", "googlesheets4", "WriteXLS", "lubridate")
for(pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    suppressPackageStartupMessages( library(pkg, character.only = TRUE) )
  }
}

#######################################################################################
#
# Authentication
#
drive_auth()

#######################################################################################
#
# Modules/functions
#

#######################################################################################
# Get curret time (WIB/Bangkok time zone)
get_current_time <- function(){
  # Get current system time
  current_time <- Sys.time()
  # Convert to Bangkok time zone (Asia/Bangkok)
  bangkok_time <- with_tz(current_time, "Asia/Bangkok")
  # Format the time to show only date and hour:minute:second
  formatted_time <- format(bangkok_time, "%Y-%m-%d %H:%M:%S")
  # Print the formatted time
  return(formatted_time)
}


#######################################################################################
# Extract file ID from a google drive file given its URL
extract_file_id <- function(url) {
  # Pattern untuk berbagai format URL Google Drive
  patterns <- c(
    "drive\\.google\\.com/file/d/([^/]+)",           # Format: /file/d/[ID]/
    "drive\\.google\\.com/open\\?id=([^/&]+)",       # Format: ?id=[ID]
    "drive\\.google\\.com/uc\\?id=([^/&]+)",         # Format: uc?id=[ID]
    "docs\\.google\\.com/[^/]+/d/([^/]+)"            # Format: /d/[ID]
  )
  # Coba setiap pattern
  for (pattern in patterns) {
    matched <- regexpr(pattern, url, perl = TRUE)
    if (matched != -1) {
      # Ekstrak group pertama (File ID)
      id <- regmatches(url, regexec(pattern, url, perl = TRUE))[[1]][2]
      return(id)
    }
  }
  # Jika tidak ada pattern yang cocok
  return(NULL)
}

#######################################################################################
# Extract Goolge drive folder ID from Google Drive URL
extract_folder_id <- function(drive_url){
  # Use a regular expression to extract the folder ID
  folder_id <- sub(".*folders/([^/?]+).*", "\\1", drive_url)
  return(folder_id)
}

#######################################################################################
# Reading data from excel / csv file saved in Google drive, given its URL.
#
# prompt: memanggil function read_excel jika type adalah xls atau read_csv jika type adalah csv
#
read_data <- function(url, ftype){
  file_ID <- extract_file_id(url)
  temp_file_path <- tempfile(fileext = paste0(".", ftype))
  drive_download(as_id(file_ID), path = temp_file_path, overwrite = TRUE)

  if (ftype == "xls" || ftype == "xlsx") {
    data <- read_excel(temp_file_path)
  } else if (ftype == "csv") {
    data <- read_csv(temp_file_path)
  } else {
    stop("Unsupported file type. Please provide 'xls', 'xlsx', or 'csv'.")
  }

  unlink(temp_file_path)
  return(data)
}

#######################################################################################
# prompt: create empty dataframe to store all the final exam calculation detail
final_exam_grading <- data.frame()

#######################################################################################
# Create a mapping of descriptions to numerical weights
grading_weights <- c(
  "Full marks. For correct method, correct calculation steps, and correct final answer" = 1.0,
  "Partial marks. For correct method, but incorrect for either calculation steps or final answer" = 0.9,
  "Partial marks. For correct method, but incorrect both calculation steps and final answer" = 0.8,
  "Partial marks. For partially correct method" = 0.7,
  "Half marks. For incorrect method/answers" = 0.5,
  "Lower half marks. For totally unrelated answers/methods or only quoting problems" = 0.35,
  "Null marks. For totally empty answer" = 0.0
)

