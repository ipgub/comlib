# Common libraries and functions/modules used for Gradings Notebook workflow
# Written in R

#######################################################################################
#
# Libraries
#
required_packages <- c("googledrive", "dplyr", "ggplot2", "readr","readxl")
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

# Extract file ID from a google drive file given its URL
extract_drive_id <- function(url) {
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


# Reading data from excel / csv file saved in Google drive, given its URL.
#
# prompt: memanggil function read_excel jika type adalah xls atau read_csv jika type adalah csv
#
read_data <- function(url, ftype){
  file_ID <- extract_drive_id(url)
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

