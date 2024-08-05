## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load necessary libraries
#library(officer)
library(pdftools)
library(furrr)
#library(dplyr)
#library(purrr)


# Function to extract content from PDF files
extract_pdf_content <- function(url) {
  tryCatch({
    temp_file <- tempfile(fileext = ".pdf")
    download.file(url, temp_file, mode = "wb", quiet = TRUE)

    # Extract text from PDF
    all_text <- pdf_text(temp_file) %>%
      paste(collapse = "\n\n")

    unlink(temp_file)
    return(all_text)
  }, error = function(e) {
    warning(paste("Error processing URL:", url, "-", e$message))
    return(NA_character_)
  })
}

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

dfFiles <- readrds_csv(output = "20. Test/CAN_Files.rds")

df <- dfFiles %>%
  dplyr::filter(mime_class == "pdf")


# Extract content based on file type
url_table <- df %>%
  mutate(extracted_text = future_map2_chr(url, mime_class, function(url, mime_class) {
    if (mime_class == "ppt") {
      extract_ppt_content(url)
    } else if (mime_class == "pdf") {
      extract_pdf_content(url)
    } else {
      NA_character_
    }
  }, .progress = TRUE))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vusa::write_file(url_table, "CAN_PDF_text", destination = "20. Test/", save_rds = TRUE)

clear_script_objects()
