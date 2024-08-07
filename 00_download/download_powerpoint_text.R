## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) Note: works only for ".pptx$" files as of now
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dfFiles <- readrds_csv(output = "20. Test/CAN_Files.rds")

df <- dfFiles %>%
  dplyr::filter(mime_class == "ppt")

install.packages("officer")
library(officer)


extract_ppt_content <- function(url) {
  tryCatch({
    temp_file <- tempfile(fileext = ".pptx")
    download.file(url, temp_file, mode = "wb", quiet = TRUE)

    ppt <- read_pptx(temp_file)

    # Get the number of slides
    slide_count <- length(ppt)

    # Extract text from all slides
    all_text <- map_chr(1:slide_count, function(i) {
      ppt %>%
        slide_summary(i) %>%
        pull(text) %>%
        paste(collapse = " ")
    }) %>%
      paste(collapse = "\n\n")

    unlink(temp_file)
    return(all_text)
  }, error = function(e) {
    warning(paste("Error processing URL:", url, "-", e$message))
    return(NA_character_)
  })
}

library(parallel)
library(furrr)

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

url_table <- df %>%
  dplyr::filter(grepl(".pptx$", filename)) %>%
  mutate(extracted_text = future_map_chr(url, extract_ppt_content, .progress = TRUE))

vusa::write_file(url_table, "CAN_Powerpoint_text", destination = "20. Test/", save_rds = TRUE)

