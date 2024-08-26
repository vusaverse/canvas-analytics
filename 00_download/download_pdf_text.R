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
library(pdftools)
library(furrr)
library(dplyr)
library(purrr)

# Set maximum file size (in bytes)
MAX_FILE_SIZE <- 10 * 1024 * 1024  # 50 MB

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

# Safely wrapped version of extract_pdf_content
safe_extract_pdf_content <- purrr::safely(extract_pdf_content)

options(future.globals.maxSize = 1000 * 1024^2)  # 1GB

tryCatch({
  dfPDF <- read_file_proj("CAN_PDF_text",
                          dir = "1. Ingelezen data/",
                          add_branch = TRUE,
                          base_dir = Sys.getenv("OUTPUT_DIR"),
                          extension = "rds")

  dfPDF_filled <- dfPDF %>%
    dplyr::filter(!is.na(extracted_text))

  dfPDF_NA <- dfPDF %>%
    dplyr::filter(is.na(extracted_text))

  # Read the input data
  dfFiles <- read_file_proj("CAN_Files",
                            dir = "1. Ingelezen data/",
                            add_branch = TRUE,
                            base_dir = Sys.getenv("OUTPUT_DIR"),
                            extension = "rds")

  df <- dfFiles %>%
    dplyr::filter(mime_class == "pdf",
                  !id %in% dfPDF_filled$id,
                  size <= MAX_FILE_SIZE)

  cat("Processing only new or previously failed files.\n")
  cat("Number of files to process: ", nrow(df), "\n")
}, error = function(e) {
  # If read_file_proj throws an error, process all files
  dfFiles <- readrds_csv(output = "20. Test/CAN_Files.rds")
  df <- dfFiles %>%
    dplyr::filter(mime_class == "pdf")

  cat("Processing all PDF files.\n")
})

# Main processing function
process_pdf_files <- function(df, batch_size = 50, num_workers = 4) {
  plan(multisession, workers = num_workers)

  url_table <- df %>%
    dplyr::filter(grepl("\\.pdf$", ignore.case = TRUE, filename))

  total_files <- nrow(url_table)
  cat("Total files to process:", total_files, "\n")

  results <- list()

  for (i in seq(1, total_files, by = batch_size)) {
    batch_end <- min(i + batch_size - 1, total_files)
    cat("Processing batch", i, "to", batch_end, "\n")

    batch <- url_table[i:batch_end, ]
    batch_result <- batch %>%
      mutate(extraction_result = future_map(url, safe_extract_pdf_content, .progress = TRUE))

    results[[length(results) + 1]] <- batch_result
  }

  bind_rows(results)
}

# Process the files
result_table <- process_pdf_files(df, batch_size = 100, num_workers = 4)

# Post-process results
final_table <- result_table %>%
  mutate(
    extracted_text = map_chr(extraction_result, ~ .x$result %||% NA_character_)) %>%
  dplyr::filter(!is.na(extracted_text)) %>%
  select(-extraction_result) %>%
  bind_rows(dfPDF_filled)

# Print summary
cat("Total files processed:", nrow(final_table), "\n")
cat("Successful extractions:", sum(!is.na(final_table$extracted_text)), "\n")
cat("Failed extractions:", sum(is.na(final_table$extracted_text)), "\n")



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(final_table, "CAN_PDF_text")

clear_script_objects()
