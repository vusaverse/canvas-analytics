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



## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
##' *INFO*
##' If the script has run before, read in the exported dataframe and keep only the non-processes files
##'

tryCatch({

  dfPPT <- read_file_proj("CAN_Powerpoint_text",
                          dir = "1. Ingelezen data/",
                          add_branch = TRUE,
                          base_dir = Sys.getenv("OUTPUT_DIR"),
                          extension = "rds")

  dfPPT_filled <- dfPPT %>%
    dplyr::filter(!is.na(extracted_text))

  dfPPT_NA <- dfPPT %>%
    dplyr::filter(is.na(extracted_text))

  # Read the input data
  dfFiles <- read_file_proj("CAN_Files",
                            dir = "1. Ingelezen data/",
                            add_branch = TRUE,
                            base_dir = Sys.getenv("OUTPUT_DIR"),
                            extension = "rds")

  df <- dfFiles %>%
    dplyr::filter(mime_class == "ppt",
                  !id %in% dfPPT_filled$id) %>%
    dplyr::filter(grepl("\\.pptx$", ignore.case = TRUE,  filename))

  cat("Processing only new or previously failed files.\n")
  ## cat the number of files to process
  cat("Number of files to process: ", nrow(df), "\n")
}, error = function(e) {
  # If read_file_proj throws an error, process all files
  # Read the input data
  dfFiles <- read_file_proj("CAN_Files",
                            dir = "1. Ingelezen data/",
                            add_branch = TRUE,
                            base_dir = Sys.getenv("OUTPUT_DIR"),
                            extension = "rds")
  df <- dfFiles %>%
    dplyr::filter(mime_class == "ppt") %>%
    dplyr::filter(grepl("\\.pptx$", ignore.case = TRUE,  filename))

  cat("Processing all PPT files.\n")
  ## cat the number of files to process
  cat("Number of files to process: ", nrow(df), "\n")
})


options(future.globals.maxSize = 1000 * 1024^2)  # 1GB

# Function to extract content from PPT
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

# Safely wrapped version of extract_ppt_content
safe_extract_ppt_content <- purrr::safely(extract_ppt_content)


# Main processing function
process_ppt_files <- function(df, batch_size = 50, num_workers = 4) {
  plan(multisession, workers = num_workers)

  url_table <- df %>%
    dplyr::filter(grepl("\\.pptx$", ignore.case = TRUE,  filename)) %>%
    sample_n(50000)

  total_files <- nrow(url_table)
  cat("Total files to process:", total_files, "\n")

  results <- list()

  for (i in seq(1, total_files, by = batch_size)) {
    batch_end <- min(i + batch_size - 1, total_files)
    cat("Processing batch", i, "to", batch_end, "\n")

    batch <- url_table[i:batch_end, ]
    batch_result <- batch %>%
      mutate(extraction_result = future_map(url, safe_extract_ppt_content, .progress = TRUE))

    results[[length(results) + 1]] <- batch_result
  }

  temp_dir <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/AppData/Local/Temp/") %>%
    unlink(recursive = TRUE)
  bind_rows(results)


}


# Process the files
result_table <- process_ppt_files(df, batch_size = 100, num_workers = parallel::detectCores() - 1)

# Post-process results
final_table <- result_table %>%
  mutate(
    extracted_text = unlist(extraction_result)) %>%
  dplyr::filter(!is.na(extracted_text)) %>%
  select(-extraction_result) %>%
  bind_rows(dfPPT_filled) %>%
  distinct()


prepare_and_send_summary(final_table,
                         dfPPT_filled,
                         nrow(df))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(final_table, "CAN_Powerpoint_text")

clear_script_objects()

