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

dfCourses <- read_file_proj("CAN_Index2",
                            dir = "1. Ingelezen data/",
                            add_branch = TRUE,
                            base_dir = Sys.getenv("OUTPUT_DIR"),
                            extension = "rds")

tryCatch({
  # Read the previously processed data
  dfPages_filled <- read_file_proj("CAN_Pages",
                                           dir = "1. Ingelezen data/",
                                           add_branch = TRUE,
                                           base_dir = Sys.getenv("OUTPUT_DIR"),
                                           extension = "rds")

  df <- dfCourses %>%
    dplyr::filter(!id %in% dfPages_filled$course_id)

  cat("Number of courses to process: ", nrow(df), "\n")


}, error = function(e) {
  # If read_file_proj throws an error, process all files
  df <- dfCourses

  cat(paste0("Processing all courses.\n"))
  cat("Number of courses to process: ", nrow(df), "\n")

})

library(parallel)
library(furrr)

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)


get_course_pages <- function(canvas, course_id, per_page = 100) {
  # Construct the API endpoint URL
  url <- paste0(canvas$base_url, "/api/v1/courses/", course_id, "/pages?per_page=", per_page)

  # Make the API request
  response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", canvas$api_key)))

  # Check the response status code
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve course pages. Please check your authentication and API endpoint.")
  }

  # Parse the response as JSON
  pages <- httr::content(response, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  pages <- pages %>%
    dplyr::mutate(page_body = purrr::map_chr(.data$page_id, ~get_page_content(canvas, course_id, .x)))

  # Return the list of pages
  return(pages)
}



dfPages <- df %>%
  pull(id) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        result <- get_course_pages(canvas, .x)
        if (nrow(result) == 1 && all(names(result) == "course_id")) {
          # This means no pages were found
          tibble(course_id = .x, no_pages = TRUE)
        } else {
          result
        }
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)


if (exists("dfPages_filled")) {
  dfPages <- bind_rows(dfPages, dfPages_filled)
  dfPages <- dfPages %>%
    mutate(across(where(is.character), ~iconv(., to = "UTF-8"))) %>%
    distinct()
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfPages, "CAN_Pages")

clear_script_objects()
