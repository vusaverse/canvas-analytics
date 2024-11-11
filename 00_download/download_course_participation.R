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
  Course_participation_filled <- read_file_proj("CAN_Course_participation",
                                            dir = "1. Ingelezen data/",
                                            add_branch = TRUE,
                                            base_dir = Sys.getenv("OUTPUT_DIR"),
                                            extension = "rds")


  # Check for errors in the existing data
  # error_courses <- Course_participation_filled %>%
  #   dplyr::filter(!is.na(error)) %>%
  #   pull(course_id)

  df <- dfCourses %>%
    dplyr::filter(!id %in% Course_participation_filled$course_id)

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


get_course_participation <- function(canvas, course_id) {
  # Construct the API endpoint URL
  url <- paste0(canvas$base_url, "/api/v1/courses/", course_id, "/analytics/activity")

  # Make the API request
  response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", canvas$api_key)))

  # Check the response status code
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve course participation data. Please check your authentication and API endpoint.")
  }

  # Parse the response as JSON
  participation_data <- httr::content(response, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  # Add course_id to the participation data
  participation_data$course_id <- course_id

  # Return the participation data
  return(participation_data)
}


dfCourse_participation <- df %>%
  pull(id) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        get_course_participation(canvas, .x)
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)


if (exists("Course_participation_filled")) {
  dfCourse_participation <- bind_rows(dfCourse_participation, Course_participation_filled) %>%
    distinct()
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfCourse_participation, "CAN_Course_participation")

clear_script_objects()


cat("finished, now student summaries")
source("00_download/download_course_student_summaries.R")
