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
  dfCourse_student_summaries_filled <- read_file_proj("CAN_Course_student_summaries",
                                   dir = "1. Ingelezen data/",
                                   add_branch = TRUE,
                                   base_dir = Sys.getenv("OUTPUT_DIR"),
                                   extension = "rds")

  # Check for errors in the existing data
  # error_courses <- dfCourse_student_summaries_filled %>%
  #   dplyr::filter(!is.na(error)) %>%
  #   pull(course_id)

  df <- dfCourses %>%
    dplyr::filter(!id %in% dfCourse_student_summaries_filled$course_id)


  cat("Number of courses to process: ", nrow(df), "\n")


}, error = function(e) {
  # If read_file_proj throws an error, process all files
  df <- dfCourses

  cat(paste0("Processing all courses.\n"))
  cat("Number of courses to process: ", nrow(df), "\n")

})





get_student_summaries <- function(canvas, course_id, per_page = 100) {
  # Construct the API endpoint URL
  url <- paste0(canvas$base_url, "/api/v1/courses/", course_id, "/analytics/student_summaries?per_page=", per_page)

  # Make the API request
  response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", canvas$api_key)))

  # Check the response status code
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve student summaries. Please check your authentication and API endpoint.")
  }

  # Parse the response as JSON
  student_summaries <- httr::content(response, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  # Add course_id to each row of the student summaries
  student_summaries$course_id <- course_id

  # Return the list of student summaries
  return(student_summaries)
}


library(parallel)
library(furrr)

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)



dfCourse_student_summaries <- df %>%
  pull(id) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        get_student_summaries(canvas, .x)
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)


if (exists("dfCourse_student_summaries_filled")) {
  dfCourse_student_summaries <- bind_rows(dfCourse_student_summaries, dfCourse_student_summaries_filled) %>%
    distinct()
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfCourse_student_summaries, "CAN_Course_student_summaries")

clear_script_objects()

