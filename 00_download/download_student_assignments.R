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


dfStudents <- read_file_proj("CAN_Students",
                            dir = "1. Ingelezen data/",
                            add_branch = TRUE,
                            base_dir = Sys.getenv("OUTPUT_DIR"),
                            extension = "rds")


tryCatch({
  # Read the previously processed data
  dfStudent_assignment_data_filled <- read_file_proj("CAN_Student_assignment_data",
                                   dir = "1. Ingelezen data/",
                                   add_branch = TRUE,
                                   base_dir = Sys.getenv("OUTPUT_DIR"),
                                   extension = "rds")

  df <- dfStudents %>%
    anti_join(dfStudent_assignment_data_filled, by = c("course_id", "id" = "student_id"))


  cat("Number of courses to process: ", nrow(df), "\n")


}, error = function(e) {
  # If read_file_proj throws an error, process all files
  df <- dfStudents

  cat(paste0("Processing all courses.\n"))
  cat("Number of courses to process: ", nrow(df), "\n")

})


get_user_course_assignment_data2 <- function(canvas, course_id, student_id) {
  # Construct the API endpoint URL
  url <- paste0(canvas$base_url, "/api/v1/courses/", course_id, "/analytics/users/", student_id, "/assignments")

  # Make the API request
  response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", canvas$api_key)))

  # Check the response status code
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve user-in-a-course-level assignment data. Please check your authentication and API endpoint.")
  }

  # Parse the response as JSON
  assignment_data <- httr::content(response, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  if (!is.list(assignment_data)) {
    assignment_data <- assignment_data %>%
    mutate(student_id = student_id)
  }

  # Return the assignment data
  return(assignment_data)
}


library(parallel)
library(furrr)

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

dfStudent_assignment_data <- df %>%
  dplyr::filter(name != "Test student") %>%
  select(course_id, id) %>%
  distinct() %>%
  future_pmap_dfr(function(course_id, id) {
    tryCatch(
      {
        get_user_course_assignment_data2(canvas, course_id, id)
      },
      error = function(e) {
        tibble(course_id = course_id, student_id = id, error = as.character(e))
      }
    )
  }, .progress = TRUE)


if (exists("dfStudent_assignment_data_filled")) {
  dfStudent_assignment_data <- bind_rows(dfStudent_assignment_data, dfStudent_assignment_data_filled) %>%
    distinct()
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfStudent_assignment_data, "CAN_Student_assignment_data")

clear_script_objects()
