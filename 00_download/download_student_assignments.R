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


dfStudents <- read_file_proj("CAN_Enrolments",
                            dir = "1. Ingelezen data/",
                            add_branch = TRUE,
                            base_dir = Sys.getenv("OUTPUT_DIR"),
                            extension = "rds")


dfStudents_filtered <- dfStudents %>%
  dplyr::filter(type == "StudentEnrollment") %>%
  dplyr::filter(!is.na(user_id), !is.na(course_id), !is.na(sis_user_id)) %>%
  distinct(user_id, course_id)

tryCatch({
  # Read the previously processed data
  dfStudent_assignment_data_filled <- read_file_proj("CAN_Student_assignment_data",
                                   dir = "1. Ingelezen data/",
                                   add_branch = TRUE,
                                   base_dir = Sys.getenv("OUTPUT_DIR"),
                                   extension = "rds")

  df <- dfStudents %>%
    anti_join(dfStudent_assignment_data_filled, by = c("course_id", "user_id" = "student_id"))


  cat("Number of courses to process: ", nrow(df), "\n")


}, error = function(e) {
  # If read_file_proj throws an error, process all files
  df <- dfStudents_filtered

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
  sample_n(100000) %>% # For testing purposes
  select(course_id, user_id) %>%
  distinct() %>%
  future_pmap_dfr(function(course_id, user_id) {
    tryCatch(
      {
        result <- get_user_course_assignment_data2(canvas, course_id, user_id)

        # If the result is empty or NULL, return a dataframe with course_id and student_id
        if (is.null(result) || nrow(result) == 0) {
          return(tibble(course_id = course_id, student_id = user_id, no_data = TRUE))
        }

        # If result is not empty, ensure it includes course_id and student_id
        if (!"course_id" %in% names(result)) {
          result$course_id <- course_id
        }
        if (!"student_id" %in% names(result)) {
          result$student_id <- user_id
        }

        result
      },
      error = function(e) {
        tibble(course_id = course_id, student_id = user_id, error = as.character(e))
      }
    )
  }, .progress = TRUE)

if (exists("dfStudent_assignment_data_filled")) {
  dfStudent_assignment_data <- bind_rows(dfStudent_assignment_data, dfStudent_assignment_data_filled) %>%
    distinct()
}



# if (exists("dfStudent_assignment_data_filled")) {
#   dfStudent_assignment_data <- bind_rows(dfStudent_assignment_data, dfStudent_assignment_data_filled) %>%
#     distinct()
# }

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfStudent_assignment_data, "CAN_Student_assignment_data")

clear_script_objects()
