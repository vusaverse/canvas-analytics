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

dfStudents <- readrds_csv(output = "20. Test/CAN_Students.rds")
cat("read in")

library(parallel)
library(furrr)

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

dfStudent_assignments <- dfStudents %>%
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


vusa::write_file(dfStudent_assignments, "CAN_Student_assignment_data", destination = "20. Test/", save_rds = TRUE)

