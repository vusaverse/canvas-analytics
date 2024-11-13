## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) Currently, not authorized to use this endpoint
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Get Course Timetable
#'
#' This function retrieves the last timetable set for a specified course in Canvas LMS.
#'
#' @param canvas A list containing the Canvas API configuration, including base_url and api_key.
#' @param course_id The ID of the course for which to retrieve the timetable.
#'
#' @return A data frame containing the course timetable information.
#'
#' @details This function uses the Canvas LMS API to fetch the last timetable set for a course
#' using the Set a course timetable endpoint. The timetable includes information about
#' scheduled events for the course.
#'
#' @export
#'
#' @examples
#' canvas <- list(base_url = "https://canvas.instructure.com", api_key = "your_api_key_here")
#' course_timetable <- get_course_timetable(canvas, course_id = 12345)
get_course_timetable <- function(canvas, course_id) {
  # Construct the API endpoint URL
  url <- paste0(canvas$base_url, "/api/v1/courses/", course_id, "/calendar_events/timetable")

  # Make the API request
  response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", canvas$api_key)))

  # Check the response status code
  # if (httr::status_code(response) != 200) {
  #   stop("Failed to retrieve course timetable. Please check your authentication and API endpoint.")
  # }

  # Parse the response as JSON
  timetable <- httr::content(response, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    as.data.frame()

  # Return the timetable
  return(timetable)
}


#
# dfCourses <- readrds_csv(output = "20. Test/CAN_Index.rds")
# cat("read in")
#
#
# # Set up parallel processing
# plan(multisession, workers = parallel::detectCores() - 1)
#
# dfTimetable <- dfCourses %>%
#   pull(course.id) %>%
#   future_map_dfr(~ {
#     tryCatch(
#       {
#         get_course_timetable(canvas, .x)
#       },
#       error = function(e) {
#         tibble(course_id = .x, error = as.character(e))
#       }
#     )
#   }, .progress = TRUE)
#
#
# vusa::write_file(dfTimetable, "CAN_Timetable", destination = "20. Test/", save_rds = TRUE)
