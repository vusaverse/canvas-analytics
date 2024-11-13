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
  dfAnnouncements_filled <- read_file_proj("CAN_Announcements",
                                 dir = "1. Ingelezen data/",
                                 add_branch = TRUE,
                                 base_dir = Sys.getenv("OUTPUT_DIR"),
                                 extension = "rds")

  df <- dfCourses %>%
    dplyr::filter(!id %in% dfAnnouncements_filled$course_id)

  cat("Number of courses to process: ", nrow(df), "\n")


}, error = function(e) {
  # If read_file_proj throws an error, process all files
  df <- dfCourses

  cat(paste0("Processing all courses.\n"))
  cat("Number of courses to process: ", nrow(df), "\n")

})

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

get_course_announcements <- function(canvas, course_id, per_page = 100) {
  # Construct the API endpoint URL
  url <- paste0(canvas$base_url, "/api/v1/courses/", course_id, "/discussion_topics?only_announcements=true&per_page=", per_page)

  # Make the API request
  response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", canvas$api_key)))

  # Check the response status code
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve course announcements. Please check your authentication and API endpoint.")
  }

  # Parse the response as JSON
  announcements <- httr::content(response, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  # If announcements is empty or NULL, return a dataframe with just the course_id
  if (is.null(announcements) || length(announcements) == 0 || (is.data.frame(announcements) && nrow(announcements) == 0)) {
    return(tibble(course_id = course_id))
  }

  # If announcements is not a data frame, convert it to one
  if (!is.data.frame(announcements)) {
    announcements <- as.data.frame(announcements)
  }

  # Add the course_id column
  announcements <- announcements %>%
    dplyr::mutate(course_id = course_id)

  # Return the data frame of announcements
  return(announcements)
}

dfAnnouncements <- df %>%
  pull(id) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        get_course_announcements(canvas, .x)
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)


if (exists("dfAnnouncements_filled")) {
  dfAnnouncements <- bind_rows(dfAnnouncements, dfAnnouncements_filled) %>%
    distinct()
}

prepare_and_send_summary(dfAnnouncements,
                         dfAnnouncements_filled,
                         nrow(df))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfAnnouncements, "CAN_Announcements")

clear_script_objects()
