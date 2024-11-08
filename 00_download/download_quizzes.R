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
  dfQuizzes_filled <- read_file_proj("CAN_Quizzes",
                                           dir = "1. Ingelezen data/",
                                           add_branch = TRUE,
                                           base_dir = Sys.getenv("OUTPUT_DIR"),
                                           extension = "rds")

  df <- dfCourses %>%
    dplyr::filter(!id %in% dfQuizzes_filled$course_id)

  cat("Number of courses to process: ", nrow(df), "\n")


}, error = function(e) {
  # If read_file_proj throws an error, process all files
  df <- dfCourses

  cat(paste0("Processing all courses.\n"))
  cat("Number of courses to process: ", nrow(df), "\n")

})


library(parallel)
library(furrr)

get_course_quizzes <- function(canvas, course_id, per_page = 100) {
  # Construct the API endpoint URL
  url <- paste0(canvas$base_url, "/api/v1/courses/", course_id, "/quizzes?per_page=", per_page)

  # Make the API request
  response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", canvas$api_key)))

  # Check the response status code
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve course quizzes. Please check your authentication and API endpoint.")
  }

  # Parse the response as JSON
  quizzes <- httr::content(response, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  # If quizzes is empty or NULL, return a dataframe with just the course_id
  if (is.null(quizzes) || length(quizzes) == 0 || (is.data.frame(quizzes) && nrow(quizzes) == 0)) {
    return(tibble(course_id = course_id))
  }

  # If quizzes is not a data frame, convert it to one
  if (!is.data.frame(quizzes)) {
    quizzes <- as.data.frame(quizzes)
  }

  # Add the course_id column
  quizzes <- quizzes %>%
    dplyr::mutate(course_id = course_id)

  # Return the data frame of quizzes
  return(quizzes)
}



# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

dfQuizzes <- df %>%
  pull(id) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        get_course_quizzes(canvas, .x)
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)

if (exists("dfQuizzes_filled")) {
  dfQuizzes <- bind_rows(dfQuizzes, dfQuizzes_filled) %>%
    distinct()
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfQuizzes, "CAN_Quizzes")

clear_script_objects()
