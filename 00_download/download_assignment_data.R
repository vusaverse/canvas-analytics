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
  dfAssignment_data_filled <- read_file_proj("CAN_Assignment_data",
                                           dir = "1. Ingelezen data/",
                                           add_branch = TRUE,
                                           base_dir = Sys.getenv("OUTPUT_DIR"),
                                           extension = "rds")

  # Check for errors in the existing data
  error_courses <- dfAssignment_data_filled %>%
    dplyr::filter(!is.na(error)) %>%
    pull(course_id)

  df <- dfCourses %>%
    dplyr::filter(!id %in% dfAssignment_data_filled$course_id | id %in% error_courses)

  cat("Number of courses to process: ", nrow(df), "\n")


}, error = function(e) {
  # If read_file_proj throws an error, process all files
  df <- dfCourses

  cat(paste0("Processing all courses.\n"))
  cat("Number of courses to process: ", nrow(df), "\n")

})


# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

dfAssignment_data <- df %>%
  pull(id) %>%
  future_map_dfr(~ {
    course_id <- .x
    tryCatch(
      {
        result <- vvcanvas::get_assignment_data(canvas, course_id)
        result %>% mutate(course_id = course_id)
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)


if (exists("dfAssignment_data_filled")) {
  dfAssignment_data <- bind_rows(dfAssignment_data, dfAssignment_data_filled)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfAssignment_data, "CAN_Assignment_data")

clear_script_objects()

