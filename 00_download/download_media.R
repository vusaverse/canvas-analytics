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

dfCourses <- read_file_proj("CAN_Index",
                            dir = "1. Ingelezen data/",
                            add_branch = TRUE,
                            base_dir = Sys.getenv("OUTPUT_DIR"),
                            extension = "rds")

tryCatch({
  # Read the previously processed data
  dfMedia_filled <- read_file_proj("CAN_Media",
                                           dir = "1. Ingelezen data/",
                                           add_branch = TRUE,
                                           base_dir = Sys.getenv("OUTPUT_DIR"),
                                           extension = "rds")

  df <- dfCourses %>%
    dplyr::filter(!course.id %in% dfMedia_filled$course_id)

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

dfMedia <- df %>%
  pull(course.id) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        vvcanvas::get_course_media_objects(canvas, .x)
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)

if (exists("dfMedia_filled")) {
  dfMedia <- bind_rows(dfMedia, dfMedia_filled) %>%
    distinct()
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfMedia, "CAN_Media")

clear_script_objects()
