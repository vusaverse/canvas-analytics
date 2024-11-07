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
  dfPages_filled <- read_file_proj("CAN_Pages",
                                           dir = "1. Ingelezen data/",
                                           add_branch = TRUE,
                                           base_dir = Sys.getenv("OUTPUT_DIR"),
                                           extension = "rds")

  # Check for errors in the existing data
  error_courses <- dfPages_filled %>%
    dplyr::filter(!is.na(error)) %>%
    pull(course_id) %>%
    unique()

  df <- dfCourses %>%
    dplyr::filter(!id %in% dfPages_filled$course_id | id %in% error_courses)

  # df <- dfCourses %>%
  #   dplyr::filter(!id %in% dfPages_filled$course_id)

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

# dfPages <- df %>%
#   pull(id) %>%
#   future_map_dfr(~ {
#     tryCatch(
#       {
#         vvcanvas::get_course_pages(canvas, .x)
#       },
#       error = function(e) {
#         tibble(course_id = .x, error = as.character(e))
#       }
#     )
#   }, .progress = TRUE)


dfPages <- df %>%
  sample_n(10) %>%
  pull(id) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        result <- vvcanvas::get_course_pages(canvas, .x)
        result %>% mutate(course_id = .x)
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)

if (exists("dfPages_filled")) {
  dfPages <- bind_rows(dfPages, dfPages_filled)
  dfPages <- dfPages %>%
    mutate(across(where(is.character), ~iconv(., to = "UTF-8"))) %>%
    distinct()
}



if (exists("dfPages_filled")) {
  dfPages <- bind_rows(dfPages, dfPages_filled)
  dfPages <- dfPages %>%
    mutate(across(where(is.character), ~iconv(., to = "UTF-8"))) %>%
    distinct()

}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfPages, "CAN_Pages")

clear_script_objects()
