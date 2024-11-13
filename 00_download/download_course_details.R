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
  dfCourse_details_filled <- read_file_proj("CAN_Course_details",
                                           dir = "1. Ingelezen data/",
                                           add_branch = TRUE,
                                           base_dir = Sys.getenv("OUTPUT_DIR"),
                                           extension = "rds")

  df <- dfCourses %>%
    dplyr::filter(!id %in% dfCourse_details_filled$id)

  cat("Number of courses to process: ", nrow(df), "\n")


}, error = function(e) {
  # If read_file_proj throws an error, process all files
  df <- dfCourses

  cat(paste0("Processing all courses.\n"))
  cat("Number of courses to process: ", nrow(df), "\n")

})


# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

dfCourse_details <- df %>%
  dplyr::pull(id) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        vvcanvas::get_course_details(canvas, .x)
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)

if (exists("dfCourse_details_filled")) {
  dfCourse_details <- bind_rows(dfCourse_details, dfCourse_details_filled)
}

prepare_and_send_summary(dfCourse_details,
                         dfCourse_details_filled,
                         nrow(df))

## Join term information to dataframe
dfTerms <- read_file_proj("CAN_Terms",
                            dir = "1. Ingelezen data/",
                            add_branch = TRUE,
                            base_dir = Sys.getenv("OUTPUT_DIR"),
                            extension = "rds")


dfCourse_details <- dfCourse_details %>%
  dplyr::left_join(dfTerms, by = c("enrollment_term_id" = "id"), suffix = c("", "_term"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfCourse_details, "CAN_Course_details")

clear_script_objects()


