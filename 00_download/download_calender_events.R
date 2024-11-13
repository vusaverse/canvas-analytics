## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
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
  dfCalendar_events_filled <- read_file_proj("CAN_Calendar_events",
                                            dir = "1. Ingelezen data/",
                                            add_branch = TRUE,
                                            base_dir = Sys.getenv("OUTPUT_DIR"),
                                            extension = "rds")

  df <- dfCourses %>%
    dplyr::filter(!id %in% dfCalendar_events_filled$course_id)

  cat("Number of courses to process: ", nrow(df), "\n")


}, error = function(e) {
  # If read_file_proj throws an error, process all files
  df <- dfCourses

  cat(paste0("Processing all courses.\n"))
  cat("Number of courses to process: ", nrow(df), "\n")

})


# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

dfCalendar_events <- dfCourses %>%
  mutate(info = map2(id, calendar.ics, ~ list(id = .x, calendar_ics = .y))) %>%
  pull(info) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        # Access the id and calendar URL from the list
        id <- .x$id
        calendar_ics <- .x$calendar_ics

        # Attempt to read the calendar data
        calendar_data <- calendar::ic_read(calendar_ics)

        # Ensure the result is a data frame and add id
        as.data.frame(calendar_data) %>% mutate(course_id = id, calendar_ics = calendar_ics)
      },
      error = function(e) {
        # If there's an error, return id and error message
        tibble(course_id = id, calendar_ics = calendar_ics, error = as.character(e))
      }
    )
  }, .progress = TRUE) %>%
  relocate(course_id, calendar_ics)

if (exists("dfCalendar_events")) {
  dfCalendar_events <- bind_rows(dfCalendar_events, dfCalendar_events_filled) %>%
    distinct()
}

prepare_and_send_summary(dfCalendar_events,
                         dfCalendar_events_filled,
                         nrow(df))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfCalendar_events, "CAN_Calendar_events")

clear_script_objects()
