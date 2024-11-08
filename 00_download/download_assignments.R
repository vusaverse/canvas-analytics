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
  dfAssignments_filled <- read_file_proj("CAN_Assignments",
                                           dir = "1. Ingelezen data/",
                                           add_branch = TRUE,
                                           base_dir = Sys.getenv("OUTPUT_DIR"),
                                           extension = "rds")

  df <- dfCourses %>%
    dplyr::filter(!id %in% dfAssignments_filled$course_id)

  cat("Number of courses to process: ", nrow(df), "\n")


}, error = function(e) {
  # If read_file_proj throws an error, process all files
  df <- dfCourses

  cat(paste0("Processing all courses.\n"))
  cat("Number of courses to process: ", nrow(df), "\n")

})


get_all_course_assignments <- function(canvas, course_id, per_page = 100) {
  # Initialize an empty list to store all assignments
  all_assignments <- list()

  # Construct the initial API endpoint URL
  url <- paste0(canvas$base_url, "/api/v1/courses/", course_id, "/assignments?per_page=", per_page)

  repeat {
    # Make the API request
    response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", canvas$api_key)))

    # Check the response status code
    if (httr::status_code(response) != 200) {
      stop("Failed to retrieve assignments. Please check your authentication and API endpoint.")
    }

    # Parse the response as JSON
    assignments <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), flatten = TRUE)

    # Append the current page of assignments to the list
    all_assignments <- c(all_assignments, list(assignments))

    # Check for the next page URL in the Link header
    link_header <- httr::headers(response)$link
    if (!is.null(link_header)) {
      links <- parse_link_header(link_header)
      next_url <- links[["next"]]
      if (!is.null(next_url)) {
        url <- next_url
      } else {
        break  # No more pages
      }
    } else {
      break  # No Link header, assume no more pages
    }
  }

  # Combine all pages of assignments into a single data frame
  combined_assignments <- do.call(plyr::rbind.fill, all_assignments)

  # If no assignments were found, return a dataframe with just the course_id
  if (is.null(combined_assignments) || nrow(combined_assignments) == 0) {
    return(tibble(course_id = course_id))
  }

  # Add the course_id column
  combined_assignments <- combined_assignments %>%
    dplyr::mutate(course_id = course_id)

  return(combined_assignments)
}


library(parallel)
library(furrr)

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

dfAssignments <- df %>%
  pull(id) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        get_all_course_assignments(canvas, .x)
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)


if (exists("dfAssignments_filled")) {
  dfAssignments <- bind_rows(dfAssignments, dfAssignments_filled) %>%
    distinct()
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfAssignments, "CAN_Assignments")

clear_script_objects()

