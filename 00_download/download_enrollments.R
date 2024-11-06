## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) api call heeft een max parameter van 100
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfCourses <- read_file_proj("CAN_Index2",
                            dir = "1. Ingelezen data/",
                            add_branch = TRUE,
                            base_dir = Sys.getenv("OUTPUT_DIR"),
                            extension = "rds")

tryCatch({
  # Read the previously processed data
  dfEnrolments_filled <- read_file_proj("CAN_Enrolments",
                                           dir = "1. Ingelezen data/",
                                           add_branch = TRUE,
                                           base_dir = Sys.getenv("OUTPUT_DIR"),
                                           extension = "rds")

  # Check for errors in the existing data
  error_courses <- dfEnrolments_filled %>%
    filter(!is.na(error)) %>%
    pull(course_id)

  df <- dfCourses %>%
    dplyr::filter(!id %in% dfEnrolments_filled$course_id | id %in% error_courses)

  cat("Number of courses to process: ", nrow(df), "\n")

}, error = function(e) {
  # If read_file_proj throws an error, process all files
  df <- dfCourses

  cat(paste0("Processing all courses.\n"))
  cat("Number of courses to process: ", nrow(df), "\n")

})



get_all_enrollments <- function(canvas, course_id, per_page = 100) {
  # Initialize an empty list to store all enrollments
  all_enrollments <- list()

  # Construct the initial API endpoint URL
  url <- paste0(canvas$base_url, "/api/v1/courses/", course_id, "/enrollments?per_page=", per_page)

  repeat {
    # Make the API request
    response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", canvas$api_key)))

    # Check the response status code
    if (httr::status_code(response) != 200) {
      stop("Failed to retrieve course enrollments. Please check your authentication and API endpoint.")
    }

    # Parse the response as JSON
    enrollments <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), flatten = TRUE)

    # Append the current page of enrollments to the list
    all_enrollments <- c(all_enrollments, list(enrollments))

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

  # Combine all pages of enrollments into a single data frame
  combined_enrollments <- do.call(plyr::rbind.fill, all_enrollments)

  return(combined_enrollments)
}

# Helper function to parse the Link header
parse_link_header <- function(header) {
  links <- strsplit(header, ",")[[1]]
  result <- list()
  for (link in links) {
    parts <- strsplit(link, ";")[[1]]
    url <- gsub("[<>]", "", trimws(parts[1]))
    rel <- gsub("rel=", "", trimws(parts[2]))
    rel <- gsub('"', '', rel)
    result[[rel]] <- url
  }
  return(result)
}


# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

dfEnrolments <- df %>%
  pull(id) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        # vvcanvas::get_course_enrollments(canvas, .x)
        get_all_enrollments(canvas, .x)
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)



if (exists("dfEnrolments_filled")) {
  dfEnrolments <- bind_rows(dfEnrolments, dfEnrolments_filled)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfEnrolments, "CAN_Enrolments")

clear_script_objects()

