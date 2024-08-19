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

get_all_course_files <- function(canvas, course_id, per_page = 100) {
  # Initialize an empty list to store all files
  all_files <- list()

  # Construct the initial API endpoint URL
  url <- paste0(canvas$base_url, "/api/v1/courses/", course_id, "/files?per_page=", per_page)

  repeat {
    # Make the API request
    response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", canvas$api_key)))

    # Check the response status code
    if (httr::status_code(response) != 200) {
      stop("Failed to retrieve files. Please check your authentication and API endpoint.")
    }

    # Parse the response as JSON
    files <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"), flatten = TRUE)

    # Add course_id to the files data frame
    files$course_id <- course_id

    # Append the current page of files to the list
    all_files <- c(all_files, list(files))

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

  # Combine all pages of files into a single data frame
  combined_files <- do.call(plyr::rbind.fill, all_files)

  return(combined_files)
}

# Helper function to parse the Link header (same as before)
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

dfCourses <- readrds_csv(output = "20. Test/CAN_Index.rds")
cat("read in")

# # library(parallel)
# # library(furrr)
#
# # Set up parallel processing
# # plan(multisession, workers = parallel::detectCores() - 1)
# #
# # dfFiles <- dfCourses %>%
# #   pull(course.id) %>%
# #   future_map_dfr(~ {
# #     tryCatch(
# #       {
# #         # vvcanvas::get_course_files(canvas, .x)
# #         get_all_course_files(canvas, .x)
# #       },
# #       error = function(e) {
# #         tibble(course_id = .x, error = as.character(e))
# #       }
# #     )
# #   }, .progress = TRUE)
# #
# #
# # vusa::write_file(dfFiles, "CAN_Files", destination = "20. Test/", save_rds = TRUE)
# #
#
#
#
library(dplyr)
library(purrr)
library(furrr)
library(parallel)

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

dfFiles <- dfCourses %>%
  pull(course.id) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        files <- get_all_course_files(canvas, .x)
        if (is.data.frame(files) && nrow(files) > 0) {
          files
        } else {
          tibble(course_id = .x, error = "No files or empty result")
        }
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfFiles, "CAN_Files")

clear_script_objects()
