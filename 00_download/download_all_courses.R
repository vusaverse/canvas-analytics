## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Distribution outside of the VU: yes.
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##' It appears that a large number of courses were not retrieved using the below function
##' *TEMP* Use function below it
# dfCourses <- vvcanvas::get_all_courses(canvas)

get_all_courses <- function(canvas, account_id = 1, per_page = 100) {
  all_courses <- data.frame()
  url <- paste0(canvas$base_url, "/api/v1/accounts/", account_id, "/courses?per_page=", per_page)

  tryCatch({
    while (!is.null(url)) {
      response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", canvas$api_key)))

      if (httr::status_code(response) != 200) {
        warning("Failed to retrieve courses from URL: ", url,
                "\nStatus code: ", httr::status_code(response),
                "\nReturning courses retrieved so far.")
        break
      }

      courses <- httr::content(response, "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE)

      all_courses <- dplyr::bind_rows(all_courses, courses)

      message("Fetched ", nrow(courses), " courses. Total: ", nrow(all_courses))

      links <- httr::headers(response)$link
      if (!is.null(links)) {
        next_link <- stringr::str_match(links, '<([^>]+)>; rel="next"')
        url <- next_link[2]
      } else {
        url <- NULL
      }
    }
  }, error = function(e) {
    warning("An error occurred: ", e$message, "\nReturning courses retrieved so far.")
  })

  return(all_courses)
}


courses <- get_all_courses(canvas)



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# write_file_proj(dfCourses, "CAN_Index")
write_file_proj(courses, "CAN_Index2")

clear_script_objects()
