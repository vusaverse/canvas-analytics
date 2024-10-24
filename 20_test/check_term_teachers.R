## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) This scripts was written for Canvas management as they wish to know which teachers are listed
##    in the courses for the term 2017-2018 academic year.
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Read course details as added to it were the term names
dfCourses <- read_file_proj("CAN_Course_details",
                            dir = "1. Ingelezen data/",
                            add_branch = TRUE,
                            base_dir = Sys.getenv("OUTPUT_DIR"),
                            extension = "rds")


# Read the previously processed data
dfEnrolments_filled <- read_file_proj("CAN_Enrolments",
                                      dir = "1. Ingelezen data/",
                                      add_branch = TRUE,
                                      base_dir = Sys.getenv("OUTPUT_DIR"),
                                      extension = "rds")

df <- dfCourses %>%
  dplyr::filter(!course.id %in% dfEnrolments_filled$course_id)


## join the course details with the enrolments
## filter on the term name and role
dfEnrolments_terms <- dfEnrolments_filled %>%
  dplyr::left_join(dfCourses %>% select(id, name_term), by = c("course_id" = "id")) %>%
  dplyr::select(course_id, name_term, id, user_id, user.id, user.name,
                user.short_name, user.sortable_name, user.integration_id, sis_user_id, type, role, sis_course_id) %>%
  dplyr::filter(role == "TeacherEnrollment",
         grepl("2017", name_term))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfEnrolments, "CAN_2017_cursus_docent")

clear_script_objects()

