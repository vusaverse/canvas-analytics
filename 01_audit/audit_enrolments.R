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

# read_file_proj()

dfEnrolments2 <- dfEnrolments %>%
  ## filter out test students
  dplyr::filter(user.name != "Test student",
                user.sortable_name != "student, Test",
                user.short_name != "Test student")

## Get all unique students
dfUnique_enrolments <- dfEnrolments2 %>%
  distinct(user.id, .keep_all = TRUE) %>%
  select(-course_id)

## Get all unique students per course
dfEnrolments_course <- dfEnrolments2 %>%
  distinct(user.id, course_id, .keep_all = TRUE)

## Get all unique students per type of enrolment

dfEnrolments_type <- dfEnrolments2 %>%
  distinct(user.id, type, .keep_all = TRUE)

dfEnrolments_type <- dfEnrolments2 %>%
  distinct(user.id, role, .keep_all = TRUE)


## Get all unique students per role course
dfEnrolments_course_role <- dfEnrolments2 %>%
  distinct(user.id, course_id, role, .keep_all = TRUE)


dfEnrolments_course_role <- dfEnrolments2 %>%
  distinct(user.id, course_id, type, .keep_all = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# write_file_proj()

clear_script_objects()
