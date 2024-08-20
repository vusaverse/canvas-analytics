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

## Read

# dfStudents <- read_file_proj("CAN_Students")

dfStudents2 <- dfStudents %>%
  ## filter out test students
  dplyr::filter(name != "Test student",
         sortable_name != "student, Test",
         short_name != "Test student")

## Get all unique students
dfUnique_students <- dfStudents2 %>%
  distinct(id, .keep_all = TRUE) %>%
  select(-course_id)

## Get all unique students per course
dfStudents_course <- dfStudents2 %>%
  distinct(id, course_id, .keep_all = TRUE)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# write_file_proj()


clear_script_objects()
