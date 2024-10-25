## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact:
##
##' *INFO*:
## 1) Left join all dataframes to the dfCourses dataframe
## 2) Counts are calculated for each course
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# dfEnrolments <- read_file_proj("CAN_Enrolments")
dfCourse_details <- read_file_proj("CAN_Course_details")

dfCourses <- read_file_proj("CAN_Index")


dfCourse_information <- dfCourse_details %>%
  dplyr::left_join(dfCourses, by = c("id" = "course.id"))


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

##' *INFO* Enrolments contains both `role` and `type` for users
##' Add user counts to the course-level analysis set
dfEnrolments <- read_file_proj("CAN_Enrolments")
dfStudents <- read_file_proj("CAN_Students")


dfStudents_summarized <- dfStudents %>%
  distinct() %>%
  group_by(course_id) %>%
  summarise(
    count_students = n()
  )

dfEnrolments_summarized_role <- dfEnrolments %>%
  distinct() %>%
  count(course_id, role) %>%
  pivot_wider(
    names_from = role,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(Enrolment_total_role = rowSums(select(., -course_id)))


dfEnrolments_summarized_type <- dfEnrolments %>%
  distinct() %>%
  count(course_id, type) %>%
  pivot_wider(
    names_from = type,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(Enrolment_total_type = rowSums(select(., -course_id)))

## Join user counts to the analysis set
dfCourse_information <- dfCourse_information %>%
  left_join(dfStudents_summarized, by = c("id" = "course_id")) %>%
  left_join(dfEnrolments_summarized_role, by = c("id" = "course_id"), suffix = c("", "_role")) %>%
  left_join(dfEnrolments_summarized_type, by = c("id" = "course_id"), suffix = c("", "_type"))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfCourse_information, "CAN_Course_level")


vusa::clear_script_objects()
