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


## _________________________________________________________________________________________________
## Course information
dfCourse_details <- read_file_proj("CAN_Course_details")

dfCourses <- read_file_proj("CAN_Index2")


dfCourse_information <- dfCourse_details %>%
  dplyr::left_join(dfCourses, by = intersect(names(dfCourse_details), names(dfCourses)))


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

## _________________________________________________________________________________________________
## Course enrolment information
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

dfCourse_information <- dfCourse_information %>%
  ##unlist all lists
  mutate(across(where(is.list), ~ map_chr(., ~ paste(.x, collapse = ", "))))


## _______________________________________________________________________________________________
## Course counts


dfAnnouncements <- read_file_proj("CAN_Announcements")
dfAssignments <- read_file_proj("CAN_Assignments")
dfMedia <- read_file_proj("CAN_Media")
dfModules <- read_file_proj("CAN_Modules")
dfPages <- read_file_proj("CAN_Pages")
dfQuizzes <- read_file_proj("CAN_Quizzes")

dfAnnouncements_summarized <- dfAnnouncements %>%
  distinct() %>%
  count(course_id) %>%
  rename(Announcement_count = n)

dfAssignments_summarized <- dfAssignments %>%
  distinct() %>%
  count(course_id) %>%
  rename(Assignment_count = n)

dfMedia_summarized <- dfMedia %>%
  distinct() %>%
  count(course_id) %>%
  rename(Media_count = n)

dfModules_summarized <- dfModules %>%
  distinct() %>%
  count(course_id) %>%
  rename(Module_count = n)

dfPages_summarized <- dfPages %>%
  distinct() %>%
  count(course_id) %>%
  rename(Page_count = n)


dfQuizzes_summarized <- dfQuizzes %>%
  distinct() %>%
  count(course_id) %>%
  rename(Quiz_count = n)

## Join course counts to the analysis set
dfCourse_information <- dfCourse_information %>%
  left_join(dfAnnouncements_summarized, by = c("id" = "course_id")) %>%
  left_join(dfAssignments_summarized, by = c("id" = "course_id")) %>%
  left_join(dfMedia_summarized, by = c("id" = "course_id")) %>%
  left_join(dfModules_summarized, by = c("id" = "course_id")) %>%
  left_join(dfPages_summarized, by = c("id" = "course_id")) %>%
  left_join(dfQuizzes_summarized, by = c("id" = "course_id"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfCourse_information, "CAN_Course_level")


vusa::clear_script_objects()
