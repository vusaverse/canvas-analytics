## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) Aggregation level users. Different types/roles are included.
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


dfEnrolments <- read_file_proj("CAN_Enrolments")
dfStudents <- read_file_proj("CAN_Students")

dfTest <- dfEnrolments %>%
  distinct(user_id, course_id, .keep_all = TRUE)

dfTest2 <- dfStudents %>%
  distinct(id, course_id, .keep_all = TRUE)


dfEnrolments_information <- dfTest %>%
  left_join(dfTest2, by = c("user_id" = "id", "course_id" = "course_id"), suffix = c(".enrolment", ".student"))





#
dfParticipation <- read_file_proj("CAN_Course_student_summaries") %>%
  select(-error)

dfEnrolments_information_part <- dfEnrolments_information %>%
  left_join(dfParticipation, by = c("user_id" = "id", "course_id" = "course_id"), suffix = c(".enrolment", ".participation"))

##' Not that interesting of data
# dfSummaries <- read_file_proj("CAN_Course_participation")


dfCourse_details <- read_file_proj("CAN_Course_details")

dfCourses <- read_file_proj("CAN_Index")


dfCourse_information <- dfCourse_details %>%
  dplyr::left_join(dfCourses, by = c("id" = "course.id"))



dfEnrolments_information_final <- dfEnrolments_information_part %>%
  left_join(dfCourse_information, by = c("course_id" = "id"), suffix = c(".enrolment", ".course"))
#
#

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfEnrolments_information_final, "CAN_User_course_level")

clear_script_objects()
