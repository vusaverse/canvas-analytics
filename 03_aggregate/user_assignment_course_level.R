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

dfAssignment <- read_file_proj("CAN_Assignments") %>%
  dplyr::filter(is.na(error)) %>%
  dplyr::select(-error) %>%
  distinct()


dfAssignment_data <- read_file_proj("CAN_Assignment_data") %>%
  dplyr::filter(is.na(error)) %>%
  dplyr::select(-error) %>%
  distinct()


dfStudent_assignment <- read_file_proj("CAN_Student_assignment_data") %>%
  dplyr::filter(is.na(error),
                !is.na(assignment_id)) %>%
  dplyr::select(-error) %>%
  distinct()


dfQuizzes <- read_file_proj("CAN_Quizzes") %>%
  dplyr::filter(!is.na(assignment_id))




dfAssignment_set <- dfAssignment %>%
  left_join(dfAssignment_data, by = c("id" = "assignment_id", "course_id" = "course_id"), suffix = c("", "_data")) %>%
  left_join(dfQuizzes, by = c("id" = "assignment_id", "course_id" = "course_id"), suffix = c("", ".quiz"))


dfEnrolments <- read_file_proj("CAN_Enrolments")
dfStudents <- read_file_proj("CAN_Students")










dfTest <- dfEnrolments %>%
  distinct(user_id, course_id, .keep_all = TRUE)

dfTest2 <- dfStudents %>%
  distinct(id, course_id, .keep_all = TRUE)


dfEnrolments_information <- dfTest %>%
  left_join(dfTest2, by = c("user_id" = "id", "course_id" = "course_id"), suffix = c(".enrolment", ".student"))


dfStudent_assignment2 <- dfStudent_assignment %>%
  dplyr::left_join(dfEnrolments_information, by = c("student_id" = "user_id", "course_id" = "course_id"))



dfCourse_details <- read_file_proj("CAN_Course_details")

dfCourses <- read_file_proj("CAN_Index2")


dfCourse_information <- dfCourse_details %>%
  dplyr::left_join(dfCourses, by = intersect(names(dfCourse_details), names(dfCourses)))



dfStudent_assignment3 <- dfStudent_assignment2 %>%
  left_join(dfAssignment_set, by = c("assignment_id" = "id", "course_id" = "course_id")) %>%
  left_join(dfCourse_information, by = c("course_id" = "id"), suffix = c("", ".course"))






dfPython <- dfStudent_assignment3 %>%
  dplyr::filter(course_id == 70920)







## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(dfStudent_assignment3, "CAN_User_Assignment_course_level")

clear_script_objects()
