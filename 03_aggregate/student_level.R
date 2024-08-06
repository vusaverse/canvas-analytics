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


dfStudents <- readrds_csv(output = "20. Test/CAN_Students.rds")
dfEnrolments <- readrds_csv(output = "20. Test/CAN_Enrolments.rds")
dfCourse_details <- readrds_csv(output = "20. Test/CAN_Course_details.rds")
dfCourse_student_summaries <- readrds_csv(output = "20. Test/CAN_Course_student_summaries.rds")


dfCourse_to_join <- dfCourse_details %>%
  distinct() %>%
  mutate(INS_Inschrijvingsjaar = str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")) %>%
  select(id, name, sis_course_id, INS_Inschrijvingsjaar)



dfEnrolments_test <- dfEnrolments %>%
  select(c(user.id,
           course_id,
           type,
           role,
           role_id,
           enrollment_state,
           sis_account_id,
           sis_user_id,
           grades.current_grade, grades.final_score,
           grades.unposted_current_grade, grades.unposted_current_score,
           grades.override_grade, grades.override_score,
           grades.unposted_final_grade, grades.unposted_final_score
  )) %>%
  distinct()

dfStudents_joined <- dfStudents %>%
  left_join(dfCourse_to_join, by = c("course_id" = "id"), suffix = c(".student", ".course")) %>%
  left_join(dfCourse_student_summaries, by = c("course_id" = "course_id", "id" = "id")) %>%
  left_join(dfEnrolments_test, by = c("id" = "user.id", "course_id" = "course_id"), suffix = c(".student", ".enrolment")) %>%
  distinct()



