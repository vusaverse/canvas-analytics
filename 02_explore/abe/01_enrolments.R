## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Verspreiding buiten de VU: Ja
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#ophalen enrolments
dfEnrolments <- read_file_proj("CAN_Enrolments")
#filteren op unieke cursusid's
df_Enrolments_filtered <- dfEnrolments %>%
  distinct(course_id, user_id, .keep_all=TRUE) %>%
  dplyr::filter(course_id %in% unique_course_ids) %>%
  dplyr::filter(enrollment_state == "active")

dfEnrolments_summarized_role <- df_Enrolments_filtered %>%
  distinct() %>%
  count(course_id, role) %>%
  pivot_wider(
    names_from = role,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total = rowSums(select(., -course_id)))

dfEnrolments_summarized_type <- df_Enrolments_filtered %>%
  distinct() %>%
  count(course_id, type) %>%
  pivot_wider(
    names_from = type,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total = rowSums(select(., -course_id)))

dfTest <- dfEnrolments_summarized_type %>%
  left_join(result, c("course_id" = "id")) %>%
  select(CourseName, INS_Inschrijvingsjaar, NoStudents, StudentEnrollment, course_id) %>%
  dplyr::filter(!is.na(NoStudents)) %>%
  mutate(gelijk_onderzoek = NoStudents == StudentEnrollment,
         verschil_onderzoek = NoStudents - StudentEnrollment)

tabyl(dfTest$gelijk_onderzoek)

dfTestTeachers <- dfEnrolments_summarized_role %>%
  left_join(result, c("course_id" = "id")) %>%
  select(CourseName, INS_Inschrijvingsjaar, NoTeachers, TeacherEnrollment, course_id) %>%
  dplyr::filter(!is.na(NoTeachers)) %>%
  mutate(gelijk_onderzoek = NoTeachers == TeacherEnrollment,
         verschil_onderzoek = NoTeachers - TeacherEnrollment)

tabyl(dfTestTeachers$gelijk_onderzoek)

dfTestTeachersTa <- dfEnrolments_summarized_role %>%
  left_join(result, c("course_id" = "id")) %>%
  select(CourseName, INS_Inschrijvingsjaar, NoTeachers, TeacherEnrollment, Coordinator, TaEnrollment, `TA No Grading`, course_id) %>%
  dplyr::filter(!is.na(NoTeachers)) %>%
  mutate(teacher_and_TA = TeacherEnrollment + Coordinator + TaEnrollment + `TA No Grading`) %>%
  mutate(gelijk_onderzoek = NoTeachers == teacher_and_TA,
         verschil_onderzoek = NoTeachers - teacher_and_TA)

tabyl(dfTestTeachersTa$gelijk_onderzoek)
