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

dfEnrolments <- read_file_proj("CAN_Enrolments")
dfCourse_details <- read_file_proj("CAN_Course_details")
dfCourses <- dfCourses %>%
  select(id, name)

dfCourse_to_join <- dfCourse_details %>%
  distinct() %>%
  mutate(INS_Inschrijvingsjaar = str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")) %>%
  select(id, name, sis_course_id, INS_Inschrijvingsjaar, course_code) %>%
  left_join(dfCourses, by = c("id" = "id",
                              "name" = "name"))


## handle double entries and non-active users
dfEnrolments <- dfEnrolments %>%
  distinct(course_id, user_id, .keep_all=TRUE) %>%
  dplyr::filter(enrollment_state == "active")



dfEnrolments_summarized_role <- dfEnrolments %>%
  distinct() %>%
  count(course_id, role) %>%
  pivot_wider(
    names_from = role,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total = rowSums(select(., -course_id))) %>%
  left_join(dfCourse_to_join, by = c(
    "course_id" = "id"
  ))


dfEnrolments_summarized_type <- dfEnrolments %>%
  distinct() %>%
  count(course_id, type) %>%
  pivot_wider(
    names_from = type,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total = rowSums(select(., -course_id))) %>%
  left_join(dfCourse_to_join, by = c(
    "course_id" = "id"
  ))


write_file(dfEnrolments_summarized_type, Name_to_save = "KeK_cursus_enrolments", save_csv = TRUE, destination = "20. Test/")
