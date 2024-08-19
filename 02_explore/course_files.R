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

dfFiles <- read_file_proj("CAN_Files")
dfCourse_details <- read_file_proj("CAN_Course_details")


dfCourse_to_join <- dfCourse_details %>%
  distinct() %>%
  mutate(INS_Inschrijvingsjaar = str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")) %>%
  select(id, name, sis_course_id, INS_Inschrijvingsjaar)


dfFiles_summarized <- dfFiles %>%
  distinct() %>%
  count(course_id,  `content-type`) %>%
  pivot_wider(
    names_from = `content-type`,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total = rowSums(select(., -course_id))) %>%
  left_join(dfCourse_to_join, by = c(
    "course_id" = "id"
  ))

dfFiles_summarized <- dfFiles %>%
  distinct() %>%
  count(course_id,  mime_class) %>%
  pivot_wider(
    names_from = mime_class,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total = rowSums(select(., -course_id))) %>%
  left_join(dfCourse_to_join, by = c(
    "course_id" = "id"
  ))
