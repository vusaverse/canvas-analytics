## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) Get aggregated student counts in course
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfPPT <- readrds_csv(output = "20. Test/CAN_Powerpoint_text.rds")
dfCourse_details <- readrds_csv(output = "20. Test/CAN_Course_details.rds")

dfCourse_to_join <- dfCourse_details %>%
  distinct() %>%
  mutate(INS_Inschrijvingsjaar = str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")) %>%
  select(id, name, sis_course_id, INS_Inschrijvingsjaar)


dfPPT_summarized <- dfPPT %>%
  distinct() %>%
  group_by(course_id) %>%
  summarise(
    n = n()
  ) %>%
  left_join(dfCourse_to_join, by = c(
    "course_id" = "id"
  ))


