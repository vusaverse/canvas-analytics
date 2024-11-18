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

##' *NOTE*
##' Uses old location as we no longer have access to this endpoint
dfDiscussions <- readrds_csv(output = "20. Test/CAN_All_courses_discussions.rds")


dfDiscussions <- dfDiscussions %>% select(course.id, course.sis_source_id, course.name, num_discussion) %>%
  mutate(has_discussion = num_discussion > 0)

dfABE_Uiteindelijk_discussions <- dfABE_Uiteindelijk %>%
  select(coursenumber, CourseName, AcademicYear, Discussionboard) %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE))

dfDiscussions2 <- dfDiscussions %>%
  dplyr::filter(course.id %in% unique_course_ids) %>%
  left_join(dfABE_Uiteindelijk, by = c(
    "course.id" = "id"
  )) %>%
  select(coursenumber, CourseName, AcademicYear, Discussionboard, has_discussion) %>%
  mutate(
    discussions_bool = Discussionboard == 1,  # Convert powerpoints to boolean
    gelijk_onderzoek = has_discussion == discussions_bool  # Compare has_ppt with powerpoints_bool
  ) %>%
  dplyr::filter(!is.na(Discussionboard))

dfDiscussions2 %>% tabyl(gelijk_onderzoek)

vusa::write_file(dfDiscussions2, "ABE_Discussions", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
