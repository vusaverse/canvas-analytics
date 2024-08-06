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

explore_scripts <- list.files("02_explore/", full.names = TRUE)

map(explore_scripts, source)

df <- dfCourse_to_join %>%
  left_join(dfAnnouncements_summarized, by = c("id" = "course_id", "INS_Inschrijvingsjaar"), suffix = c(".course", ".announcements")) %>%
  left_join(dfAssignments_summarized, by = c("id" = "course_id"), suffix = c(".course", ".assignments")) %>%
  left_join(dfEnrolments_summarized_role, by = c("id" = "course_id", "INS_Inschrijvingsjaar"), suffix = c(".course", ".role")) %>%
  left_join(dfEnrolments_summarized_type, by = c("id" = "course_id", "INS_Inschrijvingsjaar"), suffix = c(".course", ".type")) %>%
  left_join(dfFiles_summarized, by = c("id" = "course_id", "INS_Inschrijvingsjaar"), suffix = c(".course", ".files")) %>%
  left_join(dfPages_summarized, by = c("id" = "course_id", "INS_Inschrijvingsjaar"), suffix = c(".course", ".pages")) %>%
  left_join(dfPPT_summarized, by = c("id" = "course_id", "INS_Inschrijvingsjaar"), suffix = c(".course", ".powerpoint")) %>%
  left_join(dfStudents_summarized, by =c ("id" = "course_id", "INS_Inschrijvingsjaar"), suffix = c(".course", ".students"))
