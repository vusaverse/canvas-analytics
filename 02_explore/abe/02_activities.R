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


#Ontsluit nieuwe calender data (zie project deze week)
dfCalenderEvents <- read_file_proj("CAN_Calendar_events")
#Kijk naar de unique_course_ids en filter de nieuwe calender data zodat alleen de unique_course_ids voorkomen die we zien
dfCalenderEventsFiltered_with_errors <- dfCalenderEvents %>%
  dplyr::filter(course_id %in% unique_course_ids,
                !is.na(error)) %>%
  dplyr::distinct(course_id) %>%
  dplyr::mutate(has_calender = FALSE)

dfCalenderEventsFiltered_no_errors <- dfCalenderEvents %>%
  dplyr::filter(course_id %in% unique_course_ids,
                is.na(error)) %>%
  dplyr::distinct(course_id) %>%
  dplyr::mutate(has_calender = TRUE)

#kolom aanmaken waarbij wordt aangegeven of een row een calender heeft, vervolgens bind_rows toepassen.
dfCalenderfinal <- dfCalenderEventsFiltered_with_errors %>%
  dplyr::bind_rows(dfCalenderEventsFiltered_no_errors)

#Filter lege kalender data eruit. Dus match met de Timetable (1 is true 2 is false)
dfABE_Uiteindelijk_filtered <- dfABE_Uiteindelijk %>%
  dplyr::mutate(Timetable = ifelse(Timetable == 1, TRUE, FALSE)) %>%
  dplyr::select(CourseName, id, AcademicYear, Timetable) %>%
  dplyr::left_join(dfCalenderfinal, by = c("id" = "course_id")) %>%
  dplyr::mutate(gelijk_onderzoek = Timetable == has_calender)

tabyl(dfABE_Uiteindelijk_filtered$gelijk_onderzoek)

