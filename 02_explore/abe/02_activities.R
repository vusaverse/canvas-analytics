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



## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
##' Use schedule data from outside Canvas
##'


dfRooster <- readrds_csv(output = "20. Test/TermTime_historic.rds")

dfRooster_2019 <- dfRooster %>%
  dplyr::filter(ROO_Modulecode %in% Check_courses)


setdiff(Check_courses, dfRooster_2019$ROO_Modulecode)

dfVakas_breed <- readrds_csv(output = "3. Analyseset/Vakken_Analyseset_breed_na_stap_1.rds")


dfVakas_filtered <- dfVakas_breed %>%
  dplyr::filter(UAS_Vak_Code %in% Check_courses,
                UAS_Vak_Jaar %in% c(2019, 2022, 2023)) %>%
  select(UAS_Vak_Code, UAS_Vak_Jaar)

dfABE_Uiteindelijk <- dfABE_Uiteindelijk %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE)) %>%
  ## in CourseName change value R_Ebel into R_EBel
  mutate(CourseName = str_replace_all(CourseName, "R_Ebel", "R_EBel")) %>%
  left_join(dfVakas_filtered, by = c("CourseName" = "UAS_Vak_Code",
                                     "INS_Inschrijvingsjaar" = "UAS_Vak_Jaar"))

dfRooster_summarizes <- dfRooster_2019 %>%
  distinct() %>%
  count(academic_year,  ROO_Modulecode, ROO_Type_activiteit) %>%
  pivot_wider(
    names_from = ROO_Type_activiteit,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(select(., -academic_year, -ROO_Modulecode)))

dfABE_Uiteindelijk_roostering <- dfABE_Uiteindelijk %>%
  # dplyr::filter(AcademicYear == 1) %>%
  select(coursenumber, CourseName, AcademicYear, StudyYear, starts_with("P"), MeetingsTotal, INS_Inschrijvingsjaar) %>%
  select(-Powerpoints )

dfRooster_summarized <- dfABE_Uiteindelijk_roostering %>%
  left_join(dfRooster_summarizes, by = c(
    "CourseName" = "ROO_Modulecode",
    "INS_Inschrijvingsjaar" = "academic_year"
  )) %>%
  select(coursenumber, CourseName, AcademicYear, StudyYear, MeetingsTotal, Total, Hoorcollege, Werkcollege, Excursie, Practicum, PNA1, PA1, PA2, PA6) %>%
  mutate(gelijk_totaal_aantal_meetings =  Total == MeetingsTotal,
         verschil_totaal_aantal_meetings = Total - MeetingsTotal,
         colleges_overeen = Hoorcollege == PNA1,
         verschil_hoorcolleges = Hoorcollege - PNA1,
         werkcollege_overeen = Werkcollege == PA1,
         Verschil_werkcolleges = Werkcollege - PA1,
         practicum_overeen = Practicum == PA2,
         Verschil_practicum = Practicum - PA2,
         excursie_overeen = Excursie == PA6,
         Verschil_excursie = Excursie - PA6,
         AcademicYear = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE))


dfRooster_summarized %>% tabyl(gelijk_totaal_aantal_meetings)
dfRooster_summarized %>% tabyl(colleges_overeen)
dfRooster_summarized %>% tabyl(excursie_overeen)
dfRooster_summarized %>% tabyl(werkcollege_overeen)
dfRooster_summarized %>% tabyl(practicum_overeen)
dfRooster_summarized %>% tabyl(AcademicYear)

dfRooster %>% tabyl(ROO_Type_activiteit)

vusa::write_file(dfRooster_summarized, "ABE_P_Roostering", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)

