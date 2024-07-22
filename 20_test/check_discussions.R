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
sDir <- paste0(Sys.getenv("RAW_DATA_DIR"), "Canvas-api/DocumentenABE2/")
vCanvas <- list.files(sDir, recursive = TRUE, full.names = TRUE, pattern = ".sav$")

dfCourses <- readrds_csv(output = "20. Test/CANVAS_INDEX.rds")

dfABE_Uiteindelijk <- haven::read_spss(vCanvas[2])
Check_courses <- dfABE_Uiteindelijk %>%
  pull(CourseName) %>%
  unique()

dfTermTime <- readrds_csv(output = "1. Ingelezen data/ROO_TT_Roosteractiviteiten.rds")


dfRooster <- readrds_csv(output = "20. Test/TermTime_historic.rds")

dfRooster_2019 <- dfRooster %>%
  dplyr::filter(academic_year == 2019,
                ROO_Modulecode %in% Check_courses)


setdiff(Check_courses, dfRooster_2019$ROO_Modulecode)

dfVakas_breed <- readrds_csv(output = "3. Analyseset/Vakken_Analyseset_breed_na_stap_1.rds")

intersect(Check_courses, dfVakas_breed$UAS_Vak_Code)

dfVakas_filtered <- dfVakas_breed %>%
  dplyr::filter(UAS_Vak_Code %in% Check_courses,
                UAS_Vak_Jaar %in% c(2019, 2022))





## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. discussions ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
  mutate(
    INS_Inschrijvingsjaar = as.integer(str_extract(course.sis_source_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")),
    COURSE_SIS_ID = str_extract(course.sis_source_id, "^.*?(?=_(201[6-9]|202[0-9]|2030)_)")
  ) %>%
  dplyr::filter(COURSE_SIS_ID %in% Check_courses) %>%
  left_join(dfABE_Uiteindelijk_discussions, by = c(
    "COURSE_SIS_ID" = "CourseName",
    "INS_Inschrijvingsjaar" = "INS_Inschrijvingsjaar"
  )) %>%
  mutate(
    discussions_bool = Discussionboard == 1,  # Convert powerpoints to boolean
    gelijk_onderzoek = has_discussion == discussions_bool  # Compare has_ppt with powerpoints_bool
  ) %>%
  dplyr::filter(!is.na(Discussionboard))

dfDiscussions2 %>% tabyl(gelijk_onderzoek)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vusa::write_file(dfDiscussions2, "ABE_Discussions", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)

clear_script_objects()
