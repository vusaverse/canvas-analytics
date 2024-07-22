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
## X. enrolments ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

source("02_explore/course_enrolments.R")


dfCourse_to_join_new <- dfCourse_details %>%
  distinct() %>%
  mutate(
    INS_Inschrijvingsjaar = str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)"),
    COURSE_SIS_ID = str_extract(sis_course_id, "^.*?(?=_(201[6-9]|202[0-9]|2030)_)")
  ) %>%
  select(id, name, sis_course_id, COURSE_SIS_ID, INS_Inschrijvingsjaar) %>%
  dplyr::filter(COURSE_SIS_ID %in% Check_courses)


dfABE_Uiteindelijk_students <- dfABE_Uiteindelijk %>%
  select(coursenumber, CourseName, NoStudents, NoTeachers, AcademicYear) %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE))

dfTest <- dfEnrolments_summarized_type %>%
  mutate(
    INS_Inschrijvingsjaar = as.integer(str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")),
    COURSE_SIS_ID = str_extract(sis_course_id, "^.*?(?=_(201[6-9]|202[0-9]|2030)_)")
  ) %>%
  dplyr::filter(COURSE_SIS_ID %in% Check_courses) %>%
  left_join(dfABE_Uiteindelijk_students, by = c(
    "COURSE_SIS_ID" = "CourseName",
    "INS_Inschrijvingsjaar" = "INS_Inschrijvingsjaar"
  )) %>%
  select(INS_Inschrijvingsjaar, COURSE_SIS_ID, NoStudents, StudentEnrollment, course_id) %>%
  dplyr::filter(!is.na(NoStudents)) %>%
  mutate(gelijk_onderzoek = NoStudents == StudentEnrollment)


dfTest %>% tabyl(gelijk_onderzoek)


dfTest_teacher_type <- dfEnrolments_summarized_type %>%
  mutate(
    INS_Inschrijvingsjaar = as.integer(str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")),
    COURSE_SIS_ID = str_extract(sis_course_id, "^.*?(?=_(201[6-9]|202[0-9]|2030)_)")
  ) %>%
  dplyr::filter(COURSE_SIS_ID %in% Check_courses) %>%
  left_join(dfABE_Uiteindelijk_students, by = c(
    "COURSE_SIS_ID" = "CourseName",
    "INS_Inschrijvingsjaar" = "INS_Inschrijvingsjaar"
  )) %>%
  select(INS_Inschrijvingsjaar, COURSE_SIS_ID, NoTeachers, TeacherEnrollment, course_id) %>%
  dplyr::filter(!is.na(NoTeachers)) %>%
  mutate(gelijk_onderzoek = NoTeachers == TeacherEnrollment)

dfTest_teacher_role <- dfEnrolments_summarized_role %>%
  mutate(
    INS_Inschrijvingsjaar = as.integer(str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")),
    COURSE_SIS_ID = str_extract(sis_course_id, "^.*?(?=_(201[6-9]|202[0-9]|2030)_)")
  ) %>%
  dplyr::filter(COURSE_SIS_ID %in% Check_courses) %>%
  left_join(dfABE_Uiteindelijk_students, by = c(
    "COURSE_SIS_ID" = "CourseName",
    "INS_Inschrijvingsjaar" = "INS_Inschrijvingsjaar"
  )) %>%
  select(INS_Inschrijvingsjaar, COURSE_SIS_ID, NoTeachers, TeacherEnrollment, Coordinator, TaEnrollment, `TA No Grading`, course_id) %>%
  dplyr::filter(!is.na(NoTeachers)) %>%
  # mutate(count_difference = NoTeachers != TeacherEnrollment) %>%
  mutate(teacher_and_TA = TeacherEnrollment + Coordinator + TaEnrollment + `TA No Grading`) %>%
  mutate(gelijk_onderzoek = NoTeachers == teacher_and_TA)


dfTest_teacher_type %>% tabyl(gelijk_onderzoek)
dfTest_teacher_role %>% tabyl(gelijk_onderzoek)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vusa::write_file(dfTest, "ABE_Studenten", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
vusa::write_file(dfTest_teacher_role, "ABE_Gebruiker_roll", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
vusa::write_file(dfTest_teacher_type, "ABE_Gebruiker_type", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)

clear_script_objects()
