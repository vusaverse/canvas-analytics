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
  mutate(count_difference = NoStudents != StudentEnrollment)


dfTest %>% tabyl(count_difference)


dfTest_teacher <- dfEnrolments_summarized_type %>%
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
  mutate(count_difference = NoTeachers != TeacherEnrollment)


dfTest_teacher %>% tabyl(count_difference)
