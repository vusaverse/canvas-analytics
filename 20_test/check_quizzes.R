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
## X. quizzes ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

source("02_explore/course_quizzes.R")

dfABE_Uiteindelijk_quizzes <-  dfABE_Uiteindelijk %>%
  select(coursenumber, CourseName, AcademicYear, OnlineQuizzes) %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE))


dfQuiz <- dfQuizzes_summarized %>%
  mutate(
    INS_Inschrijvingsjaar = as.integer(str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")),
    COURSE_SIS_ID = str_extract(sis_course_id, "^.*?(?=_(201[6-9]|202[0-9]|2030)_)")
  ) %>%
  dplyr::filter(COURSE_SIS_ID %in% Check_courses) %>%
  left_join(dfABE_Uiteindelijk_quizzes, by = c(
    "COURSE_SIS_ID" = "CourseName",
    "INS_Inschrijvingsjaar" = "INS_Inschrijvingsjaar"
  )) %>%
  dplyr::filter(!is.na(OnlineQuizzes)) %>%
  mutate(
    has_quiz = n > 0,
    OnlineQuizzes_bool = OnlineQuizzes == 1,  # Convert powerpoints to boolean
    gelijk_onderzoek = has_quiz == OnlineQuizzes_bool  # Compare has_ppt with powerpoints_bool
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vusa::write_file(dfQuiz, "ABE_Online_quizzes", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)

clear_script_objects()
