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

dfQuizzes <- read_file_proj("CAN_quizzes")


#Kijk naar de unique_course_ids en filter de quiz data zodat alleen de quizzes voorkomen die we zijn
#filter out empty rows by filtering out the empty id's
dfQuizzes_filtered <- dfQuizzes %>%
  dplyr::filter(course_id %in% unique_course_ids,
                !is.na(id),
                ##' *to do* filter out more unwated quiz_types
                # published,
                # locked_for_user,
                !is.na(quiz_type),
                question_count > 1,
                quiz_type %notin% c("survey", "graded_survey")) %>%
  select(course_id) %>%
  mutate(has_quiz = TRUE) %>%
  distinct(course_id, .keep_all = TRUE)

dfABE_Uiteindelijk_quizzes <- dfABE_Uiteindelijk %>%
  select(id, coursenumber, CourseName, AcademicYear, OnlineQuizzes) %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE),
         OnlineQuizzes = ifelse(OnlineQuizzes == 1, TRUE, FALSE)) %>%
  left_join(dfQuizzes_filtered, by = c("id" = "course_id")) %>%
  mutate(has_quiz = if_else(is.na(has_quiz), FALSE, TRUE),
         gelijk_onderzoek = OnlineQuizzes == has_quiz)

tabyl(dfABE_Uiteindelijk_quizzes$gelijk_onderzoek)
# |> tabyl(dfABE_Uiteindelijk_quizzes$gelijk_onderzoek)
#  dfABE_Uiteindelijk_quizzes$gelijk_onderzoek   n     percent valid_percent
#                                        FALSE  55 0.156250000     0.1571429
#                                         TRUE 295 0.838068182     0.8428571
#                                           NA   2 0.005681818            NA
tabyl(dfABE_Uiteindelijk_quizzes$has_quiz)

dfQuizzes %>% tabyl(quiz_type)
dfQuizzes %>% tabyl(published)

dfQuizzes %>% glimpse

vusa::write_file(dfQuizzes, "ABE_Online_quizzes", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)

