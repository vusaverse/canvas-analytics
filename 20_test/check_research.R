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
  mutate(count_difference = NoStudents != StudentEnrollment)


dfTest %>% tabyl(count_difference)


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
  mutate(count_difference = NoTeachers != TeacherEnrollment)

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
  mutate(count_difference = NoTeachers != teacher_and_TA)


dfTest_teacher_type %>% tabyl(count_difference)
dfTest_teacher_role %>% tabyl(count_difference)


vusa::write_file(dfTest, "ABE_Studenten", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
vusa::write_file(dfTest_teacher_role, "ABE_Gebruiker_roll", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
vusa::write_file(dfTest_teacher_type, "ABE_Gebruiker_type", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)



## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. activiteiten ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfVakas_filtered %>% glimpse
dfRooster_2019 %>% glimpse


dfRooster_2019 %>%
  group_by(academic_year, ROO_Modulecode)



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
  dplyr::filter(AcademicYear == 1) %>%
  select(coursenumber, CourseName, AcademicYear, StudyYear, starts_with("P"), MeetingsTotal) %>%
  select(-Powerpoints )

dfRooster_summarized <- dfABE_Uiteindelijk_roostering %>%
  left_join(dfRooster_summarizes, by = c(
    "CourseName" = "ROO_Modulecode"
  )) %>%
  select(coursenumber, CourseName, AcademicYear, StudyYear, MeetingsTotal, Total, Hoorcollege, PNA1) %>%
  mutate(gelijk = MeetingsTotal == Total,
         colleges_overeen = Hoorcollege == PNA1,
         verschil = Hoorcollege - PNA1)


vusa::write_file(dfRooster_summarized, "ABE_P_Roostering", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. powerpoints ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

source("02_explore/course_files.R")

dfABE_Uiteindelijk_files <-  dfABE_Uiteindelijk %>%
  select(coursenumber, CourseName, AcademicYear, Powerpoints) %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE))


dfFiles_summarized_2 <- dfFiles_summarized  %>%
  mutate(
    INS_Inschrijvingsjaar = as.integer(str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")),
    COURSE_SIS_ID = str_extract(sis_course_id, "^.*?(?=_(201[6-9]|202[0-9]|2030)_)")
  ) %>%
  dplyr::filter(COURSE_SIS_ID %in% Check_courses, INS_Inschrijvingsjaar == 2019) %>%
  left_join(dfABE_Uiteindelijk_files, by = c(
    "COURSE_SIS_ID" = "CourseName",
    "INS_Inschrijvingsjaar" = "INS_Inschrijvingsjaar"
  )) %>%
  mutate(
    has_ppt = ppt > 0,
    powerpoints_bool = Powerpoints == 1,  # Convert powerpoints to boolean
    gelijk_onderzoek = has_ppt == powerpoints_bool  # Compare has_ppt with powerpoints_bool
  )


dfFiles_summarized_2 %>% tabyl(gelijk_onderzoek)

vusa::write_file(dfFiles_summarized_2, "ABE_Powerpoint", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)



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



vusa::write_file(dfQuiz, "ABE_Online_quizzes", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. mentimeter ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

source("02_explore/course_announcement.R")
source("02_explore/course_pages.R")
source("02_explore/course_ppt_content.R")

dfAnnouncements_selected <- dfAnnouncements %>%
  select(course_id, message)

dfPages_selected <- dfPages %>%
  select(course_id, page_body)


dfPPT_selected <- dfPPT %>%
  select(course_id, extracted_text)


# Step 1: Rename columns to a common name before binding
dfAnnouncements_selected <- dfAnnouncements_selected %>%
  rename(text_content = message)

dfPages_selected <- dfPages_selected %>%
  rename(text_content = page_body)

dfPPT_selected <- dfPPT_selected %>%
  rename(text_content = extracted_text)

# Step 2: Bind the rows of the three dataframes
dfCombined <- bind_rows(
  dfAnnouncements_selected,
  dfPages_selected,
  dfPPT_selected
)

##' *INFO* : menti.com bestaat ook!
# Step 3: Search for "mentimeter" in the text_content column
dfMentimeter <- dfCombined %>%
  # mutate(contains_mentimeter = str_detect(text_content, regex("mentimeter", ignore_case = TRUE))) #%>%
  mutate(contains_mentimeter = str_detect(text_content, regex("menti.com", ignore_case = TRUE))) #%>%
  # filter(contains_mentimeter)

# Step 4: Count occurrences by course_id
mentimeter_count <- dfMentimeter %>%
  group_by(course_id) %>%
  summarise(mentimeter_mentions = sum(as.integer(contains_mentimeter), na.rm = TRUE))

# mentimeter_count <- dfMentimeter %>%
#   group_by(course_id) %>%
#   summarise(mentimeter_mentions = n())
#
#
# mentimeter_count <- dfMentimeter %>%
#   group_by(course_id) %>%
#   summarise(mentimeter_mentions = n_distinct(course_id, na.rm = TRUE))


dfCourse_to_join <- dfCourse_details %>%
  distinct() %>%
  mutate(INS_Inschrijvingsjaar = str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")) %>%
  select(id, name, sis_course_id, INS_Inschrijvingsjaar)



dfmentimeter <- mentimeter_count %>%
  left_join(dfCourse_to_join, by = c(
    "course_id" = "id"
  ))


dfABE_Uiteindelijk_mentimeter <- dfABE_Uiteindelijk %>%
  select(coursenumber, CourseName, AcademicYear, Mentimeter) %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE))

dfmentimeter2 <- dfmentimeter %>%
  mutate(
    INS_Inschrijvingsjaar = as.integer(str_extract(sis_course_id         , "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")),
    COURSE_SIS_ID = str_extract(sis_course_id, "^.*?(?=_(201[6-9]|202[0-9]|2030)_)")
  ) %>%
  dplyr::filter(COURSE_SIS_ID %in% Check_courses) %>%
  left_join(dfABE_Uiteindelijk_mentimeter, by = c(
    "COURSE_SIS_ID" = "CourseName",
    "INS_Inschrijvingsjaar" = "INS_Inschrijvingsjaar"
  )) %>%
  mutate(
    has_mentimeter = mentimeter_mentions  >  0,
    metimeter_bool = Mentimeter == 1,  # Convert powerpoints to boolean
    gelijk_onderzoek = has_mentimeter == metimeter_bool  # Compare has_ppt with powerpoints_bool
  ) %>%
  dplyr::filter(!is.na(Mentimeter))


vusa::write_file(dfmentimeter2, "ABE_Mentimeter", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)


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

vusa::write_file(dfDiscussions2, "ABE_Discussions", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. college-opnames ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

source("02_explore/course_media.R")

dfMedia_summarized <- dfMedia_mutated %>%
  group_by(course_id) %>%
  summarise(count_long_videos = sum(norm_minutes >= 40, na.rm = TRUE)) %>%
  arrange(desc(count_long_videos)) %>%
  left_join(dfCourse_to_join, by = c(
    "course_id" = "id"
  ))


dfABE_Uiteindelijk_online <-  dfABE_Uiteindelijk %>%
  select(coursenumber, CourseName, AcademicYear, Collegesopgenomen) %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE))


dfMed <- dfMedia_summarized %>%
  mutate(
    INS_Inschrijvingsjaar = as.integer(str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")),
    COURSE_SIS_ID = str_extract(sis_course_id, "^.*?(?=_(201[6-9]|202[0-9]|2030)_)")
  ) %>%
  dplyr::filter(COURSE_SIS_ID %in% Check_courses) %>%
  left_join(dfABE_Uiteindelijk_online, by = c(
    "COURSE_SIS_ID" = "CourseName",
    "INS_Inschrijvingsjaar" = "INS_Inschrijvingsjaar"
  )) %>%
  dplyr::filter(!is.na(Collegesopgenomen))


vusa::write_file(dfMed, "ABE_Ogenomen_colleges", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
