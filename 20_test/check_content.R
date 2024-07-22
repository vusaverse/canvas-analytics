## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) checks on mentimeter, youtube/vimeo links, community service learning (CSL), flipped classroom
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

## PDF

## syllabus

dfSyllabus <- dfCourses %>%
  select(course.id, course.syllabus_body)


# Step 1: Rename columns to a common name before binding
dfAnnouncements_selected <- dfAnnouncements_selected %>%
  rename(text_content = message)

dfPages_selected <- dfPages_selected %>%
  rename(text_content = page_body)

dfPPT_selected <- dfPPT_selected %>%
  rename(text_content = extracted_text)

dfSyllabus_selected <- dfSyllabus %>%
  rename(text_content = course.syllabus_body,
         course_id = course.id)

# Step 2: Bind the rows of the three dataframes
dfCombined <- bind_rows(
  dfAnnouncements_selected,
  dfPages_selected,
  dfPPT_selected,
  dfSyllabus_selected
)

dfCourse_to_join <- dfCourse_details %>%
  distinct() %>%
  mutate(INS_Inschrijvingsjaar = str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")) %>%
  select(id, name, sis_course_id, INS_Inschrijvingsjaar)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. mentimeter ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfMentimeter <- dfCombined %>%
  mutate(contains_mentimeter = str_detect(text_content,
                                          regex("\\b(menti|mentimeter)\\b|\\bmenti\\.com",
                                                ignore_case = TRUE)))

# Step 4: Count occurrences by course_id
mentimeter_count <- dfMentimeter %>%
  group_by(course_id) %>%
  summarise(mentimeter_mentions = sum(as.integer(contains_mentimeter), na.rm = TRUE))



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

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. youtube/vimeo ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Define regex patterns for YouTube and Vimeo
youtube_regex <- "\\b(?:https?:\\/\\/)?(?:www\\.)?(?:youtube\\.com\\/(?:watch\\?v=|embed\\/|v\\/|live\\/|user\\/|c\\/)|youtu\\.be\\/)([A-Za-z0-9_-]{11})\\b"
vimeo_regex <- "\\b(?:https?:\\/\\/)?(?:www\\.)?(?:vimeo\\.com\\/(?:[0-9]+|channels\\/[A-Za-z0-9_-]+\\/[0-9]+|groups\\/[A-Za-z0-9_-]+\\/videos\\/[0-9]+|album\\/[0-9]+\\/video\\/[0-9]+)|player\\.vimeo\\.com\\/video\\/[0-9]+)\\b"

# Combine YouTube and Vimeo regex patterns
video_regex <- paste(youtube_regex, vimeo_regex, sep="|")

# Check for video links in text_content
dfVideoLinks <- dfCombined %>%
  mutate(contains_video_link = str_detect(text_content,
                                          regex(video_regex,
                                                ignore_case = TRUE)))

# Step 4: Count occurrences by course_id
video_link_count <- dfVideoLinks %>%
  group_by(course_id) %>%
  summarise(video_link_mentions = sum(as.integer(contains_video_link), na.rm = TRUE))

# Join with course data
dfVideoLinksJoined <- video_link_count %>%
  left_join(dfCourse_to_join, by = c(
    "course_id" = "id"
  ))




dfABE_Uiteindelijk_kennisclips <- dfABE_Uiteindelijk %>%
  select(coursenumber, CourseName, AcademicYear, Kennisclips) %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE))

dfClips <- dfVideoLinksJoined %>%
  mutate(
    INS_Inschrijvingsjaar = as.integer(str_extract(sis_course_id         , "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")),
    COURSE_SIS_ID = str_extract(sis_course_id, "^.*?(?=_(201[6-9]|202[0-9]|2030)_)")
  ) %>%
  dplyr::filter(COURSE_SIS_ID %in% Check_courses) %>%
  left_join(dfABE_Uiteindelijk_kennisclips, by = c(
    "COURSE_SIS_ID" = "CourseName",
    "INS_Inschrijvingsjaar" = "INS_Inschrijvingsjaar"
  )) %>%
  mutate(
    gelijk_onderzoek = video_link_mentions == Kennisclips  # Compare has_ppt with powerpoints_bool
  ) %>%
  dplyr::filter(!is.na(Kennisclips))



## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. flipped classroom ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Define regex pattern for "flipped classroom"
flipped_classroom_regex <- "\\bflipped\\s+classroom\\b"

# Check for "flipped classroom" in text_content
dfFlippedClassroom <- dfCombined %>%
  mutate(contains_flipped_classroom = str_detect(text_content,
                                                 regex(flipped_classroom_regex,
                                                       ignore_case = TRUE)))
#
# Step 4: Count occurrences by course_id
flipped_classroom_count <- dfFlippedClassroom %>%
  group_by(course_id) %>%
  summarise(flipped_classroom_mentions = sum(as.integer(contains_flipped_classroom), na.rm = TRUE))

# Join with course data
dfFlippedClassroomJoined <- flipped_classroom_count %>%
  left_join(dfCourse_to_join, by = c(
    "course_id" = "id"
  ))


dfABE_Uiteindelijk_flipped <- dfABE_Uiteindelijk %>%
  select(coursenumber, CourseName, AcademicYear, Flipped) %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE))


# Further processing (similar to your dfClips processing)
dfFlippedClassroomFinal <- dfFlippedClassroomJoined %>%
  mutate(
    INS_Inschrijvingsjaar = as.integer(str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")),
    COURSE_SIS_ID = str_extract(sis_course_id, "^.*?(?=_(201[6-9]|202[0-9]|2030)_)")
  ) %>%
  dplyr::filter(COURSE_SIS_ID %in% Check_courses) %>%
  left_join(dfABE_Uiteindelijk_flipped, by = c(
    "COURSE_SIS_ID" = "CourseName",
    "INS_Inschrijvingsjaar" = "INS_Inschrijvingsjaar"
  )) %>%
  mutate(
    has_flipped = flipped_classroom_mentions  >  0,
    flipped_bool = Flipped == 1,  # Convert powerpoints to boolean
    gelijk_onderzoek = has_flipped == flipped_bool  # Compare has_ppt with powerpoints_bool
  ) %>%
  dplyr::filter(!is.na(Flipped))




## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. community service learning (CSL) ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# Define regex pattern for "flipped classroom"
csl_classroom_regex <- "\\bcommunity\\s+service\\s+learning\\b"

# Check for "flipped classroom" in text_content
dfCSL <- dfCombined %>%
  mutate(contains_flipped_classroom = str_detect(text_content,
                                                 regex(csl_classroom_regex,
                                                       ignore_case = TRUE)))
#
# Step 4: Count occurrences by course_id
dfCSL_count <- dfCSL %>%
  group_by(course_id) %>%
  summarise(csl_classroom_mentions = sum(as.integer(contains_flipped_classroom), na.rm = TRUE))

# Join with course data
dfCSLJoined <- dfCSL_count %>%
  left_join(dfCourse_to_join, by = c(
    "course_id" = "id"
  ))


dfABE_Uiteindelijk_CSL <- dfABE_Uiteindelijk %>%
  select(coursenumber, CourseName, AcademicYear, CSL) %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE))


# Further processing (similar to your dfClips processing)
dfCSLFinal <- dfCSLJoined %>%
  mutate(
    INS_Inschrijvingsjaar = as.integer(str_extract(sis_course_id, "(?<=_)(201[6-9]|202[0-9]|2030)(?=_)")),
    COURSE_SIS_ID = str_extract(sis_course_id, "^.*?(?=_(201[6-9]|202[0-9]|2030)_)")
  ) %>%
  dplyr::filter(COURSE_SIS_ID %in% Check_courses) %>%
  left_join(dfABE_Uiteindelijk_CSL, by = c(
    "COURSE_SIS_ID" = "CourseName",
    "INS_Inschrijvingsjaar" = "INS_Inschrijvingsjaar"
  )) %>%
  mutate(
    has_csl = csl_classroom_mentions  >  0,
    csl_bool = CSL == 1,  # Convert powerpoints to boolean
    gelijk_onderzoek = has_csl == csl_bool  # Compare has_ppt with powerpoints_bool
  ) %>%
  dplyr::filter(!is.na(CSL))









## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vusa::write_file(dfmentimeter2, "ABE_Mentimeter", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
vusa::write_file(dfClips, "ABE_Kennisclips", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)


vusa::write_file(dfFlippedClassroomFinal, "ABE_Flipped", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
vusa::write_file(dfCSLFinal, "ABE_CSL", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)

clear_script_objects()
