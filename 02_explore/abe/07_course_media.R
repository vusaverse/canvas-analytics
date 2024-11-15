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

# dfCoursemedia <- read_file_proj("CAN_Media_with_length")
dfCoursemedia <- dfMedia_mutated


#filteren op echt college, video van minimaal 40 min, een kennisclip is korter dan 40 minuten
dfCoursemedia_filtered_colleges <- dfCoursemedia %>%
  dplyr::filter(course_id %in% unique_course_ids,
                norm_minutes >= 40) %>%
  dplyr::add_count(course_id, name = "count_course_id") %>%
  select(course_id, count_course_id) %>%
  distinct(course_id, .keep_all = TRUE) %>%
  mutate(has_college_opname = TRUE)

##' *To do* add youtube/vimeo videos to kennisclips
##' Look at check_content.R:Line 148
dfCoursemedia_filtered_kennisclips <- dfCoursemedia %>%
  dplyr::filter(course_id %in% unique_course_ids,
                norm_minutes < 40) %>%
  dplyr::add_count(course_id, name = "count_course_kennisclip_id") %>%
  select(course_id, count_course_kennisclip_id) %>%
  distinct(course_id, .keep_all = TRUE) %>%
  mutate(has_kennisclip_opname = TRUE)


dfABE_Uiteindelijk_online <-  dfABE_Uiteindelijk %>%
  select(coursenumber, CourseName, AcademicYear, Collegesopgenomen, id) %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE),
         Collegesopgenomen = ifelse(Collegesopgenomen == 1, TRUE, FALSE)) %>%
  left_join(dfCoursemedia_filtered_colleges, by = c("id" = "course_id")) %>%
  left_join(dfCoursemedia_filtered_kennisclips, by = c("id" = "course_id")) %>%
  mutate(Coursemediacollege = ifelse(!is.na(count_course_id), TRUE, FALSE),
         has_college_opname = ifelse(is.na(has_college_opname), FALSE, TRUE),
         has_kennisclip_opname = ifelse(is.na(has_kennisclip_opname), FALSE, TRUE),
         gelijk_onderzoek_college = Collegesopgenomen == has_college_opname,
         gelijk_onderzoek_kennisclip = Collegesopgenomen == has_kennisclip_opname)

tabyl(dfABE_Uiteindelijk_online$gelijk_onderzoek_college)
tabyl(dfABE_Uiteindelijk_online$gelijk_onderzoek_kennisclip)

