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

#Read research data
dfABE_Uiteindelijk <- read_file_proj("ABE_Uiteindelijk")

#Read course data
dfCourses <- read_file_proj("CAN_Course_details")

#change numeric data to year format, normalize Coursename
dfABE_Uiteindelijk <- dfABE_Uiteindelijk %>%
  mutate(INS_Inschrijvingsjaar = case_when(AcademicYear == 1 ~ 2019,
                                           AcademicYear == 2 ~ 2022,
                                           AcademicYear == 3 ~ 2023,
                                           FALSE ~ TRUE),
         CourseName = ifelse(CourseName == 'R_Ebel', "R_EBel", CourseName))

#Get all unique coursenames from research data
course_names <- dfABE_Uiteindelijk %>%
  pull(CourseName) %>%
  unique()


dfCourses_filtered <- dfCourses %>%
  dplyr::filter(course_code %in% course_names) %>%
  tidyr::separate(sis_term_id_term, into = c("academic_year", "rest"), sep = '_', extra = "merge", fill = "right") %>%
  mutate(academic_year = as.integer(academic_year)) %>%
  select(course_code, academic_year, id)

result <- dfABE_Uiteindelijk %>%
  dplyr::left_join(dfCourses_filtered, by = c("CourseName" = "course_code", "INS_Inschrijvingsjaar" = "academic_year")) %>%
  distinct()

#pull unique course ids
unique_course_ids <- result %>%
  pull(id) %>%
  unique



