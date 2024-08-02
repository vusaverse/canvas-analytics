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
## X. activiteiten ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfVakas_filtered %>% glimpse
dfRooster_2019 %>% glimpse


dfRooster_2019 %>%
  group_by(academic_year, ROO_Modulecode)

dfRooster_2019_mapped <- dfRooster_2019 %>%
  mapping_translate(ROO_Type_activiteit, ROO_Type_activiteit2, "Mapping_KeK_onderwijsvormen.csv")


## translate to general activitiy names
dfRooster_2019_mapped <-
  mapping_translate(dfRooster_2019,
                    "ROO_Type_activiteit",
                    "ROO_Type_activiteit2",
                    mapping_table_name = "Mapping_KeK_onderwijsvormen.csv",
                    KeepOriginal = FALSE
  )

dfRooster_2019_mapped %>% tabyl(ROO_Type_activiteit2)

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

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vusa::write_file(dfRooster_summarized, "ABE_P_Roostering", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)

clear_script_objects()
