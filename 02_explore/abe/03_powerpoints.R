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

dfFiles <- read_file_proj("CAN_Files")

dfFiles_summarized <- dfFiles %>%
  dplyr::filter(course_id %in% unique_course_ids,
                is.na(page_id)) %>%
  dplyr::filter(hidden == FALSE) %>%
  distinct() %>%
  count(course_id,  mime_class) %>%
  pivot_wider(
    names_from = mime_class,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total = rowSums(select(., -course_id)))


dfFiles_summarized_2 <- dfFiles_summarized  %>%
  left_join(dfABE_Uiteindelijk, by = c(
    "course_id" = "id"
  )) %>%
  dplyr::select(CourseName, course_id, AcademicYear, Powerpoints, ppt) %>%
  mutate(
    has_ppt = ppt > 0,
    powerpoints_bool = Powerpoints == 1,  # Convert powerpoints to boolean
    gelijk_onderzoek = has_ppt == powerpoints_bool  # Compare has_ppt with powerpoints_bool
  )


dfFiles_summarized_2 %>% tabyl(gelijk_onderzoek)

vusa::write_file(dfFiles_summarized_2, "ABE_Powerpoint", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)

