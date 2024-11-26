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

dfAssignments <- read_file_proj("CAN_Assignments")

dfAssignments_uiteindelijk <- dfAssignments %>%
  dplyr::select(course_id, id, error) %>%
  dplyr::filter(course_id %in% unique_course_ids,
                is.na(error)) %>%
  dplyr::add_count(course_id, name="count_assignment_data") %>%
  dplyr::distinct(course_id, count_assignment_data)

dfCombined <- dfAssignments_uiteindelijk %>%
  dplyr::left_join(dfABE_Uiteindelijk, by = c("course_id" = "id")) %>%
  dplyr::select(course_id, WOTotal, count_assignment_data) %>%
  dplyr::mutate(vergelijking_assignments = WOTotal - count_assignment_data,
                vergelijking_assignments_bool = WOTotal == count_assignment_data)

tabyl(dfCombined$vergelijking_assignments_bool)

vusa::write_file(dfCombined, "ABE_Assignments", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
