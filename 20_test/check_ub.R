## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact:
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

example <- readxl::read_excel(paste0(Sys.getenv("NETWORK_DIR"), "Datasets/Canvas/UB/Download example AIP.xlsx"))

cleaned_example <- example %>%
  mutate(cleaned_url = str_remove(url, "https://canvas.vu.nl/files/")) %>%
  mutate(cleaned_url = as.integer(str_remove(cleaned_url, "\\?")))

total <- cleaned_example %>%
  left_join(dfFiles, by=c("cleaned_url" = "id"), keep=TRUE) %>%
  dplyr::filter(!is.na(course_id))


