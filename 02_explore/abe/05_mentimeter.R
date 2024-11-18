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

source("02_explore/course_announcement.R")
source("02_explore/course_pages.R")
source("02_explore/course_ppt_content.R")
source("02_explore/course_pdf_content.R")

dfAnnouncements_selected <- dfAnnouncements %>%
  select(course_id, message) %>%
  rename(text_content = message)

dfPages_selected <- dfPages %>%
  select(course_id, page_body) %>%
  rename(text_content = page_body)


dfPPT_selected <- dfPPT %>%
  select(course_id, extracted_text) %>%
  rename(text_content = extracted_text)


dfPDF_selected <- dfPDF %>%
  select(course_id, extracted_text) %>%
  rename(text_content = extracted_text)


# Step 2: Bind the rows of the three dataframes
## TODO: join read and bind PDF data as well
dfCombined <- bind_rows(
  dfAnnouncements_selected,
  dfPages_selected,
  dfPPT_selected,
  dfPDF_selected
)

##' *INFO* : menti.com bestaat ook!
# Step 3: Search for "mentimeter" in the text_content column
dfMentimeter <- dfCombined %>%
  dplyr::filter(course_id %in% unique_course_ids) %>%
  # mutate(contains_mentimeter = str_detect(text_content, regex("mentimeter", ignore_case = TRUE))) #%>%
  # mutate(contains_mentimeter = str_detect(text_content, regex("mentimeter|menti.com", ignore_case = TRUE)))
  mutate(contains_mentimeter = str_detect(text_content, regex("menti.com", ignore_case = TRUE)))

# Step 4: Count occurrences by course_id
mentimeter_count <- dfMentimeter %>%
  group_by(course_id) %>%
  summarise(mentimeter_mentions = sum(as.integer(contains_mentimeter), na.rm = TRUE))

# Step 5: Join the count with the course data
dfABE_Uiteindelijk_mentimeter <- dfABE_Uiteindelijk %>%
  select(id, coursenumber, CourseName, AcademicYear, Mentimeter) %>%
  left_join(mentimeter_count, by = c("id" = "course_id")) %>%
  mutate(
    has_mentimeter = mentimeter_mentions  >  0,
    metimeter_bool = Mentimeter == 1,  # Convert powerpoints to boolean
    gelijk_onderzoek = has_mentimeter == metimeter_bool  # Compare has_ppt with powerpoints_bool
  )


dfABE_Uiteindelijk_mentimeter %>% tabyl(gelijk_onderzoek)
