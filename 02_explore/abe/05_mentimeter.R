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
source("02_explore/course_word_content.R")
source("02_explore/course_txt_content.R")

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

dfWord_selected <- dfWord %>%
  select(course_id, extracted_text) %>%
  rename(text_content = extracted_text)

dfText_selected <- dfText %>%
  select(course_id, extracted_text) %>%
  rename(text_content = extracted_text)


# Step 2: Bind the rows of the three dataframes
## TODO: join read and bind PDF data as well
dfCombined <- bind_rows(
  dfAnnouncements_selected,
  dfPages_selected,
  dfPPT_selected,
  dfPDF_selected,
  dfWord_selected,
  dfText_selected
)

##' *INFO* : menti.com bestaat ook!
# Step 3: Search for "mentimeter" in the text_content column
dfMentimeter <- dfCombined %>%
  dplyr::filter(course_id %in% unique_course_ids) %>%
  # mutate(contains_mentimeter = str_detect(text_content, regex("mentimeter", ignore_case = TRUE))) #%>%
  # mutate(contains_mentimeter = str_detect(text_content, regex("mentimeter|menti.com", ignore_case = TRUE)))
  mutate(contains_mentimeter = str_detect(text_content, regex("mentimeter|menti.com", ignore_case = TRUE)),
          contains_flipped = str_detect(text_content, regex("flipped classroom", ignore_case = TRUE)),
          contains_ebook = str_detect(text_content, regex("\\b(e[- ]?(book|module))\\b|\\bemodule\\b", ignore_case = TRUE)))

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
    mentimeter_bool = Mentimeter == 1,  # Convert powerpoints to boolean
    gelijk_onderzoek_mentimeter = has_mentimeter == mentimeter_bool  # Compare has_ppt with powerpoints_bool
  )

dfABE_Uiteindelijk_mentimeter %>% tabyl(gelijk_onderzoek_mentimeter)

flipped_classroom_count <- dfMentimeter %>%
  group_by(course_id) %>%
  summarize(flipped_classroom_mentions = sum(as.integer(contains_flipped), na.rm = TRUE))

dfABE_Uiteindelijk_flipped_classroom <- dfABE_Uiteindelijk %>%
  dplyr::select(id, coursenumber, CourseName, AcademicYear,Flipped) %>%
  dplyr::left_join(flipped_classroom_count, by = c("id" = "course_id")) %>%
  dplyr::mutate(
    has_flipped_classroom = flipped_classroom_mentions > 0,
    flipped_classroom_bool = Flipped == 1,
    gelijk_onderzoek_flipped_classroom = has_flipped_classroom == flipped_classroom_bool
  )

dfABE_Uiteindelijk_flipped_classroom %>% tabyl(gelijk_onderzoek_flipped_classroom)

ebook_count <- dfMentimeter %>%
  dplyr::group_by(course_id) %>%
  dplyr::summarize(ebook_available = sum(as.integer(contains_ebook), na.rm = TRUE))

dfABE_Uiteindelijk_ebooks <- dfABE_Uiteindelijk %>%
  dplyr::select(id, coursenumber, CourseName, AcademicYear, EBook) %>%
  dplyr::left_join(ebook_count, by = c("id" = "course_id")) %>%
  dplyr::mutate(
    has_ebooks = ebook_available > 0,
    ebook_bool = EBook == 1,
    gelijk_onderzoek_ebook = has_ebooks == ebook_bool
  )

dfABE_Uiteindelijk_ebooks %>% tabyl(gelijk_onderzoek_ebook)

vusa::write_file(dfABE_Uiteindelijk_mentimeter, "ABE_Mentimeter", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
vusa::write_file(dfABE_Uiteindelijk_flipped_classroom, "ABE_Flipped_classroom", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
vusa::write_file(dfABE_Uiteindelijk_ebooks, "ABE_ebooks", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)


