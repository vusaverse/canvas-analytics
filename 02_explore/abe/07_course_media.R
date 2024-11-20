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

dfCoursemedia <- read_file_proj("CAN_Media_with_length")

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

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. Kennisclips shared in youtube and vimeo links ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

# Define regex patterns for YouTube and Vimeo
youtube_regex <- "\\b(?:https?:\\/\\/)?(?:www\\.)?(?:youtube\\.com\\/(?:watch\\?v=|embed\\/|v\\/|live\\/|user\\/|c\\/)|youtu\\.be\\/)([A-Za-z0-9_-]{11})\\b"
vimeo_regex <- "\\b(?:https?:\\/\\/)?(?:www\\.)?(?:vimeo\\.com\\/(?:[0-9]+|channels\\/[A-Za-z0-9_-]+\\/[0-9]+|groups\\/[A-Za-z0-9_-]+\\/videos\\/[0-9]+|album\\/[0-9]+\\/video\\/[0-9]+)|player\\.vimeo\\.com\\/video\\/[0-9]+)\\b"

# Combine YouTube and Vimeo regex patterns
video_regex <- paste(youtube_regex, vimeo_regex, sep="|")

# Check for video links in text_content
dfVideoLinks <- dfCombined %>%
  dplyr::filter(course_id %in% unique_course_ids) %>%
  mutate(contains_video_link = str_detect(text_content,
                                          regex(video_regex,
                                                ignore_case = TRUE)))

# Count occurrences by course_id
video_links_count <- dfVideoLinks %>%
  group_by(course_id) %>%
  summarise(video_links_mentions = sum(as.integer(contains_video_link), na.rm = TRUE))

# we need to update the table dfCoursemedia_filtered_kennisclips
dfCoursemedia_filtered_kennisclips_with_links <- dfCoursemedia_filtered_kennisclips %>%
  full_join(video_links_count, by = c("course_id" = "course_id")) %>%
  mutate(has_kennisclip_opname = ifelse((video_links_mentions > 0) | (count_course_kennisclip_id > 0) , TRUE, FALSE),
         has_kennisclip_opname = ifelse(is.na(has_kennisclip_opname), FALSE, TRUE))



## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## X. Check research data ####
## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## TODO: Perform assertion tests

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

dfABE_kennisclips <- dfABE_Uiteindelijk %>%
  dplyr::select(id, Kennisclips) %>%
  dplyr::left_join(dfCoursemedia_filtered_kennisclips_with_links, by = c("id" = "course_id")) %>%
  mutate(gelijk_kennisclips = Kennisclips == count_course_kennisclip_id,
         verschil_kennisclips = Kennisclips - count_course_kennisclip_id)

tabyl(dfABE_kennisclips$gelijk_kennisclips)
tabyl(dfABE_kennisclips$verschil_kennisclips)


vusa::write_file(dfABE_Uiteindelijk_online, "ABE_Opgenomen_colleges", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
vusa::write_file(dfABE_kennisclips, "ABE_Kennisclips", destination = "20. Test/", save_rds = TRUE, save_csv = TRUE)
