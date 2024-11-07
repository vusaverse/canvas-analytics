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

dfMedia <- read_file_proj("CAN_Media")
dfCourse_details <- read_file_proj("CAN_Course_details")


dfCourse_to_join <- dfCourse_details %>%
  distinct() %>%
  mutate(INS_Inschrijvingsjaar = str_extract(sis_term_id, "(201[6-9]|202[0-9]|2030)(?=_)")) %>%
  select(id, name, sis_course_id, INS_Inschrijvingsjaar, sis_term_id)


dfMedia_mutated <- dfMedia %>%
  distinct() %>%
  mutate(
    media_sources = map(media_sources, ~{
      if(is.data.frame(.x)) .x
      else if(is.list(.x) && all(sapply(.x, is.data.frame))) do.call(rbind, .x)
      else tibble(content = list(.x))
    })
  ) %>%
  unnest(media_sources) %>%
  distinct(media_id, .keep_all=TRUE) %>%
  mutate(
    size_bits = as.numeric(size) * 1024 * 8,  # Convert KB to bits
    bitrate_bps = as.numeric(bitrate),        # Assuming it's already in bps
    duration_seconds = size_bits / bitrate_bps,
    duration_minutes = duration_seconds / 60,
    duration_hours = duration_minutes / 60,
    # New columns for normalized duration
    norm_hours = floor(duration_hours),
    norm_minutes = floor((duration_hours - norm_hours) * 60),
    norm_seconds = round(((duration_hours - norm_hours) * 60 - norm_minutes) * 60)
  ) %>%
  mutate(
    # Adjust for cases where seconds round up to 60
    norm_minutes = norm_minutes + floor(norm_seconds / 60),
    norm_seconds = norm_seconds %% 60,

    # Adjust for cases where minutes go up to 60
    norm_hours = norm_hours + floor(norm_minutes / 60),
    norm_minutes = norm_minutes %% 60,
    duration_formatted = sprintf("%02d:%02d:%02d", norm_hours, norm_minutes, norm_seconds)
  )

vusa::write_file(dfMedia_mutated, "CAN_Media_with_length", destination = "20. Test/", save_rds = TRUE)

