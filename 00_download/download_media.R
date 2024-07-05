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

dfCourses <- readrds_csv(output = "20. Test/CAN_Index.rds")
cat("read in")

library(parallel)
library(furrr)

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

dfMedia <- dfCourses %>%
  pull(course.id) %>%
  future_map_dfr(~ {
    tryCatch(
      {
        vvcanvas::get_course_media_objects(canvas, .x)
      },
      error = function(e) {
        tibble(course_id = .x, error = as.character(e))
      }
    )
  }, .progress = TRUE)


vusa::write_file(dfMedia, "CAN_Media", destination = "20. Test/", save_rds = TRUE)

#
# sample_course <- dfCourses %>% pull(course.id) %>% sample(1)
#
#
# dfTest <- vvcanvas::get_course_media_objects(canvas, sample_course) %>%
#   unnest(media_sources)
# #
# #
# dfTest_mutated <- dfTest %>%
#   mutate(
#     size_bits = as.numeric(size) * 1024 * 8,  # Convert KB to bits
#     bitrate_bps = as.numeric(bitrate),        # Assuming it's already in bps
#     duration_seconds = size_bits / bitrate_bps,
#     duration_minutes = duration_seconds / 60
#   )
