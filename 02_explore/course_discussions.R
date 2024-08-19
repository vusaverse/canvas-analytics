## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
##
##' *INFO*:
## 1) This is a datasets which was already aggregated, therefore the numbers might not be reliable
## 2) Therefore we add a boolean column, just in case
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dfDiscussions <- readrds_csv(output = "20. Test/CAN_All_courses_discussions.rds")


# dfDiscussions %>% select(course.id, course.name, num_discussion) %>%
#   mutate(has_discussion = num_discussion > 0) %>%
#   view()
