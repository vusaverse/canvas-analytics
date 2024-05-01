## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact:
##
##' *INFO*:
## 1) Left join all dataframes to the dfCourses dataframe
## 2) Counts are calculated for each course
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


df <- dfCourses %>%
  select(course.id, course.name, course.start_at, course.created_at, course.updated_at) %>%
  mutate(
    course.start_at = vvconverter::academic_year(as.Date(course.start_at)),
    course.created_at = vvconverter::academic_year(as.Date(course.created_at)),
    course.updated_at = vvconverter::academic_year(as.Date(course.updated_at))
  ) %>%
  left_join(
    dfStudents %>%
      group_by(course_id) %>%
      summarize(unique_students = n_distinct(id)),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfAnnouncements %>%
      group_by(course_id) %>%
      summarize(unique_announcements = n_distinct(id)),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfAssignments %>%
      group_by(course_id) %>%
      summarize(unique_assignments = n_distinct(id)),                                     ## max 10?
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfAssignment_groups %>%
      group_by(course_id) %>%
      summarize(unique_assignment_groups = n_distinct(id)),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfDiscussions %>%
      group_by(course_id) %>%
      summarize(unique_discussions = n_distinct(id)),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfEnrollments %>%
      distinct(course_id, user_id, role) %>%
      group_by(course_id, role) %>%
      summarize(n_enrollments = n()) %>%
      pivot_wider(names_from = role, values_from = n_enrollments, values_fill = 0),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfFiles %>%
      group_by(course_id) %>%
      summarize(unique_files = n_distinct(id)),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfFolders %>%
      group_by(course_id) %>%
      summarize(unique_folders = n_distinct(id)),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfGroup_categories %>%
      group_by(course_id) %>%
      summarize(unique_group_categories = n_distinct(id)),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfGroups %>%
      group_by(course_id) %>%
      summarize(unique_groups = n_distinct(id)),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfMedia %>%
      group_by(course_id) %>%
      summarize(unique_media_objects = n_distinct(media_id)),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfModules %>%
      group_by(course_id) %>%
      summarize(unique_modules = n_distinct(id)),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfQuizzes %>%
      group_by(course_id) %>%
      summarize(unique_quizzes = n_distinct(id)),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfQuizzes %>%
      distinct(course_id, id, quiz_type) %>%
      group_by(course_id, quiz_type) %>%
      summarize(n_quizzes = n()) %>%
      pivot_wider(names_from = quiz_type, values_from = n_quizzes, values_fill = 0),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfSections %>%
      group_by(course_id) %>%
      summarize(unique_sections = n_distinct(id)),
    by = c("course.id" = "course_id")
  ) %>%
  left_join(
    dfUsers %>%
      distinct(course_id, id) %>%
      group_by(course_id) %>%
      summarize(unique_users = n_distinct(id)),
    by = c("course.id" = "course_id")
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


vusa::clear_script_objects()
