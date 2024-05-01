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

dfCourses <- vvcanvas::get_all_courses(canvas)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfStudents <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_students(canvas, .x))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfAssignments <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_assignments(canvas, .x))


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfFiles <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_files(canvas, .x))


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfAnnouncements <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_announcements(canvas, .x))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfQuizzes <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_quizzes(canvas, .x))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfDiscussions <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_discussions(canvas, .x))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfModules <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_modules(canvas, .x))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfCourse_details <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_details(canvas, .x))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfEnrollments <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_enrollments(canvas, .x))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfFolders <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_folders(canvas, .x))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfGroups <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_groups(canvas, .x))


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfSections <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_sections(canvas, .x))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfGroup_categories <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_group_categories(canvas, .x))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfAssignment_groups <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_assignment_groups(canvas, .x))



## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfUsers <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_users(canvas, .x))

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfMedia <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_media_objects(canvas, .x))


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^





## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##' *TODO*
##' Write to file

vusa::clear_script_objects()
