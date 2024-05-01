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

## check course_id
dfStudents %>%
  tabyl(course_id)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfAssignments <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_assignments(canvas, .x))

## check course_id
dfAssignments %>%
  tabyl(course_id)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfFiles <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_files(canvas, .x))

## check course_id
dfFiles %>%
  tabyl(course_id)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfAnnouncements <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_announcements(canvas, .x))

## check course_id
dfAnnouncements %>%
  tabyl(course_id)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfQuizzes <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_quizzes(canvas, .x))

## check course_id
dfQuizzes %>%
  tabyl(course_id)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfDiscussions <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_discussions(canvas, .x))

## check course_id
dfDiscussions %>%
  tabyl(course_id)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfModules <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_modules(canvas, .x))

## check course_id
dfModules %>%
  tabyl(course_id)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfCourse_details <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_details(canvas, .x))

## check course_id
dfCourse_details %>%
  tabyl(id)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfEnrollments <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_enrollments(canvas, .x))

## check course_id
dfEnrollments %>%
  tabyl(course_id)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfFolders <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_folders(canvas, .x))

## check course_id
dfFolders %>%
  tabyl(course_id)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfGroups <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_groups(canvas, .x))

## check course_id
dfGroups %>%
  tabyl(course_id)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfSections <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_sections(canvas, .x))

## check course_id
dfSections %>%
  tabyl(course_id)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfGroup_categories <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_group_categories(canvas, .x))

## check course_id
dfGroup_categories %>%
  tabyl(course_id)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfAssignment_groups <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_assignment_groups(canvas, .x))

## check course_id
dfAssignment_groups %>%
  tabyl(course_id)



## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^




dfUsers <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_users(canvas, .x))

## check course_id
dfUsers %>%
  tabyl(course_id)

## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dfMedia <- dfCourses %>%
  pull(course.id) %>%
  map_dfr(~ vvcanvas::get_course_media_objects(canvas, .x))

## check course_id
dfMedia %>%
  tabyl(course_id)


## ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^





## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##' *TODO*
##' Write to file

vusa::clear_script_objects()
