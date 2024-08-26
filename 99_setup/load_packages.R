## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact:
##
##' *INFO*:
## 1) Loads packages for the project using library()
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Define the basic packages
basic_packages <- c(
  "cli",            # Used to add color to console messages
  "janitor",        # Used to clean up variable names from special characters
  "lubridate",      # Used to work with dates and times
  "purrr",          # Used to work with functions and vectors
  "stringr",        # Used for functions to work with strings
  "tibble",         # Used for editing and creating tibbles
  "tidyr",          # Used to clean data in the tidyverse environment
  "utils",          # Used for utility functions
  "styler",         # Used for improving the style of script
  "dplyr",         # Used for the dplyr environment
  "readr",          # Used to read in data
  "vusa",           # Mainly to always have the addins
  "dplyr",           # Used for the dplyr environment
  "officer"
)

# Load the packages into the library
suppressMessages(purrr::walk(basic_packages, ~library(.x, character.only = TRUE, warn.conflicts = FALSE)))

# if (interactive()) {
#   library(tidylog)  # Used to log messages in the console in interactive mode
# }


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vusa::clear_script_objects()
