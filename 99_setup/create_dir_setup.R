## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Education Analytics Vrije Universiteit Amsterdam
## Copyright 2024 VU
## Web Page: http://www.vu.nl
## Contact: vu-analytics@vu.nl
## Distribution outside of the VU: yes.
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Retrieve the project name from the current path
project_name <- this.path::sys.proj() %>%
  basename()

# Read the CSV file containing read and write settings
read_write_settings <- readr::read_delim("99_setup//read_and_write_settings.csv")

# Extract unique read directories, sort them, and store in sorted_read_dirs
sorted_read_dirs <- read_write_settings %>%
  pull(read_data_dir) %>%
  unique() %>%
  sort()

# Extract unique write directories, sort them, and store in sorted_write_dirs
sorted_write_dirs <- read_write_settings %>%
  pull(write_data_dir) %>%
  unique() %>%
  sort()

# Combine read and write directories, remove duplicates, and sort
combined_dirs <- c(sorted_read_dirs, sorted_write_dirs) %>%
  unique() %>%
  sort()

# Filter to keep only directories starting with a number
filtered_dirs <- combined_dirs[str_detect(combined_dirs, "^[0-9]")]

# Create a data frame with the basename of each directory
dfDirectories <- data.frame(dir_basename = filtered_dirs)

# Add a new column for the full directory path and check if it exists
dfDirectories <- dfDirectories %>%
  mutate(
    full_dir_path = paste0(Sys.getenv("OUTPUT_DIR"), "_REPOSITORIES/", project_name, "/", Sys.getenv("BRANCH"), "/", dir_basename, "/"),
    path_exists = dir.exists(full_dir_path)
  )

# Identify new paths that do not exist yet
new_paths_to_create <- dfDirectories %>%
  dplyr::filter(!path_exists) %>%
  pull(full_dir_path)

# Create new directories for paths that do not exist
if (!is_empty(new_paths_to_create)) {
  map(new_paths_to_create, ~ dir.create(.x, recursive = TRUE))
}

# Update the OUTPUT_DIR environment variable to include the project name
project_specific_output_dir <- paste0(Sys.getenv("OUTPUT_DIR"), "_REPOSITORIES/", project_name, "/")
Sys.setenv(OUTPUT_DIR = project_specific_output_dir)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vusa::clear_script_objects()
