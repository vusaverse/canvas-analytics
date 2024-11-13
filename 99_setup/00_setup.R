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

## Get current branch
Sys.setenv("BRANCH" = system("git branch --show-current", intern = TRUE))

## Restore packages from renv
renv::restore(prompt = FALSE)

source("99_setup/load_packages.R")
source("99_setup/load_systemvariables.R")
source("99_setup/create_dir_setup.R")
source("99_setup/read_and_write_settings_functions.R")
source("99_setup/connect_to_slack.R", print.eval = TRUE)

##'* INFO*
##' Authenticate with the Canvas LMS API
##' Assumes that the Canvas API token is stored in the environment variable CANVAS_API_TOKEN
##' Assumes that the Canvas API URL is stored in the environment variable CANVAS_BASE_URL
##' https://vusaverse.github.io/vvcanvas/reference/canvas_authenticate.html
canvas <- vvcanvas::canvas_authenticate()

##' *INFO* clear_global_proj
object_names <- ls(envir = .GlobalEnv)

# Concatenate the object names into a space-separated string
default_keep_list <- paste(object_names, collapse = " ")

# Set the environment variable
Sys.setenv(DEFAULT_KEEP_LIST = default_keep_list)

vusa::clear_global_proj()

##'* INFO*
##' All possible settings:
##' https://docs.posit.co/ide/server-pro/reference/session_user_settings.html
##' https://docs.posit.co/ide/server-pro/rstudio_pro_sessions/session_startup_scripts.html
##' Enforce margin of 100; use rstudio.sessionInit hook as RStudio needs to be initiated.
setHook("rstudio.sessionInit", function(newSession) {
  if (newSession) {
    vusa::use_rstudio_prefs_silent(
      "margin_column" = as.integer(100)
    )
  }
}, action = "append")
