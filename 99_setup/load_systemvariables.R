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
set_all_envs <- function(var.name, var.value) {
  args = list(var.value)
  names(args) = var.name
  do.call(Sys.setenv, args)
}

## Lees in systeemvariabelen excel bestand
to_set <- readxl::read_xlsx("G:/DSZ/SA2016/Datasets/Documentatie/Project/renviron.xlsx")

## Set SHAREPOINT_DIR
Sys.setenv(SHAREPOINt_DIR = paste0("C:/Users/", Sys.getenv("USERNAME"), "/Vrije Universiteit Amsterdam/"))

Sys.setenv(DOCUMENTATION_DIR =
             paste0(Sys.getenv("SHAREPOINt_DIR"), "Education Analytics - General/01_Documentatie/XX. Data Documentatie/"))

Sys.setenv(MAP_TABLE_DIR =
             paste0(Sys.getenv("SHAREPOINt_DIR"), "Education Analytics - General/01_Documentatie/XX. Data Documentatie/Mapping Tables/"))

## zet variabelen in R system variables
pmap(list(to_set$variable, to_set$value), set_all_envs)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vusa::clear_script_objects()
