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
to_set <- readxl::read_xlsx("Z:/Datasets/Documentatie/Project/renviron_azure_test.xlsx")

## zet variabelen in R system variables
pmap(list(to_set$variable, to_set$value), set_all_envs)
