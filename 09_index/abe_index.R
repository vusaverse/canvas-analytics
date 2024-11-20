folder_Abe <- "02_explore/abe"

script_files <- data.frame("files" = list.files(folder_Abe, pattern = "\\.R$", full.names = TRUE))

map(script_files$files, ~tryCatch(
  source(.x),
  error = function(e) {
    paste("Error in file:", .x, "->", e$message)
  }
))

