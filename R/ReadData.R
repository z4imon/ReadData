read_data_file <- function(file_path) {

  file_extension <- tools::file_ext(file_path)

  data <- switch(tolower(file_extension),
                 "csv" = read.csv(file_path),
                 "tsv" = handle_tsv(file_path),
                 "xls" = handle_excel(file_path),
                 "xlsx" = handle_excel(file_path),
                 "json" = handle_json(file_path),
                 "rds" = handle_rds(file_path),
                 stop("Unsupported file format: ", file_extension))
  return(data)
}

write_data_file <- function(data, file_path) {

  file_extension <- tools::file_ext(file_path)

  switch(tolower(file_extension),
         "csv" = write.csv(data, file_path, row.names = FALSE),
         "tsv" = write_tsv(data, file_path),
         "xls" = write_excel(data, file_path),
         "xlsx" = write_excel(data, file_path),
         "json" = write_json(data, file_path),
         "rds" = write_rds(data, file_path),
         stop("Unsupported file format: ", file_extension))

  message("Data successfully written to ", file_path)
}

handle_tsv <- function(file_path) {
  data <- read.table(file_path, header = TRUE, sep = "\t")
  return(data)
}

write_tsv <- function(data, file_path) {
  write.table(data, file_path, sep = "\t", row.names = FALSE, col.names = TRUE)
}

handle_excel <- function(file_path) {
  library(readxl)
  data <- readxl::read_excel(file_path)
  class(data) <- "data.frame"
  return(data)
}

write_excel <- function(data, file_path) {
  library(openxlsx)
  openxlsx::write.xlsx(data, file_path)
}

handle_json <- function(file_path) {
  library(jsonlite)
  data <- fromJSON(file_path)
  return(data)
}

write_json <- function(data, file_path) {
  library(jsonlite)
  jsonlite::write_json(data, file_path)
}

handle_rds <- function(file_path) {
  data <- readRDS(file_path)
  return(data)
}

write_rds <- function(data, file_path) {
  saveRDS(data, file_path)
}
