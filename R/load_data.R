#' Load Data
#'
#' Imports multiple .csv files into R from a single, specified directory.
#' Files should all be .csv format and have the same columns and names.
#'
#' @param path the file path where all of your files are stored
#'
#' @return a data frame with all files concatenated
#' @export
#'
#' @examples
load_data <- function(path) {
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}
