#' DIM HEAD TAIL
#'
#' Print the output of all of these functions at once. Redundant but useful.
#'
#' @param df dataframe that you wish to apply this function to. Can also call a vector/matrix with data.frame(df)
#'
#' @return returns the dataframe DIMensions rows then columns; HEAD output first 6 rows; TAIL output last 6 rows
#' @export
#'
#' @examples
#' dimheadtail(CogLin_Data)
#' dimheadtail(data.frame(CogLin_Data))
#'
dimheadtail <- function(df) {
  writeLines("Dataframe Size rows then columns")
  print(dim(df))
  writeLines("HEAD")
  print(head(df))
  writeLines("TAIL")
  print(tail(df))
}
