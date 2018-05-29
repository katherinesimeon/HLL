#' DIM HEAD - Print the output of all of these functions at once
#'
#' @param df dataframe that you wish to apply this function to. Can also call a vector/matrix with data.frame(df)
#'
#' @return returns the dataframe DIMensions rows then columns & HEAD output first 6 rows
#' @export
#'
#' @examples
#' dimhead(CogLin_Data)
#' dimhead(data.frame(CogLin_Data))
dimhead <- function(df) {
  writeLines("Dataframe Size rows then columns")
  print(dim(df))
  writeLines("FIRST 6 ROWS")
  print(head(df))
}
