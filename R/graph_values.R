#' Compute values for graphing
#'
#' Takes dataframe and calculates descriptive statistics for a given dependent variable that is formatted for KS graphing with ggplot2
#'
#' @param df Dataframe. What data you want to use. Dataframe should be formatted so that each row has a dependent variable marked with the condition that you wish to sortby
#' @param sort_by String or list. What conditions you want your data summarized by; e.g. average by SNR etc.; can be a character "quotes" or a list c("a","b")
#' @param dep_var Dependent variable, column name that exists in dataframe
#'
#' @return datagrame that lists the N/sample size, mean, std deviation, std error
#' @export
#'
#' @examples
#' graph_values(CogLin_Data,"Noise_Condition",Percent)
#'
graph_values <- function(df,sort_by,dep_var){
  dep_var <- enquo(dep_var)
  ddply(df, sort_by, summarise,
        N    = sum(!is.na(!!dep_var)),
        mean = mean(!!dep_var, na.rm=TRUE),
        sd   = sd(!!dep_var, na.rm=TRUE),
        se   = sd / sqrt(N)
  )
}
