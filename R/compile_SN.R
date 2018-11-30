#' Post-process Semantic Norming Data for KS Dissertation
#' Post-process Semantic Norming from PsychoPy for Exp 2 
#'
#' @param filename The Psychopy Output File. Should be a string (with '') with a .csv extension.
#'
#' @return A cleaned dataframe with individual trials for a given participant.
#' @export
#'
#' @examples
#' postprocess_FUNK('FK_Pilot2_SN2_testrun_2018_Nov_18_1023.csv')
#' This function was originally designed to compile semantic norming data for KS Dissertation.
#' Provides an "Expectancy Column" which is 1=matches intended semantic relation or 0=does NOT match intended semantic relation
compile_SN <- function(filename) {
  data_fr_file <- read.csv(filename)
  SN <- data.frame(data_fr_file)
  
  SN <- SN[2:(nrow(SN)-1),]
  SN1 <- SN[,1:5]
  SN2 <- cbind(SN$mouse.RT,as.character(SN$Location),as.character(SN$participant))
  
  SNorm <- cbind(SN2[,3],SN1,SN2[,1:2])
  
  colnames(SNorm) <- c("Subject","Competitor_Condition","Left_Pic","Right_Pic","Answer_Location","Center_Pic","RT_Seconds","Selection_Location")
  
  Subject_Col <- data.frame(do.call('rbind', strsplit(as.character(SNorm$Subject),'_',fixed=TRUE)))
  
  SNorm <- cbind(Subject_Col[,2:3],SNorm[,2:8])
  
  colnames(SNorm) <- c("Subject","List","Competitor_Condition","Left_Pic","Right_Pic","Answer_Location","Center_Pic","RT_Seconds","Selection_Location")
  
  SNorm
  
  # Get rid if extensions and other unnecessary text
  SNorm$Right_Pic <- gsub(SNorm$Right_Pic, pattern=".jpg$", replacement="")
  SNorm$Left_Pic <- gsub(SNorm$Left_Pic, pattern=".jpg$", replacement="")
  SNorm$Center_Pic <- gsub(SNorm$Center_Pic, pattern=".jpg$", replacement="")
  
  
  ##### Add Expectancy Column
  num = c(1:nrow(SNorm))
  for (i in num) {
    SNorm$Expected[i] <- ifelse(identical(as.character(SNorm$Answer_Location[i]),as.character(SNorm$Selection_Location[i])),1,0)
  }
  
  norming_ans <<- SNorm
  print(SNorm)
}