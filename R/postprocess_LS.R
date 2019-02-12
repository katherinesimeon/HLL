#' Post-process LEXICAL SEMANTIC (R56)
#'
#'
#' Post-process behavioral data for Lexical semantic task.
#'
#' @param filename The Psychopy Output File. Should be a string (with '') with a .csv extension.
#' @param AV_Condition A string that indicates whether the AV condition was AV (audio + visual) or AO (audio only)
#'
#' @return  A cleaned dataframe with individual trials for a given participant. AV_Condition will be added based on input parameters.
#' @export
#'
#' @examples
#' postprocess_LS('LS001_A2_testrun_2018_Feb_17_1411.csv','AV')
postprocess_LS <- function(filename,AV_Condition) {
  data_fr_file <- read.csv(filename)
  LS <- data.frame(data_fr_file)

  LS <- LS[2:21,]
  LS1 <- LS[,1:6]
  LS2 <- cbind(LS$mouse.RT,as.character(LS$Location),as.character(LS$participant))

  LexSem <- cbind(LS2[,3],LS1,LS2[,1:2])

  colnames(LexSem) <- c("Subject","Target","Right_Pic","Center_Pic","Answer_Location","Competitor_Condition","Left_Pic","RT_Seconds","Selection_Location")

  Subject_Col <- data.frame(do.call('rbind', strsplit(as.character(LexSem$Subject),'_',fixed=TRUE)))

  LexSem <- cbind(Subject_Col,LexSem[,2:9])

  colnames(LexSem) <- c("Subject","List","Target","Right_Pic","Center_Pic","Answer_Location","Competitor_Condition","Left_Pic","RT_Seconds","Selection_Location")


  # Get rid if extensions and other unnecessary text
  LexSem$Right_Pic <- gsub(LexSem$Right_Pic, pattern=".jpg$", replacement="")
  LexSem$Left_Pic <- gsub(LexSem$Left_Pic, pattern=".jpg$", replacement="")
  LexSem$Center_Pic <- gsub(LexSem$Center_Pic, pattern=".jpg$", replacement="")
  LexSem$Target <- gsub(LexSem$Target, pattern=".mp4$", replacement="")
  LexSem$Target <- gsub(LexSem$Target, pattern="^Find the ", replacement="")
  LexSem$Target <- gsub(LexSem$Target, pattern="^Find_the_", replacement="")

  ##### Add Accuracy Column
  num = c(1:20)
  for (i in num) {
    LexSem$Accuracy[i] <- ifelse(identical(as.character(LexSem$Answer_Location[i]),as.character(LexSem$Selection_Location[i])),1,0)
  }

  LexSem_Final <- add_manual_var(LexSem,"AV_Condition",as.character(AV_Condition))

  Lexical_Semantic <<- LexSem_Final
  print(LexSem_Final)
}
