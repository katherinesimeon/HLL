#' Postprocess fʌn.nɔlɛdʒi - EXP 2 (KS Dissertation)
#'
#' Post-process endpoint LWL accuracy from PsychoPy for fʌn.nɔlɛdʒi Exp. 2 (KS Dissertation)
#' Includes separate column for semantic prime. Also has a trial column for merging.
#'
#' @param filename The Psychopy Output File. Should be a string (with '') with a .csv extension.
#'
#' @return A cleaned dataframe with individual trials for a given participant.
#' @export
#'
#' @examples
#' postprocess_LWL_E2(file.choose(),"NH")
postprocess_LWL_E2<- function(filename,group) {
  data_fr_file <- read.csv(filename)
  E <- data.frame(data_fr_file)

  # Get participant number
  subject <- as.character(E$participant[1])

  # Get stuff that we need
  E <- E[2:(nrow(E)),]
  E <- E[,1:(ncol(E)-1)]
  E1 <- E[,1:6]
  E2 <- E[,13:16]
  full <- cbind(E1,E2)

  # Re-label columns
  colnames(full) <- c("prime","audio","left","right","answer","competitor","RTseconds","mouse.x","mouse.y","response")

  full$audio <- gsub(full$audio, pattern=".wav$", replacement="")
  full$left <- gsub(full$left, pattern=".jpg$", replacement="")
  full$right <- gsub(full$right, pattern=".jpg$", replacement="")


  # Accuracy column
  num = c(1:nrow(full))
  for (i in num) {
    full$Accuracy[i] <- ifelse(identical(full$answer[i],full$response[i]),1,0)
  }

  Subject_Col <- data.frame(do.call('rbind', strsplit(as.character(subject),'_',fixed=TRUE)))

  full$Exp <- rep(Subject_Col$X3,nrow(full))
  full$Subject <- rep(Subject_Col$X2,nrow(full))
  full$Trial <- 1:nrow(full)

  full <- cbind(full$Subject,full$Trial,full[,1:12])


  colnames(full) <- c("Subject","Trial","prime","audio","left_pic","right_pic",
                      "answer","competitor","RTsec","mouse.x","mouse.y",
                      "response","Accuracy","Experiment")

  Exp2 <- full

  Exp2 <- add_manual_var(Exp2,"Group",as.character(group))

  Exp2 <<- Exp2

  ### Output
  print(Exp2)
}
