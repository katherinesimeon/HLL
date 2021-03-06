#' postprocess_LexDec
#'
#' For Lexical Decision Psychopy Data in fʌn.nɔlɛdʒi Exp. 1.1 (KS Dissertation) and DegPhon (CS Capstone)
#'
#' @param filename The Psychopy Output File. Should be a string (with '') with a .csv extension. Must be written with correct participant label conventions (e.g. DegPhon_Subject #_ED_L#)
#' @param order The order that was assigned to the participant. This must be numeric.
#' @param group A string that indicates a group distinction (for example, group can be age or whether the participant is a child or adult)
#'
#' @return
#' @export
#'
#' @examples
#' postprocess_LexDec(file.choose()) # Use this for ease to pick out the file you want to use

postprocess_LexDec <- function(filename,order,group) {
  data_fr_file <- read.csv(filename)
  ed <- data.frame(data_fr_file)

  # Get participant number
  subject <- as.character(ed$participant[1])

  # Get stuff that we need
  ed <- ed[2:(nrow(ed)-1),]
  ed <- ed[,1:(ncol(ed)-1)]
  ed1 <- ed[,1:2]
  ed2 <- ed[,9:10]
  full <- cbind(ed1,ed2)

  # Re-label columns
  colnames(full) <- c("audio","answer","response","RT-seconds")

  full$audio <- gsub(full$audio, pattern=".wav$", replacement="")


  # Accuracy column
  num = c(1:nrow(full))
  for (i in num) {
    full$Accuracy[i] <- ifelse(identical(full$answer[i],full$response[i]),1,0)
  }

  Subject_Col <- data.frame(do.call('rbind', strsplit(as.character(subject),'_',fixed=TRUE)))

  full$Subject <- rep(Subject_Col$X2,nrow(full))

  full <- cbind(full$Subject,full[,1:5])

  full$List <- rep(Subject_Col$X4,nrow(full))

  audio_sep <- data.frame(do.call('rbind', strsplit(as.character(full$audio),'_',fixed=TRUE)))

  full <- cbind(full,audio_sep[,1:2])

  colnames(full) <- c("Subject","audio","answer","response","RT-seconds","Accuracy","List","Word_Status","Probability")

  LD <- full

  LD <- add_manual_var(LD,"Order",as.numeric(order))
  LD <- add_manual_var(LD,"Group",as.character(group))

  total <- (sum(LD$Accuracy))

  LD <<- LD

  ### Output
  print(LD)
  print(paste0("Overall Accuracy: ",total,"/",nrow(LD)))
}
