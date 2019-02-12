#' Post-process CONTEXT data (KS QRP)
#'
#' @param filename The Psychopy Output File. Should be a string (with '') with a .csv extension.
#' @param noise_condition A string that indicates whether the listening condition was in noise or in quiet.
#'
#' @return A cleaned dataframe with individual trials for a given participant. The overall accuracy for the subject in percent by semantic expectancy condition (i.e.: Congruent (sem), Conflicitng (nonsem), Neutral). Can also be used for CONTEXT RHYME DATA
#' @export
#'
#' @examples
#' postprocess_Context('Context07_12SNR_testrun_2016_Jul_21_1600.csv',"noise")
#'
#'
postprocess_Context <- function(filename,noise_condition) {
  df = read.csv(filename)
  f <- data.frame(df) # make data frame

  subject <- as.character(f$participant[1])
  f <- f[2:62,]
  f <- f[,1:19]
  f1 <- f[,1:8]
  f2 <- f[,14:19]
  ftotal <- cbind(f1,f2)

  last_row <- nrow(ftotal)-1
  full <- ftotal[1:last_row,] # remove last row of dataframe

  num = c(1:nrow(full))

  for (i in num) {
  full$AdjRT[i] <- full$mouse.RT[i] - (0.1 + full$Sound.start..sec.[i] + full$length[i])
  }

  # determine accuracy of responses
  f_ans <- recode(full$ans,"'left' = 1 ; 'right' = 2 ; 'upperleft' = 3 ; 'upperright' = 4")
  f_loc <- recode(full$Location,"'left' = 1 ; 'right' = 2 ; 'upperleft' = 3 ; 'upperright' = 4")
  f_ans <- as.matrix(f_ans)
  f_loc <- as.matrix(f_loc)

  # add new column for accuracy of responses
  num = c(1:nrow(full))
  for (i in num) {
  full$Acc[i] <- ifelse(identical(f_ans[i],f_loc[i]),1,0)
  }


  # data tables for wrong answers
  wrong <- subset(full,full$Acc == 0)
  err_sent <- table(wrong$sentence)

  # separate dataframes for each sentence type
  sem <- full[grep("^sem$", full$sentence), ]
  nonsem <- full[grep("^nonsem$", full$sentence), ]
  neut <- full[grep("^neut$", full$sentence), ]

  # mean values for accuracy and RT for each semantic context
  mRT <- c(mean(sem$AdjRT), mean(nonsem$AdjRT), mean(neut$AdjRT))

  noise_condition <- tolower(noise_condition)

  acc <- ifelse(noise_condition=="noise",c((sum(sem$Acc)/20), (sum(nonsem$Acc)/20), (sum(neut$Acc)/20)),ifelse(noise_condition=="quiet",c((sum(sem$Acc)/10), (sum(nonsem$Acc)/10), (sum(neut$Acc)/10)),"Error"))

  describe <- rbind(mRT,acc)
  colnames(describe) <- c('Congruent', 'Conflicting', 'Neutral')

  full$Subj <- rep(subject,nrow(full))

  num <- c(1:nrow(full))
  for (i in num) {
    var <- as.character(full$Location[i])
    coldat <- full[[var]]
    full$choice[i] <- as.character(coldat[i])
  }

  full_rev <- cbind(full$Subj,as.character(full$audio),as.character(full$sentence),as.character(full$ans),as.character(full$Location),as.character(full$choice),full$AdjRT,full$Acc)
  full_rev <- data.frame(full_rev)
  colnames(full_rev) <- c("Subject","audio","sentence","ans_pos","sel_pos","selection","AdjRT","Acc")
  full_rev

  ID_break <- data.frame(do.call('rbind', strsplit(as.character(full_rev$Subject),'_',fixed=TRUE)))
  ID_break
  full_final <- cbind(ID_break$X2,ID_break$X3,full_rev[,2:8])

  colnames(full_final) <- c("Subject","SNR","audio","sentence","ans_pos","sel_pos","selection","AdjRT","Acc")
  full_final

  Context <<- full_final

  print(round(describe,3)) # save combined matrix to subject name
}



