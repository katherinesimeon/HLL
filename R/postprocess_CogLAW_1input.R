#' Postprocess CogLAW data - 1 INPUT ONLY
#'
#' FORCED CHOICE & 1 INPUT ONLY. Written by KS March 2018 for CS Thesis CogLAW - Cognitive Learning Attributes & Words
#'
#' @param filename The Psychopy Output File. Should be a string (with '') with a .csv extension.
#'
#' @return a cleaned dataframe with individual trials for a given participant. The overall accuracy for the subject in percent, accuracy for attributes vs. labels, & the accuracy by each target word
#' @export
#'
#' @examples
#' postprocess_CogLAW('CogLAW_002_Order1A.csv')
#' Note that this does not allow you to add attribute and age group which is not automated in the PsychoPy file
#'
postprocess_CogLAW_1input <- function(filename) {
  data_fr_file <- read.csv(filename)
  CL <- data.frame(data_fr_file)

  CL
  dim(CL)

  ## Get the information you need
  CL_1 <- CL[2:19,1:6]
  CL_2 <- cbind(CL$mouse.RT,as.character(CL$Location),as.character(CL$participant))
  CL_2_new <- data.frame(CL_2[2:19,])

  CogLAW <- cbind(CL_1,CL_2_new) # New dataframe with relevant information

  # Get rid if extensions and other unnecessary text
  CogLAW$right <- gsub(CogLAW$right, pattern=".jpg$", replacement="")
  CogLAW$left <- gsub(CogLAW$left, pattern=".jpg$", replacement="")
  CogLAW$center <- gsub(CogLAW$center, pattern=".jpg$", replacement="")
  CogLAW$audio <- gsub(CogLAW$audio, pattern=".wav$", replacement="")
  CogLAW$audio <- gsub(CogLAW$audio, pattern="^where_is_the_", replacement="")
  CogLAW$audio <- gsub(CogLAW$audio, pattern="^where_is_the_", replacement="")
  CogLAW$audio <- gsub(CogLAW$audio, pattern="^which_one_can_", replacement="")

  CogLAW_v1 <- CogLAW # Save current dataframe as is just in case
  # Name columns for clarity
  colnames(CogLAW) <- c("Right_Image","Center_Image","Target_Location","Word","Question","Left_Image","RT","Response","Subject")

  ##### Reformat Subject Column
  Subject_Col <- data.frame(do.call('rbind', strsplit(as.character(CogLAW$Subject),'_',fixed=TRUE)))
  Subject_Col

  CogLAW <- cbind(Subject_Col[,2:3],CogLAW[,1:8])

  # Rename columns for a final time
  colnames(CogLAW) <- c("Subject","Order","Right_Image","Center_Image","Target_Location","Word","Question","Left_Image","RT","Response")
  CogLAW$Order <- gsub(CogLAW$Order, pattern="^ORDER", replacement="")


  ##### Add Accuracy Column
  num = c(1:18)
  for (i in num) {
    CogLAW$Accuracy[i] <- ifelse(identical(as.character(CogLAW$Target_Location[i]),as.character(CogLAW$Response[i])),1,0)
  }

  ##### Accuracy Calculations
  # Overall Accuracy (across labels and attributes)
  total <- (sum(CogLAW$Accuracy)/18)*100
  # Accuracy by question type
  By_Question <- ddply(CogLAW, c("Question"), summarise,
                       Acc    = sum(Accuracy),
                       Acc_Percentage = (Acc/9)*100
  )
  # Accuracy by target word
  By_Word <- ddply(CogLAW, c("Word","Question"), summarise,
                   Acc    = sum(Accuracy),
                   Acc_Percentage = (Acc/3)*100
  )

  CogLAW <<- CogLAW
  ##### Output
  print(CogLAW)
  print(paste0("Overall Accuracy: ",round(total,digits=2)))
  print(By_Question)
  print(By_Word)
}
