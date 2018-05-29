#' Restructure Looking While Listening (LWL) Data
#'
#' @param dataframe The dataframe (LWL output) that you want to modify. This MUST have the following columns: Subject, Condition (phonological, semantic, control), Trial, Timebin, Fixation (0, 0.5 or 1), target, competitor, distractor.
#' Your areas of interest (target, competitor, distractor) should be columns filled with 0s. These will populate via this function. 
#'
#' @return The same dataframes with the columns target, competitor, distractor filled based on specific parameters.
#' @export
#'
#' @examples
#' restructured_dataframe <- restructure_LWL(dataframe_name);
#' Be sure to create your area of interest columns that need to be labelled: target, competitor, distractor (case-sensitive). 
#' It should like like this: dataframe_name$target <- 0
#' Frankly, the reason this is a function is because the original R script is 200 lines. This is a VERY context specific function. 
restructure_LWL <- function(dataframe) {
  data_long <- dataframe
  
  num = c(1:nrow(data_long))
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="controlC"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="controlR"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="controlL"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="semanticCL"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="phonologicalCL"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="semanticCR"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="phonologicalCR"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="semanticRC"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="phonologicalRC"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="semanticLC"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="phonologicalLC"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="semanticRL"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="phonologicalRL"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="semanticLR"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  for (i in num) {
    data_long$target[i] <- ifelse(data_long$Condition[i]=="phonologicalLR"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$target[i]))
  }
  
  ## Assigning values to DISTRACTOR column
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="controlC"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="controlR"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="controlL"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="controlC"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="semanticCL"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="phonologicalCL"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="semanticCR"&&data_long$Fixation[i]==0,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="phonologicalCR"&&data_long$Fixation[i]==0,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="semanticRC"&&data_long$Fixation[i]==0,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="phonologicalRC"&&data_long$Fixation[i]==0,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="semanticLC"&&data_long$Fixation[i]==0,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="phonologicalLC"&&data_long$Fixation[i]==0,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="semanticRL"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="phonologicalRL"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="semanticLR"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  for (i in num) {
    data_long$distractor[i] <- ifelse(data_long$Condition[i]=="phonologicalLR"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$distractor[i]))
  }
  
  ## Assigning values to COMPETITOR column
  ## Did not do this for controlC, controlR,controlL because there are no competitors in these conditions
  for (i in num) {
    data_long$competitor[i] <- ifelse(data_long$Condition[i]=="semanticCL"&&data_long$Fixation[i]==0,1,ifelse(data_long$Fixation[i]==".",NA,data_long$competitor[i]))
  }
  
  for (i in num) {
    data_long$competitor[i] <- ifelse(data_long$Condition[i]=="phonologicalCL"&&data_long$Fixation[i]==0,1,ifelse(data_long$Fixation[i]==".",NA,data_long$competitor[i]))
  }
  
  for (i in num) {
    data_long$competitor[i] <- ifelse(data_long$Condition[i]=="semanticCR"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$competitor[i]))
  }
  
  for (i in num) {
    data_long$competitor[i] <- ifelse(data_long$Condition[i]=="phonologicalCR"&&data_long$Fixation[i]==1,1,ifelse(data_long$Fixation[i]==".",NA,data_long$competitor[i]))
  }
  
  for (i in num) {
    data_long$competitor[i] <- ifelse(data_long$Condition[i]=="semanticRC"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$competitor[i]))
  }
  
  for (i in num) {
    data_long$competitor[i] <- ifelse(data_long$Condition[i]=="phonologicalRC"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$competitor[i]))
  }
  
  for (i in num) {
    data_long$competitor[i] <- ifelse(data_long$Condition[i]=="semanticLC"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$competitor[i]))
  }
  
  for (i in num) {
    data_long$competitor[i] <- ifelse(data_long$Condition[i]=="phonologicalLC"&&data_long$Fixation[i]==0.5,1,ifelse(data_long$Fixation[i]==".",NA,data_long$competitor[i]))
  }
  
  for (i in num) {
    data_long$competitor[i] <- ifelse(data_long$Condition[i]=="semanticRL"&&data_long$Fixation[i]==0,1,ifelse(data_long$Fixation[i]==".",NA,data_long$competitor[i]))
  }
  
  for (i in num) {
    data_long$competitor[i] <- ifelse(data_long$Condition[i]=="phonologicalRL"&&data_long$Fixation[i]==0,1,ifelse(data_long$Fixation[i]==".",NA,data_long$competitor[i]))
  }
  
  for (i in num) {
    data_long$competitor[i] <- ifelse(data_long$Condition[i]=="semanticLR"&&data_long$Fixation[i]==0,1,ifelse(data_long$Fixation[i]==".",NA,data_long$competitor[i]))
  }
  
  for (i in num) {
    data_long$competitor[i] <- ifelse(data_long$Condition[i]=="phonologicalLR"&&data_long$Fixation[i]==0,1,ifelse(data_long$Fixation[i]==".",NA,data_long$competitor[i]))
  }
  
  print(data_long)
}
  