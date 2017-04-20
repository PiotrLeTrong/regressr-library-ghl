rm(list = ls())
load("/Users/piotr/Dropbox/School/Spring 2017/670 Data Sci/DataSciProject/data/1.raw/footballdata.RData")

summarizer <- function(x) {
  output <- data.frame()
  colcounter <- colnames(x)
  for(i in 1:ncol(x)){
    if (class(x[[i]]) == "factor"){
      x[[i]] <- as.character(x[[i]])
    }
    if (grepl("[A-z]", x[[i]][1],  perl=TRUE) == FALSE){
      x[[i]] <- as.numeric(x[[i]])
    }
    
    if (min(x[[i]]) == 0 & max(x[[i]]) == 1){
      x[[i]] <- as.logical(x[[i]])
    }
    output <- rbind(output, 
          data.frame(VarName = colcounter[i],
                     VarType = class(x[[i]])))
  }
  output
} #Look at GoogleDrive File for updates

output <- summarizer(cfb.scoring)
