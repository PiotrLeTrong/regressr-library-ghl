rm(list = ls())
load("/Users/piotr/Dropbox/School/Spring 2017/670 Data Sci/DataSciProject/data/1.raw/footballdata.RData")
summarizer <- function(df){
  output <- data.frame()
  colcounter <- colnames(df)
  for(i in 1:ncol(df)){
    if (class(df[[i]]) == "factor"){
      df[[i]] <- as.character(df[[i]])
    }
    if (grepl("-|/[^A-z]", df[[i]][1],  perl=TRUE) == TRUE & class(df[[i]]) !="Date"){
      if(grepl("-|[^A-z]", df[[i]][1],  perl=TRUE) == TRUE & class(df[[i]]) !="Date"){
        df[[i]] <- as.Date(df[[i]], "%Y-%m-%d")
      } else {
        df[[i]] <- as.Date(df[[i]], "%Y/%m/%d")
      }
    }
    if (grepl("[A-z]", df[[i]][1],  perl=TRUE) == FALSE & class(df[[i]]) !="Date"){
      df[[i]] <- as.numeric(df[[i]])
    }
    if (min(df[[i]]) == 0 & max(df[[i]]) == 1){
      df[[i]] <- as.logical(df[[i]])
    }
    if (grepl("^False$|^FALSE$|^F$", df[[i]][1],  perl=TRUE) == TRUE | grepl("^True$|^TRUE$|^T$", df[[i]][1],  perl=TRUE) == TRUE){
      df[[i]] <- as.logical(df[[i]])
    }
    print(colcounter[i])
    output <- rbind(output, 
          data.frame(VarName = colcounter[i],
                     VarType = class(df[[i]])))
  }
  definitions <- data.frame(VarType = c("numeric",
                                        "character",
                                        "factor",
                                        "Date",
                                        "logical"),
                            VarDefinition = c("Numeric variables are straight forward: continous variables.",
                                              "These are categorical variables, thus are seen as discrete variables.",
                                              "These are categorical variables, similar to characters, but are ordered.",
                                              "Time Series Variable that makes the data set either time-series based or panel.",
                                              "These variables are True/False binary variables."))
  output <- merge(output, definitions, by = "VarType", all.x = T)
  output <- output[,c(2,1,3)]
  head(df)
  print(output)
  writeLines("\n Ok, it looks like you are ready to run the second function of the library that will help you define the dependent and independent variables.")
}
summarizer(cfb.scoring)