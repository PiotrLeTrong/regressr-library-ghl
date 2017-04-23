rm(list = ls())
load("/Users/piotr/Dropbox/School/Spring 2017/670 Data Sci/DataSciProject/data/1.raw/footballdata.RData")

functiontres <- function(modelType, 
                         dependentVar, 
                         independentVar, 
                         df,
                        logDepen = F, 
                        logIndepen = NULL, 
                        squareIndepend = NULL){
  for(i in 1:ncol(df)){
    if(class(df[[i]]) == "factor"){
      df[[i]] <- as.character(df[[i]])
    }
    if(grepl("[0-9]", df[[i]][runif(1,1,nrow(df))]) == TRUE & grepl("-|/[^A-z]", colVect[runif(1,1,nrow(df))],  perl=TRUE) == FALSE){
      df[[i]] <- as.numeric(df[[i]])
    }
  }
  modelType <- tolower(modelType)
  if (logDepen == T){
    dependentVar <- deparse(substitute(dependentVar))
    df[,dependentVar] <- log(df[,dependentVar]) 
  }
  if(modelType == "ols"){
    
  } else if (modelType == "probit") {
    
  } else if (modelType == "logit") {
    
  } else if (modelType == "tobit") {
    
  } else if (modelType == "multinominal probit") {
    
  } else if (modelType == "multinominal logit") {
    
  } else{
    writeLines("\n Unsupported model type, please try one of the followwing:
               \n - OLS
               \n - probit
               \n - logit
               \n - tobit
               \n - multinominal probit
               \n - multinominal logit")
  }
}

test <- function(df, dependentVar){
  for(i in 1:ncol(df)){
    if(class(df[[i]]) == "factor"){
      df[[i]] <- as.character(df[[i]])
    }
    if(grepl("[0-9]", df[[i]][runif(1,1,nrow(df))]) == TRUE & grepl("-|/[^A-z]", df[[i]][runif(1,1,nrow(df))],  perl=TRUE) == FALSE){
      df[[i]] <- as.numeric(df[[i]])
    }
  }
  dependentVar <- deparse(substitute(dependentVar))
  df[,dependentVar] <- log(df[,dependentVar])
  print(df)
}
test(df = cfb.scoring, dependentVar = offensive.G)

