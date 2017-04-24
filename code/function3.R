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
    if(grepl("[0-9]", df[[i]][runif(1,1,nrow(df))]) == TRUE & grepl("-|/[^A-z]", df[[i]][runif(1,1,nrow(df))],  perl=TRUE) == FALSE){
      df[[i]] <- as.numeric(df[[i]])
    }
  }
  modelType <- deparse(substitute(modelType))
  modelType <- tolower(modelType)
  if (logDepen == T){
    dependentVar <- deparse(substitute(dependentVar))
    df[,dependentVar] <- log(df[,dependentVar])
    df[,dependentVar]
  }
  if (length(logIndepen) > 0){
    for(i in 1:length(depenVect)){
      name <-depenVect[i]
      df[,name] <- df[,name]^2
      df[,name]
    }
  }
  # if(modelType == "ols"){
  #   
  # } else if (modelType == "probit") {
  #   
  # } else if (modelType == "logit") {
  #   
  # } else if (modelType == "tobit") {
  #   
  # } else if (modelType == "multinominal probit") {
  #   
  # } else if (modelType == "multinominal logit") {
  #   
  # } else{
  #   writeLines("\n Unsupported model type, please try one of the followwing:
  #              \n - OLS
  #              \n - probit
  #              \n - logit
  #              \n - tobit
  #              \n - multinominal probit
  #              \n - multinominal logit")
  # }
  print(df)
}
cfb.scoring.tiny <- cfb.scoring[,1:4]

test(cfb.scoring, offensive.G)

functiontres(ols, offensive.G, offensive.TD, cfb.scoring.tiny, T)