rm(list = ls())
load("/Users/piotr/Dropbox/School/Spring 2017/670 Data Sci/regressr-library-ghl/data/1.raw/footballdata.RData")

for(i in 1:ncol(cfb.scoring)){
  if(class(cfb.scoring[[i]]) == "factor"){
    cfb.scoring[[i]] <- as.character(cfb.scoring[[i]])
  }
  if(grepl("[0-9]", cfb.scoring[[i]])==TRUE | cfb.scoring[[i]]==""){
    cfb.scoring[[i]] <- as.numeric(cfb.scoring[[i]])
  }
}
# test test test
interpreter <- function(df,
                        modelType, 
                        dependentVar, 
                        independentVar, 
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
  modelType <- tolower(modelType)
  if (logDepen == T){
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
  modelSpec <- paste(dependentVar, "~", paste(independentVar, collapse = " + "))
  modelSpec
  if(modelType == "ols"){
    output <- lm(noquote(modelSpec), df)
    summary(output)
    varNames <-   row.names(coef(summary(output)))
    coefficients <- as.numeric(coef(summary(output))[,1])
    error <- as.numeric(coef(summary(output))[,2])
    tVal <- as.numeric(coef(summary(output))[,3])
    pVal <- as.numeric(coef(summary(output))[,4])
    for(i in 2:length(varNames)){
      if(pVal[i] < 1.95){
        writeLines(paste("In this regression the independent variable", varNames[i],
                         " has a statistically significant impact on the dependent variable",
                         "a 1 unit increase in the independent variable causes a ", sprintf("%.3f",coefficients[i]),
                         "increase in the dependent variable. \n"))
      } else {
        writeLines(paste("In this regression the independent variable", varNames[i],
                         " does not have  a statistically significant relationship with the dependent variable"))
      }
    }
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
  } else {
    writeLines("\n Unsupported model type, please try one of the followwing:
               \n - OLS
               \n - probit
               \n - logit
               \n - tobit
               \n - multinominal probit
               \n - multinominal logit")
  }
}

test <- function(df,
                        modelType, 
                        dependentVar, 
                        independentVar){
  #dependentVar <- deparse(substitute(dependentVar))
  #independentVar <- deparse(substitute(independentVar))
  

}

interpreter(df = cfb.scoring, modelType = "ols", dependentVar = "offensive.TD", independentVar =  c("defensive.TD", "defensive.FG"))
