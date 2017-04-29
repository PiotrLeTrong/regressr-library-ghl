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
package.test <- function(package){
  for(p in 1:length(package)){
    test <- c()
    test <- installed.packages()[,1] == package[p]
    if (any(test) == TRUE){
      pack <- package[p]
      require(pack, character.only = TRUE)
    } else {
      pack <- package[p]
      install.packages(package[p])
      require(pack, character.only = TRUE)
    }
  }
}

package.test(c("digest", "AER"))
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
  } else if (modelType == "probit") {
    output <- glm(formula = noquote(modelSpec), family = binomial(link = "probit"), data = df)
    summary(output)
    varNames <-   row.names(coef(summary(output)))
    coefficients <- as.numeric(coef(summary(output))[,1])
    error <- as.numeric(coef(summary(output))[,2])
    tVal <- as.numeric(coef(summary(output))[,3])
    pVal <- as.numeric(coef(summary(output))[,4])
    print(coefficients)
    print(error)
    print(tVal)
    print(pVal)
    print(summary(output))
    for(i in 2:length(varNames)){
      if(pVal[i] < 1.95){
        if(class(df[,varNames[i]]) == "logical"){
          for(j in 2:length(varNames)){
            if(i!=j){
              a <- coefficients[j] * mean(df[, varNames[j]])
            }
          }
          marginpctchange <-pnorm(a + coefficients[i] * (1 * mean(df[, varNames[i]]))) - pnorm(coefficients[i] * (0 *mean(df[, varNames[i]])))
          
          baselinepct <- pnorm(coefficients[1])
          writeLines(paste("In this regression the independent variable", varNames[i],
                           " has a statistically significant impact on the dependent variable",
                           "a 1 Standard Deviation increase in the independent variable causes a ", sprintf("%.3f",marginpctchange),
                           "percentage increase in the dependent variable. All of this is based on a baseline probability of"
                           ,sprintf("%.3f",baselinepct),"\n"))
        } else {
          for(j in 2:length(varNames)){
            if(i!=j){
              a <- coefficients[j] * mean(df[, varNames[j]])
            }
          }
          marginpctchange <-pnorm(a + coefficients[i] * (sd(df[, varNames[i]]) + mean(df[, varNames[i]]))) - pnorm(a + coefficients[i] * mean(df[, varNames[i]]))
          
          baselinepct <- pnorm(coefficients[1])
          writeLines(paste("In this regression the independent variable", varNames[i],
                           " has a statistically significant impact on the dependent variable",
                           "a 1 Standard Deviation increase in the independent variable causes a ", sprintf("%.3f",marginpctchange),
                           "percentage increase in the dependent variable. All of this is based on a baseline probability of"
                           ,sprintf("%.3f",baselinepct),"\n"))
          
          
        }
      } else {
        writeLines(paste("In this regression the independent variable", varNames[i],
                         " does not have  a statistically significant relationship with the dependent variable"))
      }
    }
  } else if (modelType == "logit"){
    output <- glm(formula = noquote(modelSpec), family = binomial(link = "logit"), data = df)
    summary(output)
    varNames <-   row.names(coef(summary(output)))
    coefficients <- as.numeric(coef(summary(output))[,1])
    error <- as.numeric(coef(summary(output))[,2])
    tVal <- as.numeric(coef(summary(output))[,3])
    pVal <- as.numeric(coef(summary(output))[,4])
    for(i in 2:length(varNames)){
      if(pVal[i] < 1.95){
        marginpctchange <- (exp(coefficients[i]) - 1)*100
        baselinepct <-(exp(coefficients[1]) - 1)*100
        writeLines(paste("In this regression the independent variable", varNames[i],
                         " has a statistically significant impact on the dependent variable",
                         "a 1 unit increase in the independent variable causes a ", sprintf("%.3f",marginpctchange),
                         "percentage increase in the dependent variable. All of this is based on a baseline probability of"
                         ,sprintf("%.3f",baselinepct),"\n"))
      } else {
        writeLines(paste("In this regression the independent variable", varNames[i],
                         " does not have  a statistically significant relationship with the dependent variable"))
      }
    }
    
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
                 independentVar, 
                 leftCensor = -Inf,
                 rightCensor = Inf,
                 logDepen = F, 
                 logIndepen = NULL, 
                 squareIndepend = NULL){
  modelSpec <- paste(dependentVar, "~", paste(independentVar, collapse = " + "))
  output <- tobit(formula = modelSpec, data = df, left = leftCensor , right = rightCensor)
  # if (modelType == "tobit"){
  #   if (leftCensor != -Inf | rightCensor != Inf){
  # 
  #   } else {
  #   writeLines("No censors inputted, please insert the left or ight limit for the tobit model to run.
  #               If there are no censors and it is a binominal model, then please run logit/probit with 0-1 dependent variable")
  #   }
  #   }
}


  
test <- function(df,
                 modelType, 
                 dependentVar, 
                 independentVar, 
                 leftCensor = -Inf,
                 rightCensor = Inf,
                 logDepen = F, 
                 logIndepen = NULL, 
                 squareIndepend = NULL){
  modelSpec <- dependentVar
  modelSpec1 <- paste(independentVar, collapse = " + ")
  
}

output <- test(df = cfb.scoring, 
     modelType = "tobit",
     dependentVar = "isCal", 
     independentVar =  c("defensive.TD", "defensive.FG"),
     leftCensor = 0)
modelSpec <- "isCal"
modelSpec1 <- paste( c("defensive.TD", "defensive.FG"), collapse = " + ") 
modelSpec2 <- paste("isCal", "~", paste( c("defensive.TD", "defensive.FG"), collapse = " + "))
output <-
  tobit(
    formula = modelSpec2 ,
    data = cfb.scoring,
    left = 0 ,
    right = Inf
  )

output1 <- tobit(formula = as.formula(noquote(modelSpec2)) , data = modelSpec2, left = 0 , right = Inf)