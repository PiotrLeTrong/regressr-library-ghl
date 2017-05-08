rm(list = ls())
load("/Users/piotr/Dropbox/School/Spring 2017/670 Data Sci/regressr-library-ghl/working_folder/data/1.raw/footballdata.RData")


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
                        squareIndepend = NULL,
                        detail = FALSE){
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
    for(i in 1:length(logIndepen)){
      name <-logIndepen[i]
      df[,name] <- log(df[,name])
      df[,name]
    }
  }
  if (length(squareIndepend) > 0){
    squareVector <- c()
    for(i in 1:length(squareIndepend)){
      name <- squareIndepend[i]
      name_square <- paste0(squareIndepend[i], ".square")
      df[,name_square] <- df[,name]^2
      squareVector <- c(squareVector, name_square)
    }
    indepVar <- c(squareVector, independentVar)
  }
  if(length(squareIndepend) > 0){
    modelSpec <- paste(dependentVar, "~", paste(indepVar,  collapse = " + "))
    } else {
    modelSpec <- paste(dependentVar, "~", paste(independentVar, collapse = " + "))
  }
  if(modelType == "ols"){
    output <- lm(noquote(modelSpec), df)
    summary(output)
    varNames <-   row.names(coef(summary(output)))
    coefficients <- as.numeric(coef(summary(output))[,1])
    error <- as.numeric(coef(summary(output))[,2])
    tVal <- as.numeric(coef(summary(output))[,3])
    pVal <- as.numeric(coef(summary(output))[,4])
    dropTemp <- c()
    for(i in 2:length(varNames)){
      if(pVal[i] < 1.95){
        if(logDepen == T & length(logIndepen) > 0){
          writeLines(paste("In this regression the independent variable", varNames[i],
                           " has a statistically significant relationship (at a 95% level of confidence) with the dependent variable",
                           "a 1 unit increase in the independent variable causes a ", sprintf("%.3f",coefficients[i]),
                           "percentage increase in the dependent variable. \n"))
        } else if (logDepen == T & length(logIndepen) == 0){
          writeLines(paste("In this regression the independent variable", varNames[i],
                           " has a statistically significant relationship (at a 95% level of confidence) with the dependent variable",
                           "a 1 unit increase in the independent variable causes a ", sprintf("%.3f",coefficients[i]*100),
                           "percentage increase in the dependent variable. \n"))
        } else if (logDepen == F & length(logIndepen) > 0){
          writeLines(paste("In this regression the independent variable", varNames[i],
                           " has a statistically significant relationship (at a 95% level of confidence) with the dependent variable",
                           "a 1 percent increase in the independent variable causes a ", sprintf("%.3f",coefficients[i]/100),
                           "increase in the dependent variable. \n"))
        } else if (logDepen == F & length(logIndepen) == 0 & length(squareIndepend) == 0 ){
        writeLines(paste("In this regression the independent variable", varNames[i],
                         " has a statistically significant relationship (at a 95% level of confidence) with the dependent variable",
                         "a 1 unit increase in the independent variable causes a ", sprintf("%.3f",coefficients[i]),
                         "increase in the dependent variable. \n"))

        } else if (length(squareIndepend) > 0){
          if(grepl(".square", varNames[i])){
            temp <- c()
            temp <- gsub(".square", "", varNames[i])
            s <- grep(temp, varNames)
            s <- s[2]
            finalCoeff <- 2*coefficients[i] + coefficients[s]
            writeLines(paste("In this regression the independent variable", temp,
                             " has a statistically significant relationship (at a 95% level of confidence) with the dependent variable",
                             "a 1 unit increase in the independent variable causes a ", sprintf("%.3f", finalCoeff),
                             "increase in the dependent variable due to its squared value. \n"))
            dropTemp <- c(dropTemp, temp)
          } else if (varNames[i] == dropTemp){
            writeLines(paste("In this regression the independent variable", varNames[i],
                             " has a statistically significant relationship (at a 95% level of confidence) with the dependent variable",
                             "a 1 unit increase in the independent variable causes a ", sprintf("%.3f",coefficients[i]),
                             "increase in the dependent variable. \n"))

        } else {
        writeLines(paste("In this regression the independent variable", varNames[i],
                         " does not have a statistically significant relationship (at a 95% level of confidence) with the dependent variable. \n"))
      }
      }
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
        if(class(df[,varNames[i]]) == "logical"){
          for(j in 2:length(varNames)){
            if(i!=j){
              a <- coefficients[j] * mean(df[, varNames[j]])
            }
          }
        marginpctchange_a <- exp(a + coefficients[i] * (sd(df[, varNames[i]]) + mean(df[, varNames[i]])))
        marginpctchange_b <- exp(a + coefficients[i] * (sd(df[, varNames[i]]) + mean(df[, varNames[i]]))) + 1
        marginpctchange_c <- marginpctchange_a/marginpctchange_b
        basepctchange_a <- exp(a + coefficients[i] * mean(df[, varNames[i]])) 
        basepctchange_b <- exp(a + coefficients[i] * mean(df[, varNames[i]])) + 1
        basepctchange_c <- basepctchange_a/basepctchange_b
        marginpctchange <- marginpctchange_c - basepctchange_c
        
        baselinepct <-(exp(coefficients[1])/(1 + exp(coefficients[1])))
                       
        writeLines(paste("In this regression the independent variable", varNames[i],
                         " has a statistically significant impact on the dependent variable",
                         "a 1 unit increase in the independent variable causes a ", sprintf("%.3f",marginpctchange),
                         "percentage increase in the dependent variable. All of this is based on a baseline probability of"
                         ,sprintf("%.3f",baselinepct),"\n"))
        } else {
          for(j in 2:length(varNames)){
            if(i!=j){
              a <- coefficients[j] * mean(df[, varNames[j]])
            }
          }
          marginpctchange_a <- exp(a + coefficients[i] * (1 + mean(df[, varNames[i]])))
          marginpctchange_b <- exp(a + coefficients[i] * (1 + mean(df[, varNames[i]]))) + 1
          marginpctchange_c <- marginpctchange_a/marginpctchange_b
          basepctchange_a <- exp(a + coefficients[i] * mean(df[, varNames[i]])) 
          basepctchange_b <- exp(a + coefficients[i] * mean(df[, varNames[i]])) + 1
          basepctchange_c <- basepctchange_a/basepctchange_b
          marginpctchange <- marginpctchange_c - basepctchange_c
          
          baselinepct <-(exp(coefficients[1])/(1 + exp(coefficients[1])))
          
          writeLines(paste("In this regression the independent variable", varNames[i],
                           " has a statistically significant impact on the dependent variable",
                           "a 1 unit increase in the independent variable causes a ", sprintf("%.3f",marginpctchange * 100),
                           "percentage increase in the dependent variable. All of this is based on a baseline probability of"
                           ,sprintf("%.3f",baselinepct * 100),"\n"))
        }
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
               \n - ols
               \n - probit
               \n - logit
               \n - tobit [Coming Soon]
               \n - multinominal probit [Coming Soon]
               \n - multinominal logit [Coming Soon]")
  }
  if(detail == T){
    print(summary(output))
  }
}

output <- interpreter(df = cfb.scoring,
                      modelType = "ols",
                      dependentVar = "isCal", 
                      independentVar =  c("defensive.TD", "defensive.FG"),
                      detail = T)
