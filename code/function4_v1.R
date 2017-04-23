#This version of the function is shorter, but it takies longer time to evaluate user's inputs

modelOptimize <- function(df, dependentVar, independentVar = colnames(df)[colnames(df) != dependentVar], include = NULL, 
                           model = c("OLS", "binary probit", "binary logit", "ordered probit", "ordered logit", "multinomial logit", "multinomial probit"), 
                           time.series = FALSE, time.var = NULL) {
  #Mape function for calculation of mape
  mape <- function(yhat, y){
    return(100*mean(abs(yhat/y - 1), na.rm=T))
  }
  set.seed(100)
  #Partition for time-series and non-time-series data
  if(time.series == FALSE) {
    rand <- runif(nrow(df)) 
    train <- df[rand > 0.3, ]
    validate <- df[rand <= 0.3 & rand > 0.15, ]
    test <- df[rand <= 0.15, ]
  }
  ##still thinking about how to address this
  # else if(as.Date(df[, time.var])) {
  #   stop("Please transform you time variable.")
  # }
  else {
    df <- df[order(df[, time.var]), ]
    train <- df[1:round(0.7*nrow(df)), ]
    validate <- df[(round(0.7*nrow(df)) + 1):round(0.85*nrow(df)), ]
    test <- df[(round(0.85*nrow(df)) + 1):nrow(df), ]
  }
  independentVar <- independentVar[!independentVar %in% include]
  superMaster <- c()
  for(i in 1:length(independentVar)) {
    combination <- combn(independentVar, i)
    master <- c()
    for(i in 1:ncol(combination)){
      ivList <- combination[, i]
      pasta <- ""
      for(i in 1:(length(ivList) - 1)) {
        pasta <- paste(pasta, ivList[i], "+", sep = "")
      }
      pasta <- paste(dependentVar, "~", pasta, ivList[length(ivList)], sep = " ")
      if(model == "OLS") {
        fit <- lm(unquote(pasty), data = train)
        validate$yhat <- predict(fit, newdata = validate)
        test$yhat <- predict(fit, newdata = test)
      } else {
        if(model == "binary probit"){
          fit <- glm(unquote(pasty), family = binominal(link = "probit"), data = train)
        } else if(model == "binary logit") {
          fit <- glm(unquote(pasty), family = binominal(link = "logit"), data = train)
        } else if(model == "ordered probit") {
          if(!require(MASS)) {
            install.packages("MASS")
            library(MASS)
          }
          fit <- polr(unquote(pasty), data = train, method = "probit")
        } else if(model == "ordered logit") {
          if(!require(MASS)) {
            install.packages("MASS")
            library(MASS)
          }
          fit <- polr(unquote(pasty), data = train, method = "logit")
        } else if(model == "multinomical logit") {
          if(!require(mlogit)) {
            install.packages("mlogit")
            library(mloit)
          }
          fit <- mlogit(unquote(pasty), data = train)
        } else {
          if(!require(mlogit)) {
            install.packages("mlogit")
            library(mloit)
          }
          fit <- mlogit(unquote(pasty), data = train, probit = TRUE)
        }
        validate$yhat <- predict(fit, newdata = validate, type = "response")
        test$yhat <- predict(fit, newdata = test, type = "response")
      }  

      validateMape <- mape(validate$yhat, validate[, dependentVar])
      testMape <- mape(test$yhat, test[, dependentVar])
      diagnostic <- c(paste(ivList, sep = ","), validateMape, testMape)
      master <- rbind(master, diagnostic)
    }
    superMaster <- rbind.data.frame(superMaster, master)
  }
  colnames(superMaster) <- c("independent_variables", "validate_mape", "test_mape")
  bestValidate <- superMaster[order(superMaster$validateMape), ]
  bestTest <- superMaster[order(superMaster$testMape),]
}


