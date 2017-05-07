#This version of function 4 is much longer, but takes less time to evaluate users'inputs (model)

optimizer <- function(df, depVar, indepVar = colnames(df)[colnames(df) != depVar], include = NULL, 
                      model = c("OLS", "binary probit", "binary logit", "ordered probit", 
                                    "ordered logit", "multinomial probit", "multinomial logit"), 
                      time.series = FALSE, time.var = NULL,
                      save.csv = FALSE) {
  
  ###Mape function for calcultation of mape###
  mape <- function(yhat, y){
    return(100*mean(abs(yhat/y - 1), na.rm=T))
  }
  
  #Check date/time for time series
  if((time.series) & is.null(time.var)) (stop("Must enter the date/time variable for time-series analysis."))
  #Partiton for time-series and non-time series data
  set.seed(100)
  if(time.series == FALSE) {
    rand <- runif(nrow(df)) 
    train <- df[rand > 0.3, ]
    validate <- df[rand <= 0.3 & rand > 0.15, ]
    test <- df[rand <= 0.15, ]
  }
  ##still thinking about how to address this: reformat date/time variable so the function can sort the data by date/time
  # else if(as.Date(df[, time.var])) {
  #   stop("Please transform you time variable.")
  # }
  else {
    df <- df[order(df[, time.var]), ]
    train <- df[1:round(0.7 * nrow(df)), ]
    validate <- df[(round(0.7 * nrow(df)) + 1):round(0.85*nrow(df)), ]
    test <- df[(round(0.85 * nrow(df)) + 1):nrow(df), ]
  }
  
  #Paste variables users want to include in all specifications to use in formulas
  include.paste <- ""
  x <- 1
  while(x <= length(include)) {
    include.paste <- paste0(include.paste, "+", include[x])
    x <- x + 1
  }
  
  #Exclude "include" in possible combinations
  indepVar <- indepVar[!indepVar %in% include]
 
  #Different models
  if(model == "OLS") {
    
    #Loop of the numbers of variables combined
    ll <- lapply(1:length(indepVar), FUN = function(a) {
      combination <- combn(indepVar, a, simplify = FALSE)
      #Loop of combinations of j numbers of variabels
      options(warn = -1)
      l <- lapply(combination, FUN = function(i) {
        #Paste dependent variable and independent variables to a formula to use in regressions
        pasta <- ""
        x <- 1
        while(x < length(i)) {
          pasta <- paste0(pasta, i[x], "+")
          x <- x + 1
        }
        regFormula<- paste0(depVar, "~", pasta, i[length(i)], include.paste)
        #Estimate regression
        fit <- lm(noquote(regFormula), data = train)
        #Predict yhat for validate and test sets
        validate$yhat <- predict(fit, newdata = validate)
        test$yhat <- predict(fit, newdata = test)
        #calculate MAPE for validate and test sets
        validateMape <- mape(validate$yhat, validate[, depVar])
        testMape <- mape(test$yhat, test[, depVar])
        diagnostic <- c(paste(paste(include, collapse = ", "),
                              if(length(include)) (", "),
                              paste(i, collapse = ", ")), validateMape, testMape)
        return(diagnostic)
      })
      master <- do.call("rbind", l)
      return(master)
    })
    superMaster <- do.call("rbind", ll)
    } else if(model == "binary probit") {
      #Loop of the numbers of variables combined
      ll <- lapply(1:length(indepVar), FUN = function(a) {
        combination <- combn(indepVar, a, simplify = FALSE)
        #Loop of combinations of j numbers of variabels
        options(warn = -1)
        l <- lapply(combination, FUN = function(i) {
          #Paste dependent variable and independent variables to a formula to use in regressions
          pasta <- ""
          x <- 1
          while(x < length(i)) {
            pasta <- paste0(pasta, i[x], "+")
            x <- x + 1
          }
          regFormula<- paste0(depVar, "~", pasta, i[length(i)], include.paste)
          #Estimate regression
          fit <- glm(noquote(regFormula), family = binomial(link = "probit"), data = train)
          #Predict yhat for validate and test sets
          validate$yhat <- predict(fit, type = "response", newdata = validate)
          test$yhat <- predict(fit, type = "response", newdata = test)
          #calculate MAPE for validate and test sets
          validateMape <- mape(validate$yhat, validate[, depVar])
          testMape <- mape(test$yhat, test[, depVar])
          diagnostic <- c(paste(paste(include, collapse = ", "),
                                if(length(include)) (", "),
                                paste(i, collapse = ", ")), validateMape, testMape)
          return(diagnostic)
        })
        master <- do.call("rbind", l)
        return(master)
      })
      superMaster <- do.call("rbind", ll)
      } else if(model == "binary logit"){
        #Loop of the numbers of variables combined
        ll <- lapply(1:length(indepVar), FUN = function(a) {
          combination <- combn(indepVar, a, simplify = FALSE)
          #Loop of combinations of j numbers of variabels
          options(warn = -1)
          l <- lapply(combination, FUN = function(i) {
            #Paste dependent variable and independent variables to a formula to use in regressions
            pasta <- ""
            x <- 1
            while(x < length(i)) {
              pasta <- paste0(pasta, i[x], "+")
              x <- x + 1
            }
            regFormula<- paste0(depVar, "~", pasta, i[length(i)], include.paste)
            #Estimate regression
            fit <- glm(noquote(regFormula), family = binomial(link = "logit"), data = train)
            #Predict yhat for validate and test sets
            validate$yhat <- predict(fit, type = "response", newdata = validate)
            test$yhat <- predict(fit, type = "response", newdata = test)
            #calculate MAPE for validate and test sets
            validateMape <- mape(validate$yhat, validate[, depVar])
            testMape <- mape(test$yhat, test[, depVar])
            diagnostic <- c(paste(paste(include, collapse = ", "),
                                  if(length(include)) (", "),
                                  paste(i, collapse = ", ")), validateMape, testMape)
            return(diagnostic)
          })
          master <- do.call("rbind", l)
          return(master)
        })
        superMaster <- do.call("rbind", ll)
      } else if(model == "ordered probit"){
        if(!require(MASS)) {
          install.packages("MASS")
          library(MASS)
        }
        #Loop of the numbers of variables combined
        ll <- lapply(1:length(indepVar), FUN = function(a) {
          combination <- combn(indepVar, a, simplify = FALSE)
          #Loop of combinations of j numbers of variabels
          options(warn = -1)
          l <- lapply(combination, FUN = function(i) {
            #Paste dependent variable and independent variables to a formula to use in regressions
            pasta <- ""
            x <- 1
            while(x < length(i)) {
              pasta <- paste0(pasta, i[x], "+")
              x <- x + 1
            }
            regFormula<- paste0(depVar, "~", pasta, i[length(i)], include.paste)
            #Estimate regression
            fit <- polr(noquote(regFormula), data = train, method = "probit")
            #Predict yhat for validate and test sets
            validate$yhat <- predict(fit, type = "response", newdata = validate)
            test$yhat <- predict(fit, type = "response", newdata = test)
            #calculate MAPE for validate and test sets
            validateMape <- mape(validate$yhat, validate[, depVar])
            testMape <- mape(test$yhat, test[, depVar])
            diagnostic <- c(paste(paste(include, collapse = ", "),
                                  if(length(include)) (", "),
                                  paste(i, collapse = ", ")), validateMape, testMape)
            return(diagnostic)
          })
          master <- do.call("rbind", l)
          return(master)
        })
        superMaster <- do.call("rbind", ll)
      } else if(model == "ordered logit"){
        if(!require(MASS)) {
          install.packages("MASS")
          library(MASS)
        }
        #Loop of the numbers of variables combined
        ll <- lapply(1:length(indepVar), FUN = function(a) {
          combination <- combn(indepVar, a, simplify = FALSE)
          #Loop of combinations of j numbers of variabels
          options(warn = -1)
          l <- lapply(combination, FUN = function(i) {
            #Paste dependent variable and independent variables to a formula to use in regressions
            pasta <- ""
            x <- 1
            while(x < length(i)) {
              pasta <- paste0(pasta, i[x], "+")
              x <- x + 1
            }
            regFormula<- paste0(depVar, "~", pasta, i[length(i)], include.paste)
            #Estimate regression
            fit <- polr(noquote(regFormula), data = train, method = "logistic")
            #Predict yhat for validate and test sets
            validate$yhat <- predict(fit, type = "response", newdata = validate)
            test$yhat <- predict(fit, type = "response", newdata = test)
            #calculate MAPE for validate and test sets
            validateMape <- mape(validate$yhat, validate[, depVar])
            testMape <- mape(test$yhat, test[, depVar])
            diagnostic <- c(paste(paste(include, collapse = ", "),
                                  if(length(include)) (", "),
                                  paste(i, collapse = ", ")), validateMape, testMape)
            return(diagnostic)
          })
          master <- do.call("rbind", l)
          return(master)
        })
        superMaster <- do.call("rbind", ll)
      } else if (model == "multinomial logit") {
        if(!require(mlogit)) {
          install.packages("mlogit")
          library(mlogit)
        }
        #Loop of the numbers of variables combined
        ll <- lapply(1:length(indepVar), FUN = function(a) {
          combination <- combn(indepVar, a, simplify = FALSE)
          #Loop of combinations of j numbers of variabels
          options(warn = -1)
          l <- lapply(combination, FUN = function(i) {
            #Paste dependent variable and independent variables to a formula to use in regressions
            pasta <- ""
            x <- 1
            while(x < length(i)) {
              pasta <- paste0(pasta, i[x], "+")
              x <- x + 1
            }
            regFormula<- paste0(depVar, "~", pasta, i[length(i)], include.paste)
            #Estimate regression
            fit <- mlogit(noquote(regFormula), data = train)
            #Predict yhat for validate and test sets
            validate$yhat <- predict(fit, type = "response", newdata = validate)
            test$yhat <- predict(fit, type = "response", newdata = test)
            #calculate MAPE for validate and test sets
            validateMape <- mape(validate$yhat, validate[, depVar])
            testMape <- mape(test$yhat, test[, depVar])
            diagnostic <- c(paste(paste(include, collapse = ", "),
                                  if(length(include)) (", "),
                                  paste(i, collapse = ", ")), validateMape, testMape)
            return(diagnostic)
          })
          master <- do.call("rbind", l)
          return(master)
        })
        superMaster <- do.call("rbind", ll)
      } else {
        if(!require(mlogit)) {
          install.packages("mlogit")
          library(mlogit)
        }
        #Loop of the numbers of variables combined
        ll <- lapply(1:length(indepVar), FUN = function(a) {
          combination <- combn(indepVar, a, simplify = FALSE)
          #Loop of combinations of j numbers of variabels
          options(warn = -1)
          l <- lapply(combination, FUN = function(i) {
            #Paste dependent variable and independent variables to a formula to use in regressions
            pasta <- ""
            x <- 1
            while(x < length(i)) {
              pasta <- paste0(pasta, i[x], "+")
              x <- x + 1
            }
            regFormula<- paste0(depVar, "~", pasta, i[length(i)], include.paste)
            #Estimate regression
            fit <- mlogit(noquote(regFormula), data = train, probit = TRUE)
            #Predict yhat for validate and test sets
            validate$yhat <- predict(fit, type = "response", newdata = validate)
            test$yhat <- predict(fit, type = "response", newdata = test)
            #calculate MAPE for validate and test sets
            validateMape <- mape(validate$yhat, validate[, depVar])
            testMape <- mape(test$yhat, test[, depVar])
            diagnostic <- c(paste(paste(include, collapse = ", "),
                                  if(length(include)) (", "),
                                  paste(i, collapse = ", ")), validateMape, testMape)
            return(diagnostic)
          })
          master <- do.call("rbind", l)
          return(master)
        })
        superMaster <- do.call("rbind", ll)
        }
  colnames(superMaster) <- c("independent_variables", "validate_mape", "test_mape")
  #Store sorted data frame to work space
  bestValidate <- superMaster[order(superMaster[, 2]), ]
  row.names(bestValidate) <- NULL
  bestValidate <<- bestValidate
  bestTest <- superMaster[order(superMaster[, 3]),]
  row.names(bestTest) <- NULL
  bestTest <<- bestTest
  #Return message
  message("Data frames sorted by validate set MAPE and test set MAPE are stored in work space.")
  #Save .csv files
  if(save.csv) {
    write.csv(bestValidate, "whatever")
    write.csv(bestTest, "whatever2")
  }
}



temp <- tempfile()
download.file("https://github.com/GeorgetownMcCourt/data-science/raw/master/homeworks/homework2/toStudents/homework2_data.Rda", temp, mode="wb")
load(temp)
hmwk$date <- as.Date(hmwk$date, "%Y-%mm-%dd")
hmwk <- hmwk[hmwk$complete == TRUE, ]
hmwk <- hmwk[hmwk$GEOID==10180, ]
hmwk <- hmwk[,-1]
a <- proc.time()[3]
fit <- glm(complete~month + year, hmwk, family = binomial(link = "probit"))
optimizer(hmwk, depVar = "complete",  model = "binary probit")
proc.time()[3]-a  
