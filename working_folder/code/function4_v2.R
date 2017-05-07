#This version of function 4 is much longer, but takes less time to evaluate users'inputs (model)

modelOptimize <- function(df, dependentVar, independentVar = colnames(df)[colnames(df) != dependentVar], include = NULL, 
                          model = c("OLS", "binary probit", "binary logit", "ordered probit", 
                                    "ordered logit", "multinomial logit", "multinomial probit"), 
                          time.series = FALSE, time.var = NULL,
                          save.csv = FALSE) {
  #Mape function for calcultation of mape
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
  include.paste <<- include.paste 
  #Exclude "include" in possible combinations
  independentVar <- independentVar[!independentVar %in% include]
  independentVar <<- independentVar
  #Different models
  if(model == "OLS") {
    superMaster <- c()
    #Loop of the numbers of variables combined
    for(i in 1:length(independentVar)) {
      combination <- combn(independentVar, i, simplify = FALSE)
      master <- c()
      #Loop of combinations of j numbers of variabels
      options(warn = -1)
      for(i in 1:length(combination)) {
        ivList <- combination[i]
        ivList <- unlist(ivList)
        pasta <- ""
        #Paste independent variables, the variables user want to include, and the dependent variable to a formula
        x <- 1
        while(x < length(ivList)) {
          pasta <- paste0(pasta, ivList[x], "+")
          x <- x + 1
          }
        pasta <- paste0(dependentVar, "~", pasta, ivList[length(ivList)], include.paste)
        pasta <<- pasta
        #Run regression
        fit <- lm(noquote(pasta), data = train)
        #Predict validate and test sets
        validate$yhat <- predict(fit, newdata = validate)
        test$yhat <- predict(fit, newdata = test)
        #calculate MAPE for validate and test sets
        validateMape <- mape(validate$yhat, validate[, dependentVar])
        testMape <- mape(test$yhat, test[, dependentVar])
        diagnostic <- c(paste(ivList, include, collapse = ","), validateMape, testMape)
        #Append diagnosis of regressions of different combinations of i number of variables
        master <- rbind(master, diagnostic)
        }
      #Append different numbers of combinations
      superMaster <- rbind(superMaster, master)
      }
    } else if(model == "binary probit") {
      superMaster <- c()
      for(i in 1:length(independentVar)) {
        combination <- combn(independentVar, i, simplify = FALSE)
        master <- c()
        for(i in 1:length(combination)) {
          ivList <- combination[i]
          ivList <- unlist(ivList)
          pasta <- ""
          x <- 1
          while(x < length(ivList)) {
            pasta <- paste0(pasta, ivList[x], "+")
            x <- x + 1
          }
          pasta <- paste0(dependentVar, "~", pasta, ivList[length(ivList)], include.paste)
          fit <- glm(noquote(pasta), family = binominal(link = "probit"), data = train)
          validate$yhat <- predict(fit, newdata = validate, type = "reponse")
          test$yhat <- predict(fit, newdata = test, type = "reponse")
          testMape <- mape(test$yhat, test[, dependentVar])
          diagnostic <- c(paste(ivList, include, collapse = ","), validateMape, testMape)
          master <- rbind(master, diagnostic)
          }
        superMaster <- rbind(superMaster, master)
        
        }
      } else if(model == "binary logit") {
        superMaster <- c()
        for(i in 1:length(independentVar)) {
          combination <- combn(independentVar, i, simplify = FALSE)
          master <- c()
          for(i in 1:length(combination)) {
            ivList <- combination[i]
            ivList <- unlist(ivList)
            pasta <- ""
            x <- 1
            while(x < length(ivList)) {
              pasta <- paste0(pasta, ivList[x], "+")
              x <- x + 1
            }
            pasta <- paste0(dependentVar, "~", pasta, ivList[length(ivList)], include.paste)
            fit <- glm(noquote(pasta), family = binominal(link = "lobit"), data = train)
            validate$yhat <- predict(fit, newdata = validate, type = "reponse")
            test$yhat <- predict(fit, newdata = test, type = "reponse")
            validateMape <- mape(validate$yhat, validate[, dependentVar])
            testMape <- mape(test$yhat, test[, dependentVar])
            diagnostic <- c(paste(ivList, collapse = ","), validateMape, testMape)
            master <- rbind(master, diagnostic)
          }
          superMaster <- rbind(superMaster, master)
          
        }
      } else if(model == "ordered propit"){
        if(!require(MASS)) {
          install.packages("MASS")
          library(MASS)
        }
        superMaster <- c()
        for(i in 1:length(independentVar)) {
          combination <- combn(independentVar, i, simplify = FALSE)
          master <- c()
          for(i in 1:length(combination)) {
            ivList <- combination[i]
            ivList <- unlist(ivList)
            pasta <- ""
            x <- 1
            while(x < length(ivList)) {
              pasta <- paste0(pasta, ivList[x], "+")
              x <- x + 1
            }
            pasta <- paste0(dependentVar, "~", pasta, ivList[length(ivList)], include.paste)
            fit <- polr(noquote(pasta), data = train, method = "probit")
            validate$yhat <- predict(fit, newdata = validate, type = "reponse")
            test$yhat <- predict(fit, newdata = test, type = "reponse")
            validateMape <- mape(validate$yhat, validate[, dependentVar])
            testMape <- mape(test$yhat, test[, dependentVar])
            diagnostic <- c(paste(ivList, include, collapse = ","), validateMape, testMape)
            master <- rbind(master, diagnostic)
          }
          superMaster <- rbind(superMaster, master)
          
        }
      } else if(model == "ordered logit") {
        if(!require(MASS)) {
          install.packages("MASS")
          library(MASS)
        }
        superMaster <- c()
        for(i in 1:length(independentVar)) {
          combination <- combn(independentVar, i, simplify = FALSE)
          master <- c()
          for(i in 1:length(combination)) {
            ivList <- combination[i]
            ivList <- unlist(ivList)
            pasta <- ""
            x <- 1
            while(x < length(ivList)) {
              pasta <- paste0(pasta, ivList[x], "+")
              x <- x + 1
            }
            pasta <- paste0(dependentVar, "~", pasta, ivList[length(ivList)], include.paste)
            fit <- polr(noquote(pasta), data = train, method = "logit")
            validate$yhat <- predict(fit, newdata = validate, type = "reponse")
            test$yhat <- predict(fit, newdata = test, type = "reponse")
            validateMape <- mape(validate$yhat, validate[, dependentVar])
            testMape <- mape(test$yhat, test[, dependentVar])
            diagnostic <- c(paste(ivList, include, collapse = ","), validateMape, testMape)
            master <- rbind(master, diagnostic)
          }
        } 
        superMaster <- rbind(superMaster, master)
        
      } else if (model == "multinomial logit") {
        if(!require(mlogit)) {
          install.packages("mlogit")
          library(mloit)
        }
        superMaster <- c()
        for(i in 1:length(independentVar)) {
          combination <- combn(independentVar, i, simplify = FALSE)
          master <- c()
          for(i in 1:length(combination)) {
            ivList <- combination[i]
            ivList <- unlist(ivList)
            pasta <- ""
            x <- 1
            while(x < length(ivList)) {
              pasta <- paste0(pasta, ivList[x], "+")
              x <- x + 1
            }
            pasta <- paste0(dependentVar, "~", pasta, ivList[length(ivList)], include.paste)
            fit <- mlogit(noquote(pasta), data = train)
            validate$yhat <- predict(fit, newdata = validate, type = "reponse")
            test$yhat <- predict(fit, newdata = test, type = "reponse")
            validateMape <- mape(validate$yhat, validate[, dependentVar])
            testMape <- mape(test$yhat, test[, dependentVar])
            diagnostic <- c(paste(ivList, include, collapse = ","), validateMape, testMape)
            master <- rbind(master, diagnostic)
          }
        } 
        superMaster <- rbind(superMaster, master)
      } else {
        if(!require(mlogit)) {
          install.packages("mlogit")
          library(mloit)
        }
        superMaster <- c()
        for(i in 1:length(independentVar)) {
          combination <- combn(independentVar, i, simplify = FALSE)
          master <- c()
          for(i in 1:length(combination)) {
            ivList <- combination[i]
            ivList <- unlist(ivList)
            pasta <- ""
            x <- 1
            while(x < length(ivList)) {
              pasta <- paste0(pasta, ivList[x], "+")
              x <- x + 1
            }
            pasta <- paste0(dependentVar, "~", pasta, ivList[length(ivList)], include.paste)
            fit <- mlogit(noquote(pasta), data = train, probit = TRUE)
            validate$yhat <- predict(fit, newdata = validate, type = "reponse")
            test$yhat <- predict(fit, newdata = test, type = "reponse")
            validateMape <- mape(validate$yhat, validate[, dependentVar])
            testMape <- mape(test$yhat, test[, dependentVar])
            diagnostic <- c(paste(ivList, include, collapse = ","), validateMape, testMape)
            master <- rbind(master, diagnostic)
          }
          #Append all output from i number of variables combine
          superMaster <- rbind(superMaster, master)
        }
      }
  colnames(superMaster) <- c("independent_variables", "validate_mape", "test_mape")
  #Store sorted data frame to work space
  bestValidate <- superMaster[order(superMaster[, 2]), ]
  row.names(bestValidate) <- NULL
  bestValidate <<- bestValidate
  bestTest <- superMaster[order(superMaster[, 3]),]
  row.names(bestTest) <- NULL
  bestTest <<- bestTest
  return("Data frames sorted by validate set MAPE and test set MAPE are stored in work space.")
  if(save.csv) {
    write.csv(bestValidate, "whatever")
    write.csv(bestValidate, "whatever2")
    }
}


temp <- tempfile()
download.file("https://github.com/GeorgetownMcCourt/data-science/raw/master/homeworks/homework2/toStudents/homework2_data.Rda", temp, mode="wb")
load(temp)
hmwk$date <- as.Date(hmwk$date, "%Y-%mm-%dd")

a <- proc.time()[3]
modelOptimize(hmwk, dependentVar = "emp", include = c("avg_rad.sum", "avg_rad.mean"), model = "OLS", time.series = TRUE, time.var = "date")
proc.time()[3]-a  
