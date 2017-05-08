library("devtools")
library(roxygen2)
#' Find The Best Specifications
#'
#' @description This function searches for regression model specifications with the lowest MAPE, taking inputs of a data frame, a dependent variable, a list of potential independent variables, and a regression model.
#' @param data a data frame 
#' @param depVar a dependent variable 
#' @param indepVar a list of independent variables the subsets of which the dependent variable will be regressed on. Defaul is all variables in the data frame but the dependent variable.
#' @param include a list of independent variables included in all regressions
#' @param model regression model to estimate, including "OLS", "binary probit", "binary logit", "ordered probit", "ordered logit", "multinomial probit", "multinomial logit"
#' @param time.series logical. This is used to decide the partition process.
#' @param timeVar the time series variable
#' @param sqIndep any variables in the independent variable list to consider quadratic terms in the regressions
#' @keywords regressr
#' @export
#' @examples 
#' ##Use iris dataset 
#' ##OLS model 
#' optimizer(iris, depVar = "Sepal.Length", include = "Sepal.Width", model = "OLS") 
#' ##Multinomial logistic model 
#' optimizer(iris, depVar = "Species", model = "multinomial logit")

optimizer <- function(data, depVar, indepVar = colnames(data)[colnames(data) != depVar], include = NULL, 
                      model = c("OLS", "binary probit", "binary logit", "ordered probit", "ordered logit", "multinomial logit"), 
                      time.series = FALSE, timeVar = NULL,
                      sqIndep = NULL) {
  
  #Mape function for calculation of mape
  mape <- function(yhat, y){
    return(100*mean(abs(yhat/y - 1), na.rm=T))
  }
  
  #Mean f1 function for calculation of mean f1
  meanf1 <- function(actual, predicted){
    #Mean F1 score function
    #actual = a vector of actual labels
    #predicted = predicted labels
    classes <- unique(actual)
    results <- data.frame()
    for(k in classes){
      results <- rbind(results, 
                       data.frame(class.name = k,
                                  weight = sum(actual == k)/length(actual),
                                  precision = sum(predicted == k & actual == k)/sum(predicted == k), 
                                  recall = sum(predicted == k & actual == k)/sum(actual == k)))
    }
    results$score <- results$weight * 2 * (results$precision * results$recall) / (results$precision + results$recall) 
    return(sum(results$score))
  }
  
  
  #If user specifies quadratic terms to include, create these variables, and add the names to indepVar 
  if(length(sqIndep)) {
    names_square <- paste0(sqIndep, "_sqGenerated")
    data[, names_square] <- data[ , sqIndep]^2
    indepVar <- c(indepVar, names_square)
  }
  
  #Check date/time for time series data in order to sort data for partition
  if((time.series) & is.null(timeVar)) (stop("Must enter the date/time variable for time-series analysis."))
  if((time.series) & length(timeVar) & class(timeVar) != "Date") {
    if (grepl("\\d{4}-\\d{2}-\\d{2}", timeVar[runif(1,1,length(timeVar))], perl=TRUE) == TRUE) {
      timeVar <- as.Date(timeVar, "%Y-%mm-%dd")
      } else if (grepl("\\d{4}/\\d{2}/\\d{2}", timeVar[runif(1,1,length(timeVar))], perl=TRUE) == TRUE) {
        timeVar <- as.Date(timeVar, "%Y/%mm/%dd")
      } else if(grepl("\\d{2}-\\d{2}-\\d{2}", timeVar[runif(1,1,length(timeVar))], perl=TRUE) == TRUE) {
        timeVar <- as.Date(timeVar, "%y/%mm/%dd")
      } else if (grepl("\\d{1,2}\\w{3, }\\d{4}", timeVar[runif(1,1,length(timeVar))], perl=TRUE) == TRUE) {
        timeVar <- as.Date(timeVar, "%d%b%Y")
      } else if (grepl("\\d{4}-\\d{2}-\\d{2}\\s+\\d{2}:\\d{2}:\\d{2}", timeVar[runif(1,1,length(timeVar))], perl=TRUE) == TRUE) {
        timeVar <- as.Date(timeVar, "%Y-%mm-%dd %H:%M:%S")
      } else if (grepl("\\d{4}/\\d{2}/\\d{2}\\s+\\d{2}:\\d{2}:\\d{2}", timeVar[runif(1,1,length(timeVar))], perl=TRUE) == TRUE) {
        timeVar <- as.Date(timeVar, "%Y/%mm/%dd %H:%M:%S")
      } else {
        stop("Please transfrom the time variable to date format.")
      }
    }

  #Partiton for time-series and non-time series data
  set.seed(100)
  if(time.series == FALSE) {
    rand <- runif(nrow(data)) 
    train <- data[rand > 0.3, ]
    validate <- data[rand <= 0.3 & rand > 0.15, ]
    test <- data[rand <= 0.15, ]
  } else {
    data <- data[order(data[, timeVar]), ]
    train <- data[1:round(0.7*nrow(data)), ]
    validate <- data[(round(0.7*nrow(data)) + 1):round(0.85*nrow(data)), ]
    test <- data[(round(0.85*nrow(data)) + 1):nrow(data), ]
  }
  
  #Paste variables users want to include in all specifications to use in formulas; paste(x, collaspe = "+") 
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
    
    #Different numbers of variables combined
    ll <- lapply(1:length(indepVar), FUN = function(a) {
      combination <- combn(indepVar, a, simplify = FALSE)
      #Different combinations of a given number of variabels
      options(warn = -1)
      l <- lapply(combination, FUN = function(i) {
        #If the quadratic term of a variable is included but not the variable itself, include the variable
        if (length(sqIndep)) {
          quadratic <- grep("_sqgenerated$", i, value = TRUE)
          b <- strsplit(quadratic, "_sqgenerated")
          lapply(b, FUN = function(x) {
            if((!x %in% i) & (!x %in% include)) (i <- c(i, x))
          })
        }
        #In case anything repeats
        i <- unique(i)
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
        #Combine to a vector
        diagnostic <- c(paste(paste(include, collapse = ", "),
                              if(length(include)) (", "),
                              paste(i, collapse = ", ")), validateMape, testMape)
        return(diagnostic)
      })
      #Rbind the rows for different combinations of a given number of combinations
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
        #If the quadratic term of a variable is included but not the variable itself, include the variable
        if (length(sqIndep)) {
          quadratic <- grep("_sqgenerated$", i, value = TRUE)
          b <- strsplit(quadratic, "_sqgenerated")
          lapply(b, FUN = function(x) {
            if((!x %in% i) & (!x %in% include)) (i <- c(i, x))
          })
        }
        #In case anything repeats
        i <- unique(i)
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
        #If the quadratic term of a variable is included but not the variable itself, include the variable
        if (length(sqIndep)) {
          quadratic <- grep("_sqgenerated$", i, value = TRUE)
          b <- strsplit(quadratic, "_sqgenerated")
          lapply(b, FUN = function(x) {
            if((!x %in% i) & (!x %in% include)) (i <- c(i, x))
          })
        }
        #In case anything repeats
        i <- unique(i)
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
        #If the quadratic term of a variable is included but not the variable itself, include the variable
        if (length(sqIndep)) {
          quadratic <- grep("_sqgenerated$", i, value = TRUE)
          b <- strsplit(quadratic, "_sqgenerated")
          lapply(b, FUN = function(x) {
            if((!x %in% i) & (!x %in% include)) (i <- c(i, x))
          })
        }
        #In case anything repeats
        i <- unique(i)
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
        validate$yhat <- predict(fit, type = "class", newdata = validate)
        test$yhat <- predict(fit, type = "class", newdata = test)
        #calculate MAPE for validate and test sets
        validateF1 <- meanf1(validate[, depVar], validate$yhat)
        testF1 <- meanf1(test[, depVar], test$yhat)
        diagnostic <- c(paste(paste(include, collapse = ", "),
                              if(length(include)) (", "),
                              paste(i, collapse = ", ")), validateF1, testF1)
        xyz <<- diagnostic 
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
        #If the quadratic term of a variable is included but not the variable itself, include the variable
        if (length(sqIndep)) {
          quadratic <- grep("_sqgenerated$", i, value = TRUE)
          b <- strsplit(quadratic, "_sqgenerated")
          lapply(b, FUN = function(x) {
            if((!x %in% i) & (!x %in% include)) (i <- c(i, x))
          })
        }
        #In case anything repeats
        i <- unique(i)
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
        validate$yhat <- predict(fit, type = "class", newdata = validate)
        test$yhat <- predict(fit, type = "class", newdata = test)
        #calculate MAPE for validate and test sets
        validateF1 <- meanf1(validate[, depVar], validate$yhat)
        testF1 <- meanf1(test[, depVar], test$yhat)
        diagnostic <- c(paste(paste(include, collapse = ", "),
                              if(length(include)) (", "),
                              paste(i, collapse = ", ")), validateF1, testF1)
        return(diagnostic)
      })
      master <- do.call("rbind", l)
      return(master)
    })
    superMaster <- do.call("rbind", ll)
  } else {
    if(!require(nnet)) {
      install.packages("nnet")
      library(nnet)
    }
    #Loop of the numbers of variables combined
    ll <- lapply(1:length(indepVar), FUN = function(a) {
      combination <- combn(indepVar, a, simplify = FALSE)
      #Loop of combinations of j numbers of variabels
      options(warn = -1)
      l <- lapply(combination, FUN = function(i) {
        #If the quadratic term of a variable is included but not the variable itself, include the variable
        if (length(sqIndep)) {
          quadratic <- grep("_sqgenerated$", i, value = TRUE)
          b <- strsplit(quadratic, "_sqgenerated")
          lapply(b, FUN = function(x) {
            if((!x %in% i) & (!x %in% include)) (i <- c(i, x))
          })
        }
        #In case anything repeats
        i <- unique(i)
        #Paste dependent variable and independent variables to a formula to use in regressions
        pasta <- ""
        x <- 1
        while(x < length(i)) {
          pasta <- paste0(pasta, i[x], "+")
          x <- x + 1
        }
        regFormula<- paste0(depVar, "~", pasta, i[length(i)], include.paste)
        #Estimate regression
        fit <- multinom(noquote(regFormula), data = train)
        #Predict yhat for validate and test sets
        validate$yhat <- predict(fit, type = "class", newdata = validate)
        test$yhat <- predict(fit, type = "class", newdata = test)
        #calculate MAPE for validate and test sets
        validateF1 <- meanf1(validate[, depVar], validate$yhat)
        testF1 <- meanf1(test[, depVar], test$yhat)
        diagnostic <- c(paste(paste(include, collapse = ", "),
                              if(length(include)) (", "),
                              paste(i, collapse = ", ")), validateF1, testF1)
        return(diagnostic)
      })
      master <- do.call("rbind", l)
      return(master)
    })
    superMaster <- do.call("rbind", ll)
    }
  if(model %in% c("OLS", "binary probit", "binary logit")){
    colnames(superMaster) <- c("independent_variables", "validate_mape", "test_mape")
    #Store sorted data frame to work space
    lowest_mape_validate <- superMaster[order(superMaster[, 2]), ]
    row.names(lowest_mape_validate) <- NULL
    lowest_mape_validate <- lowest_mape_validate[!duplicated(lowest_mape_validate), ]
    lowest_mape_validate <<- lowest_mape_validate
    lowest_mape_test <- superMaster[order(superMaster[, 3]),]
    row.names(lowest_mape_test) <- NULL
    lowest_mape_test <- lowest_mape_test[!duplicated(lowest_mape_test), ]
    lowest_mape_test <<- lowest_mape_test
    } else {
      colnames(superMaster) <- c("independent_variables", "validate_f1", "test_f1")
      #Store sorted data frame to work space
      highest_f1_validate <- superMaster[order(superMaster[, 2], decreasing = TRUE), ]
      row.names(highest_f1_validate) <- NULL  
      highest_f1_validate <- highest_f1_validate[!duplicated(highest_f1_validate), ]
      highest_f1_validate <<- highest_f1_validate
      highest_f1_test <- superMaster[order(superMaster[, 3], decreasing = TRUE), ]
      row.names(highest_f1_test) <- NULL
      highest_f1_test <- highest_f1_test[!duplicated(highest_f1_test), ]
      highest_f1_test <<- highest_f1_test
  }
  #Return message
  message("Data frames with sorted specifications are stored in work space.")

}




