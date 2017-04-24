#This version of function 4 is much longer, but takes less time to evaluate users'inputs (model)

optimizer <- function(df, dependentVar, independentVar = colnames(df)[colnames(df) != dependentVar], include = NULL, 
                      model = c("OLS", "binary probit", "binary logit", "ordered probit", 
                                    "ordered logit", "multinomial logit", "multinomial probit"), 
                      time.series = FALSE, time.var = NULL,
                      save.csv = FALSE) {
  #####################################Functions to use##########################################################
  ###Mape function for calcultation of mape###
  mape <- function(yhat, y){
    return(100*mean(abs(yhat/y - 1), na.rm=T))
  }
  
  ###Paste independent variables stored in function###
  pasteIV <- function() {
    ivList <- combination[i]
    ivList <- unlist(ivList)
    ivList <<- ivList
    pasta <- ""
    x <- 1
    while(x < length(ivList)) {
      pasta <- paste0(pasta, ivList[x], "+")
      x <- x + 1
    }
    pasta <- paste0(dependentVar, "~", pasta, ivList[length(ivList)], include.paste)
    pasta <<- pasta
  }
  ###Predict yhat & calculate MAPE & append master data frame stored in function###
  #Predict validate and test sets 
  predCalAppend <- function() {
    validate$yhat <- predict(fit, newdata = validate)
    test$yhat <- predict(fit, newdata = test)
    #calculate MAPE for validate and test sets
    validateMape <- mape(validate$yhat, validate[, dependentVar])
    testMape <- mape(test$yhat, test[, dependentVar])
    include <<- include
    diagnostic <- c(paste(paste(include, collapse = ", "), ", ", paste(ivList, collapse = ", ")), validateMape, testMape)
    diagnostic <<- diagnostic
    master <- rbind(master, diagnostic)
    master <<- master
  }
  ################################################################################################################
  
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
        #Paste IVs (including "include") & DV to a formula
        pasteIV()
        
        #Run regression
        fit <- lm(noquote(pasta), data = train)
        
        #Run predCalAppend
        predCalAppend()
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
          #Paste IVs (including "include") & DV to a formula
          pasteIV()
          #Run regressions
          fit <- glm(noquote(pasta), family = binominal(link = "probit"), data = train)
          #Predict yhats, calculate MAPE
          predCalAppend()
          }
        superMaster <- rbind(superMaster, master)
        
        }
      } else if(model == "binary logit") {
        superMaster <- c()
        for(i in 1:length(independentVar)) {
          combination <- combn(independentVar, i, simplify = FALSE)
          master <- c()
          for(i in 1:length(combination)) {
            #Paste IVs (including "include") & DV to a formula
            pasteIV()
            #RUn regressions
            fit <- glm(noquote(pasta), family = binominal(link = "lobit"), data = train)
            #Run predCalAppend
            predCalAppend()
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
            #Paste IVs (including "include") & DV to a formula
            pasteIV()
            #RUn regressions
            fit <- polr(noquote(pasta), data = train, method = "probit")
            #Run predCalAppend
            predCalAppend()
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
            #Paste IVs (including "include") & DV to a formula
            pasteIV()
            #RUn regressions
            fit <- polr(noquote(pasta), data = train, method = "logit")
            #Run predCalAppend
            predCalAppend()
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
            #Paste IVs (including "include") & DV to a formula
            pasteIV()
            #RUn regressions
            fit <- mlogit(noquote(pasta), data = train)
            #Run predCalAppend
            predCalAppend()
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
            #Paste IVs (including "include") & DV to a formula
            pasteIV()
            #RUn regressions
            fit <- mlogit(noquote(pasta), data = train, probit = TRUE)
            #Run predCalAppend
            predCalAppend()
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
  #Return message
  message("Data frames sorted by validate set MAPE and test set MAPE are stored in work space.")
  #Save .csv files
  if(save.csv) {
    write.csv(bestValidate, "whatever")
    write.csv(bestTest, "whatever2")
  }
  rm(list = c("diagnostic", "include.paste", "ivList", "independentVar", "pasta"))
}


temp <- tempfile()
download.file("https://github.com/GeorgetownMcCourt/data-science/raw/master/homeworks/homework2/toStudents/homework2_data.Rda", temp, mode="wb")
load(temp)
hmwk$date <- as.Date(hmwk$date, "%Y-%mm-%dd")
hmwk <- hmwk[hmwk$complete == TRUE, ]
hmwk <- hmwk[hmwk$GEOID == "10180", ]
hmwk <- hmwk[, -1]
a <- proc.time()[3]
optimizer(hmwk, dependentVar = "emp",  model = "OLS", time.series = TRUE, time.var = "date")
proc.time()[3]-a  
