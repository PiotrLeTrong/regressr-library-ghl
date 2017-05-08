#' A summary function
#'
#' This function gives someone a sky-in-the-eye view of the data as it looks into the structure of the data frame and gives reccomendations.
#' @param df This is where a data frame goes. Not a lot of science here. 
#' @keywords summary statistics
#' @export
#' @examples
#' summarizer()

summarizer <- function(df){
  output <- data.frame()
  colcounter <- colnames(df)
  colVect <- c()
  NumOfNA <- c()
   for(i in 1:ncol(df)){
     colVect <- df[[i]]
     NumOfNA <- sum(is.na(colVect))
     colVect <- colVect[complete.cases(colVect)]
    if (class(colVect) == "factor"){
      colVect <- as.character(colVect)
    }
    if (grepl("-|/[^A-z]", colVect[runif(1,1,length(colVect))],  perl=TRUE) == TRUE & class(colVect) !="Date"){
      if(grepl("-|[^A-z]", colVect[runif(1,1,length(colVect))],  perl=TRUE) == TRUE & class(colVect) !="Date"){
        colVect <- as.Date(colVect, "%Y-%m-%d")
      } else {
        colVect <- as.Date(colVect, "%Y/%m/%d")
      }
    }
    if (grepl("[A-z]", colVect[runif(1,1,length(colVect))],   perl=TRUE) == FALSE & class(df[[i]]) !="Date"){
      colVect <- as.numeric(colVect)
    }
    if (min(colVect) == 0 & max(colVect) == 1){
      colVect <- as.logical(colVect)
    }
    if (grepl("^False$|^FALSE$|^F$", colVect[runif(1,1,length(colVect))],  perl=TRUE) == TRUE | grepl("^True$|^TRUE$|^T$", colVect[runif(1,1,length(colVect))],  perl=TRUE) == TRUE){
      colVect <- as.logical(colVect)
    }
    output <- rbind(output,
                    data.frame(VarName = colcounter[i],
                               VarType = class(colVect),
                               NumberocfNAs = NumOfNA))
   }
  definitions <- data.frame(VarType = c("numeric",
                                        "character",
                                        "factor",
                                        "Date",
                                        "logical"),
                            VarDefinition = c("Numeric variables are straight forward: continous variables.",
                                              "These are categorical variables, thus are seen as discrete variables.",
                                              "These are categorical variables, similar to characters, but are ordered.",
                                              "Time Series Variable that makes the data set either time-series based or panel.",
                                              "These variables are True/False binary variables."))
  output <- merge(output, definitions, by = "VarType", all.x = T)
  output <- output[,c(2,1,4,3)]

  print(output)
  for(i in 1:nrow(output)){
    if(output[i,4] > 0.1*nrow(df)){
      writeLines(paste0(sprintf("%.3f",100*(output[i,4]/nrow(df))), "% of the ", output[i,1]," variables have missing observations. This can cause some regressions to not encompass the whole sample."))
    }
  }
  writeLines("\n Ok, it looks like you are ready to run the second function of the library that will help you define the dependent and independent variables.")
}
