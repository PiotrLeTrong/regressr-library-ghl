rm(list = ls())
## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    if (!require(x, character.only = TRUE)) 
      stop("Package not found")
  }
}

## These lines specify your packages
packages <- c("XML")

##Run code
invisible(lapply(packages, pkgTest))
require(XML)

for(i in 2008:2016){
  url <- paste0("http://www.cfbstats.com/", i, "/leader/national/team/offense/split01/category09/sort01.html")
  assign(paste0("cfb.offense.scoring.",i), data.frame(readHTMLTable(url), year = i))
}

for(i in 2008:2016){
  url <- paste0("http://www.cfbstats.com/", i, "/leader/national/team/defense/split01/category09/sort01.html")
  assign(paste0("cfb.defense.scoring.",i), data.frame(readHTMLTable(url), year = i))
}

offense.list <- list(
  cfb.offense.scoring.2008, 
  cfb.offense.scoring.2009, 
  cfb.offense.scoring.2010, 
  cfb.offense.scoring.2011, 
  cfb.offense.scoring.2012, 
  cfb.offense.scoring.2013, 
  cfb.offense.scoring.2014, 
  cfb.offense.scoring.2015, 
  cfb.offense.scoring.2016 
)
defense.list <- list(
  cfb.defense.scoring.2008, 
  cfb.defense.scoring.2009, 
  cfb.defense.scoring.2010, 
  cfb.defense.scoring.2011, 
  cfb.defense.scoring.2012, 
  cfb.defense.scoring.2013, 
  cfb.defense.scoring.2014, 
  cfb.defense.scoring.2015, 
  cfb.defense.scoring.2016
)

offense.list <- lapply(offense.list, function(x){
  x <- x[,-1]
  colnames(x) <- gsub("NULL.","", colnames(x))
  x
})

defense.list <- lapply(defense.list, function(x){
  x <- x[,-1]
  colnames(x) <- gsub("NULL.","", colnames(x))
  x
})

offense.scoring <- do.call("rbind", offense.list)
defense.scoring <- do.call("rbind", defense.list)


names(offense.scoring)[2:9] <- paste("offensive",
                                     names(offense.scoring)[2:9], sep=".")
names(defense.scoring)[2:9] <- paste("defensive",
                                     names(defense.scoring)[2:9], sep=".")

cfb.scoring <- merge(offense.scoring,
                     defense.scoring,
                     by = c("Name", "year"),
                     all = T)
#### We're just making as many combinations of T/F statements####
cfb.scoring$isCal <- 0
cfb.scoring$isCal[cfb.scoring$Name == "California"] <- 1
cfb.scoring$isUCLA <- "0"
cfb.scoring$isUCLA[cfb.scoring$Name == "UCLA"] <- "1"
cfb.scoring$isStanford <- F
cfb.scoring$isStanford[cfb.scoring$Name == "Stanford"] <- T
cfb.scoring$isUSC <- "F"
cfb.scoring$isUSC[cfb.scoring$Name == "USC"] <- "T"
cfb.scoring$isTexas <- "False"
cfb.scoring$isTexas[cfb.scoring$Name == "Texas"] <- "True"
cfb.scoring$SeasonStartDate <- as.Date(paste0("09/01/", cfb.scoring$year), "%m/%d/%Y")
cfb.scoring$SeasonEndDate <- as.character(as.Date(paste0("01/09/", cfb.scoring$year+1), "%m/%d/%Y"))
cfb.scoring$noDataChar <- NA
cfb.scoring$noDataChar[sample(nrow(cfb.scoring), 
                              size = round(0.5*nrow(cfb.scoring)), 
                              replace = F)] <- "Data"
cfb.scoring$noDataNum <- NA
cfb.scoring$noDataNum[sample(nrow(cfb.scoring), 
                              size = round(0.5*nrow(cfb.scoring)), 
                              replace = F)] <- 1
cfb.scoring$noDataLogi <- NA
cfb.scoring$noDataLogi[sample(nrow(cfb.scoring), 
                              size = round(0.25*nrow(cfb.scoring)), 
                              replace = F)] <- TRUE
cfb.scoring$noDataLogi[sample(nrow(cfb.scoring), 
                              size = round(0.25*nrow(cfb.scoring)), 
                              replace = F)] <- FALSE
cfb.scoring$noDataDate <- as.Date(NA)
cfb.scoring$noDataDate[sample(nrow(cfb.scoring), 
                              size = round(0.5*nrow(cfb.scoring)), 
                              replace = F)] <- as.Date(paste0("09/01/", cfb.scoring$year), "%m/%d/%Y")
save(cfb.scoring, file = "/Users/piotr/Dropbox/School/Spring 2017/670 Data Sci/DataSciProject/data/1.raw/footballdata.RData")