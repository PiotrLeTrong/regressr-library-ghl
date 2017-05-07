#' Package Testing Function
#'
#' This function tests whether certain packages are installed on this R instance for some of the other functions to run. It 
#' It can also serve as a way for the user to test for other packaeges
#' @param package Here you can inpute a vector of names of packages you wish to install/run on this computer in 1 go.
#' @keywords package
#' @export
#' @examples
#' package.tester()
package.tester <- function(package){
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