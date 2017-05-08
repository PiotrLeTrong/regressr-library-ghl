#Data Science Final Project
#' Dependent Variable Evaluator Function
#'
#' This function's main use is to check the nature of the dependent variable, suggest a model type, and explain the model to the user.
#' @param depvar What is the name of your dependent variable?
#' @param order Is your variable ordinal? Default is set to FALSE
#' @param panel Is your data in panel or cross sectional form? Default is set to FALSE
#' @export
#' @examples
#' 

#User inputs a dependent variable
dvaluator <- function(depvar, order = F, panel = F) {
  if (is.numeric(depvar) == T) {
    writeLines(paste("Your dependent variable is continuous, like age and temperature, continuous variables move along a potentially infinite range. I recommend you use Orindary Least Squares Regression. This model type is intuitive to interpret and minimizes the error between observed data and our predictions by projecting a linear relationship between the dependent and independenet variables."))
  }else if ((is.logical(depvar) == T)) {
    writeLines(print("Your dependent variable is binary, meaning it has only two outcomes such as on and off, true or false. (0 at the minimum, and 1 at the maximum. Note: The coefficients cannot be interpreted directly; they must be converted to odds ratios or marginal effects."))
  }else if (is.factor(depvar) == T & order == F) {
    writeLines(paste("Your dependent variable is categorical; it represents discrete categories without a clear order like data with a series of S-shaped curves originating near 0 and terminating near 1. Like a logit model, this model outputs coefficients that cannot be interpreted directly and must be converted to odds ratios or marginal effects."))
  }else if (is.factor(depvar) == T & order == T) {
   writeLines(paste("Your dependent variable is ordinal; it is divided into discrete categories that follow a clear order. Examples of ordinal variables include Likert Scales: Agree, Neutral, Disagree; and class rankings: 1st, 2nd, 3rd, etc. I recommend using an ordered probit regression, it represents discrete categories without a clear order like data with a series of S-shaped curves originating near 0 and terminating near 1. Coefficients cannot be interpreted directly, but positive coefficients indicate movement towards the higher end of the dependent variable, whie negative coefficients signal a move toward a lower level of the dependent variable."))
  }else if (is.numeric(depvar) == T & panel == T) {
    writeLines(paste("You indicated you have panel data: data that tracks multiple observations across multiple points in time. I recommend using a fixed-effects model. Fixed-effects assumes there are certain characteristics of an independent varaible that do not change over time. For example if the independent variable is a city, we might expect its demography or statutes to not vary significantly from one day to the next. Fixed-effects lets us control for these unchanging but relevant factors, even if we don't have specific data on those factors. Warning: if a feature of your data happens to be one of these unchanging features, you will not be able to observe those characteristics in a fixed-effects model."))
  }
  writeLines(paste("\nNow that you know the nature of you dependent variable and what model you'd like to run, I recommend using the next function in the library, (name of function). All you have to do is feed it a dependent variable and a set of independent variables. A dependent variable is the outcome variable you want to know something about. The independent variables are those factors you think explain your dependent variable. For instance, if I'm interested in test scores, the scores would be my dependent variable, and my independent variables might be data on the test takers and the schools they attend."))
}