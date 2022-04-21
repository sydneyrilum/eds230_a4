#' Compute atmospheric conductance (Cat)
#' 
#' This function calculates atmospheric conductance (Cat), which is how easily vapor diffuses from vegetation surfaces
#' 
#' @param zm height at which windspeed is measured in cm, usually 200cm above the vegetation 
#' @param h vegetation height (cm)
#' @param v windspeed (cm/sec)
#' @param kd default value of 0.7
#' @param ko default value of 0.1
#' @author Mia Forsline, Kristin Gill, Sydney Rilum
#' @examples compute_cat(zm = 300, h = 100, v = 20, kd = 0.7,  ko = 0.1)
#' @return value in $


compute_npv <- function(value, time, discount = 0.12) {
  result = value / (1 + discount)**time
  return(result)
}

