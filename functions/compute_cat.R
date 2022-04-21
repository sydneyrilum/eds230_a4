#' Compute atmospheric conductance (Cat)
#' 
#' This function calculates atmospheric conductance (Cat), which is how easily vapor diffuses from vegetation surfaces
#' 
#' @param zm height at which windspeed is measured in cm, usually 200cm above the vegetation 
#' @param h vegetation height (cm)
#' @param v windspeed (cm/sec)
#' @param kd default value of 0.7
#' @param k0 default value of 0.1
#' @author Mia Forsline, Kristin Gill, Sydney Rilum
#' @examples compute_cat(zm = 300, h = 100, v = 20, kd = 0.7,  ko = 0.1)
#' @return atmospheric conductance (units?)


compute_cat <- function(zm, h, v, kd = 0.7, k0 = 0.1) {
  if(zm - h < 200) stop("The height at which windspeed is measured in cm must be 200cm above the vegetation height")
  
  zd = kd * h
  
  z0 = k0 * h
  
  cat = v / (6.25 * log( ((zm - zd) / z0 )**2) )
  
  return(cat)
}

