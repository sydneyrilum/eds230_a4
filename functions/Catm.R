#' Compute atmospheric conductance (Cat)
#' 
#' This function calculates atmospheric conductance (Cat) in cm/sec, which is how easily vapor diffuses from vegetation surfaces
#' 
#' @param h vegetation height (cm)
#' @param v windspeed (cm/sec)
#' @param k_d default value of 0.7
#' @param k_0 default value of 0.1
#' @author Mia Forsline, Kristin Gill, Sydney Rilum
#' @examples compute_cat(zm = 300, h = 100, v = 20, kd = 0.7,  ko = 0.1)
#' @return atmospheric conductance (cm/sec)


Catm <- function(h, v, k_d = 0.7, k_0 = 0.1) {
  # if(zm - h < 200) stop("The height at which windspeed is measured in cm must be 200cm above the vegetation height")
  
  zd = k_d * h
  
  z0 = k_0 * h
  
  zm = 200 + h 
  
  cat = v / (6.25 * (log( ((zm - zd) / z0 ) )**2) )
  
  return(cat)
}

