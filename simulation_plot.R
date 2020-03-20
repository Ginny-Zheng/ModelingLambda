#' A Monte Carlo Simulation plot
#'
#' This function allows you to plot simulated data.
#' @param numPoints number of data points in each simulation. Defaults to be 100.
#' @param b_true true value of the slope.
#' @param x_range range of X values.
#' @param x_sdev standard deviation of X.
#' @param y_sdev standard deviation of Y.
#' @param plotTRUE logical value. If TRUE, plot data without error. 
#' If FALSE, plot data with normal error.
#' @keywords Monte Carlo Simulation
#' @import rgr
#' @import IsoplotR
#' @examples 
#' simulation_plot(b_true = 3, x_sdev = 0.3, y_sdev = 0.3, x_range = 5, plotTRUE = TRUE)

simulation_plot <- function(numPoints = 100, b_true, x_range, x_sdev, y_sdev, plotTRUE)
{
  x_true <- runif(n = numPoints, min = 0, max = x_range)
  #rnorm(n = numPoints, mean = 10, sd = 5)
  y_true <- x_true*b_true
  
  # add error term
  x_err <- rnorm(n = numPoints, mean = 0, sd = x_sdev)
  x_obs <- x_true + x_err
  
  y_err <- rnorm(n = numPoints, mean = 0, sd = y_sdev)
  y_obs <- y_true + y_err
  
  if(plotTRUE == TRUE)
  {
    plot(y_true~x_true)
  }
  else
  {
    plot(y_obs~x_obs)
  }
  
}
