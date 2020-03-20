#' A Monte Carlo Simulation Function
#'
#' This function allows you to run Monte Carlo Simulation to get estimated slope b
#' from OLR and YORK models.
#' @param numRuns number of iterations. Defaults to be 1000.
#' @param numPoints number of data points in each simulation. Defaults to be 100.
#' @param b_true true value of the slope.
#' @param x_range range of X values.
#' @param x_sdev standard deviation of X.
#' @param y_sdev standard deviation of Y.
#' @keywords Monte Carlo Simulation
#' @import rgr
#' @import IsoplotR
#' @export estimations: estiamted OLR slope, estimated standard error of OLR slope,
#' estimated YORK slope, estimated standard error of YORK slope.
#' @examples 
#' simulation_fun(b_true = 3, x_sdev = 0.3, y_sdev = 0.3, x_range = 5)

simulation_fun <- function(numRuns = 1000, numPoints = 100, b_true,
                     x_range, x_sdev, y_sdev)
{
  x_true <- runif(n = numPoints, min = 0, max = x_range)
  #rnorm(n = numPoints, mean = 10, sd = 5)
  y_true <- x_true*b_true
  
  result <- matrix(0, numRuns, 4)
  ## iteration
  for (iter in 1:numRuns) {
    # add error term
    x_err <- rnorm(n = numPoints, mean = 0, sd = x_sdev)
    x_obs <- x_true + x_err
    
    y_err <- rnorm(n = numPoints, mean = 0, sd = y_sdev)
    y_obs <- y_true + y_err
    
    ## model fitting
    # OLR
    OLRoutput <- lm(y_obs~x_obs)
    OLRsum <- summary(OLRoutput)
    OLR <- OLRsum$coefficients[2,1]
    OLRse <- OLRsum$coefficients[2,2]
    
    # YORK
    tempYork <- cbind(x_obs, x_sdev, y_obs, y_sdev)
    Yorkoutput <- york(tempYork, alpha = 0.05)
    YORK <- Yorkoutput$b[1]
    YORKse <- Yorkoutput$b[2]
    
    result[iter, ] <- c(OLR, OLRse, YORK, YORKse)
    
  }
  
  colnames(result) <- c("OLR Slope","OLR SE", 
                        "YORK Slope", "YORK SE")
  rownames(result) <- seq(1:numRuns)
  
  result_sum <- apply(result, 2, mean)
  
  return(result_sum)
}
