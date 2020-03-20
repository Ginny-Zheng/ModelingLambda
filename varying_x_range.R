#' Compute OLR and YORK estimates with varying x range
#'
#' This function allows you to estimate OLR and YORK slope with different range of X.
#' @param numRuns number of iterations. Defaults to be 1000.
#' @param b_true true value of the slope.
#' @param max_x_range maximum range of X values.
#' @param x_sdev standard deviation of X.
#' @param y_sdev standard deviation of Y.
#' @import rgr
#' @import IsoplotR
#' @export data.frame estimated slope and standard error for OLR and YORK models with
#' X range increasing by 0.1 from 1 to max_x_range.
#' @examples
#' vary_x_range(b_true = 3, x_sdev = 0.3, y_sdev = 0.3, max_x_range = 5, numRuns = 200)



vary_x_range <- function(b_true, max_x_range, x_sdev, y_sdev, numRuns = 1000, numPoints = 100){
  x_range_1 <- seq(1, max_x_range, by = 0.1)
  OLR_beta <- rep(100, length(x_range_1))
  YORK_beta <- rep(100, length(x_range_1))
  OLR_R <- rep(100, length(x_range_1))
  OLR_se <- rep(100, length(x_range_1))
  YORK_se <- rep(100, length(x_range_1))
  YORK_R <- rep(100, length(x_range_1))
  YORK_pvalue <- rep(100, length(x_range_1))
  for (i in 1:length(x_range_1)) {
    a <- simulation_fun(b_true = b_true, x_range = x_range_1[i], x_sdev = x_sdev,
                  y_sdev = y_sdev, numRuns = numRuns, numPoints = numPoints)
    OLR_beta[i] <- a[1]
    OLR_se[i] <- a[2]
    OLR_R[i] <- a[3]

    YORK_beta[i] <- a[4]
    YORK_se[i] <- a[5]
    YORK_R[i] <- a[6]
    YORK_pvalue[i] <- a[7]
  }
  result <- data.frame(OLR_beta = OLR_beta, OLR_se = OLR_se, OLR_R = OLR_R,
                       YORK_beta = YORK_beta, YORK_se = YORK_se, YORK_R = YORK_R, YORK_pvalue = YORK_pvalue)
  return(result)
}
