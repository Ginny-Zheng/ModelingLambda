#' Plot OLR and YORK estimates with varying x range
#'
#' This function allows you to plot estimated OLR and YORK slope with different range of X.
#' @param b_true true value of the slope.
#' @param max_x_range maximum range of X values.
#' @param result output from vary_x_range function with same slope and max_x_range.
#' @import rgr
#' @import IsoplotR
#' @examples 
#' est_value <- vary_x_range(b_true = 3, x_sdev = 0.3, y_sdev = 0.3, max_x_range = 5, numRuns = 200)
#' plot_beta(b_true = 3, max_x_range = 5, result = est_value)

plot_beta <- function(max_x_range, b_true, result){
  
  x_range_1 <- seq(1, max_x_range, by = 0.1)
  
  plot(x_range_1, result$OLR_beta, 'l', xlab = 'X Range', ylab = 'Estimated Beta',
       main = paste("True Beta ", b_true), lwd = 2, 
       ylim = c(min(result$OLR_beta-2*result$OLR_se, result$YORK_beta-2*result$YORK_se),
                max(result$OLR_beta+2*result$OLR_se, result$YORK_beta+2*result$YORK_se)))
  lines(x_range_1, result$OLR_beta+2*result$OLR_se, col = 'black', lty = 2)
  lines(x_range_1, result$OLR_beta-2*result$OLR_se, col = 'black', lty = 2)
  
  lines(x_range_1, result$YORK_beta, col = 'red', lwd = 2)
  lines(x_range_1, result$YORK_beta+2*result$YORK_se, col = 'red', lty = 2)
  lines(x_range_1, result$YORK_beta-2*result$YORK_se, col = 'red', lty = 2)
  legend("bottomright", legend = c("OLR", "YORK"), lty = 1,
         col = c("black", 'red'))
}
