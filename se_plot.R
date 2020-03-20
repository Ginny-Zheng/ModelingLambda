#' Plot relative error and standard error of OLR and YORK estimates with varying x range
#'
#' This function allows you to plot relative error and standard error of estimated OLR and YORK slope 
#' with different range of X. relative error = (estimation-true)/true
#' @param b_true true value of the slope.
#' @param max_x_range maximum range of X values.
#' @param result output from vary_x_range function with same slope and max_x_range.
#' @param error_only logical: if TRUE, plot standard error of the estimated slope.
#' if FALSE, plot relative error of estimated OLR and YORK slope. Defaults to be FALSE.
#' @import rgr
#' @import IsoplotR
#' @examples 
#' est_value <- vary_x_range(b_true = 3, x_sdev = 0.3, y_sdev = 0.3, max_x_range = 5, numRuns = 200)
#' plot_error(b_true = 3, max_x_range = 5, result = est_value, error_only = TRUE)

plot_error <- function(max_x_range,b_true, result, error_only = FALSE){
  
  x_range_1 <- seq(1, max_x_range, by = 0.1)
  if(error_only == FALSE){
    plot(x_range_1, (result$OLR_beta-b_true)/b_true, 'l', xlab = 'X Range', ylab = 'Relative Error',
         main = paste("True Beta ", b_true), lwd = 2, 
         ylim = c(min((result$OLR_beta-2*result$OLR_se-b_true)/b_true,(result$YORK_beta-2*result$YORK_se-b_true)/b_true),
                  max((result$OLR_beta+2*result$OLR_se-b_true)/b_true, (result$YORK_beta+2*result$YORK_se-b_true)/b_true)))
    lines(x_range_1, (result$OLR_beta+2*result$OLR_se-b_true)/b_true, col = 'black', lty = 2)
    lines(x_range_1, (result$OLR_beta-2*result$OLR_se-b_true)/b_true, col = 'black', lty = 2)
    
    lines(x_range_1, (result$YORK_beta-b_true)/b_true, col = 'red', lwd = 2)
    lines(x_range_1, (result$YORK_beta+2*result$YORK_se-b_true)/b_true, col = 'red', lty = 2)
    lines(x_range_1, (result$YORK_beta-2*result$YORK_se-b_true)/b_true, col = 'red', lty = 2)
    legend("bottomright", legend = c("OLR", "YORK"), lty = 1,
           col = c("black", 'red'))
  }else{
    plot(x_range_1, result$OLR_se, 'l', lwd = 2, xlab = "X Range", ylab = "SE",
         ylim = c(min(result$OLR_se, result$YORK_se),
                  max(result$OLR_se, result$YORK_se)),
         main = paste("True Beta", b_true))
    lines(x_range_1, result$YORK_se, col = 'red', lwd = 2)
    legend("topright", legend = c("OLR", "YORK"), lty = 1,
           col = c("black", 'red'))
  }
  
}

