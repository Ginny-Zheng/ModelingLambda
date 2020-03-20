#' Plot the relative error of OLR and YORK estimates with varying x range
#'
#' This function allows you to plot the relative error of estimated OLR and YORK slope with different range of X.
#' @param b_true true value of the slope.
#' @param max_x_range maximum range of X values.
#' @param result output from vary_x_range function with same slope and max_x_range.
#' @import rgr
#' @import IsoplotR
#' @import tidyr
#' @examples 
#' est_value <- varying_x_range(b_true = 3, x_sdev = 0.3, y_sdev = 0.3, max_x_range = 20, numRuns = 1000)
#' plot_beta_error(max_x_range = 20, b_true = b_true, result = est_result, x_intercept = 2.2)

plot_beta_error <- function(max_x_range, b_true, result, x_intercept){
  
  x_range_1 <- seq(1, max_x_range, by = 0.1)
  result$x <- x_range_1
  temp1 <- result %>% gather(key = 'x', value = "est", OLR_beta, YORK_beta) %>% select(x, est)
  temp2 <- result %>% gather(key = 'x', value = "est", OLR_se, YORK_se) %>% select(x, est)
  data1 <- data.frame(range = c(x_range_1, x_range_1), temp1, temp2)
  data1$x <- as.factor(data1$x)
  
  ## plot relative error
  ggplot(data = data1, aes(x = range, y = est, color = x)) + ylim(c(-0.5, 0.3))+
    geom_point(aes(y = (est-b_true)/b_true), size = 2, shape = 19) +
    geom_point(aes(y = (est+2*est.1-b_true)/b_true), size = 1.2, shape = 19, alpha = 0.5) +
    geom_point(aes(y = (est-2*est.1-b_true)/b_true), size = 1.2, shape = 19, alpha = 0.5) +
    theme_minimal()+ theme(legend.position = c(0.88, 0.2)) +
    scale_color_manual(values=c("firebrick1", "dodgerblue3"), 
                       name="Regression\nMethods",
                       labels=c("OLR", "YORK")) +
    ggtitle(paste(c("Relative Error of the Estimated Beta (True Beta ="), b_true,c(")"))) +  xlab("Range of X") + 
    ylab("Relative Error") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.5))) +
    theme(legend.title = element_text(size=13), legend.text = element_text(size = 13),
          axis.title.y = element_text(size = rel(1.3), margin = margin(0,10,0,0)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.3), margin = margin(5,0,0,0)),
          plot.margin = margin(t = 15, r = 10, b = 10, l = 10, unit = "pt")) +
    geom_hline(yintercept = -0.05, colour = 'green3', lwd = 1.2, lty = 5) +
    geom_vline(xintercept = x_intercept, color = 'green3', lwd = 1.2, lty = 5) +
    geom_text(x=3, y=-0.3, label=x_intercept, color = "green3", size=6)
  
}
