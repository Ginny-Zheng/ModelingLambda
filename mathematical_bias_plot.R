#### plot mathematical bias
## find tipping point 
a <- seq(1,20, by=0.01)
y <- a^2/(a^2+12*0.15^2)

library(ggplot2)
library(ggrepel)
#### Cl vs C
data <- data.frame(a=a,y = y, col = c(rep("Greater than 5%", 127), rep("Less than 5%", 1774)))
data$col <- as.factor(data$col)
#qplot(a, (y-1), data=data, colour=col) + 
#scale_colour_gradientn(colours = rainbow(10)) + 
ggplot(data = data, aes(x = a, y = (y-1), color = col))+ geom_line(size = 2)+
  geom_hline(yintercept = -0.05, colour = 'blue') +
  geom_vline(xintercept = 2.265, color = 'blue') +
  geom_text(x=3, y=-0.15, label="2.265", color = "red", size=6) +labs(color = "Relative Bias") +
  ggtitle("Relative Bias of OLR--Cl vs C") +  xlab("Range of X") + ylab("Relative Bias") +
  theme(plot.title = element_text(hjust = 0.5, size = rel(2))) +
  theme(legend.position = c(0.83, 0.2))+
  theme(axis.text.x = element_text(size = rel(1.5), margin = margin(10,0,0,0)),
        axis.text.y = element_text(size = rel(1.5), margin = margin(0,10,0,0)),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt"),
        axis.title.y = element_text(size = rel(1.5), margin = margin(0,20,0,0)),
        axis.title.x = element_text(size = rel(1.5), margin = margin(10,0,0,0)),
        legend.title = element_text( size=16), 
        legend.text = element_text(size = 14))


#### C vs H
a <- seq(1,50, by=0.01)
y <- a^2/(a^2+12*0.25^2)
data <- data.frame(a=a,y = y, col = c(rep("Greater than 5%", 277), rep("Less than 5%", 4624)))
#qplot(a, (y-1), data=data, colour=a) + 
#  scale_colour_gradientn(colours = rainbow(10)) + 
ggplot(data = data, aes(x = a, y = (y-1), color = col))+ geom_line(size = 2)+
  geom_hline(yintercept = -0.05, colour = 'blue') +
  geom_vline(xintercept = 3.775, color = 'blue') +
  geom_text(x=5.5, y=-0.29, label="3.775", color = "red", size=6) +labs(color = "Relative Bias") +
  ggtitle("Relative Bias of OLR--C vs H") +  xlab("Range of X") + ylab("Relative Bias") +
  theme(plot.title = element_text(hjust = 0.5, size = rel(2))) +
  theme(legend.position = c(0.8, 0.2))+
  theme(axis.text.x = element_text(size = rel(1.5), margin = margin(10,0,0,0)),
        axis.text.y = element_text(size = rel(1.5), margin = margin(0,10,0,0)),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt"),
        axis.title.y = element_text(size = rel(1.5), margin = margin(0,20,0,0)),
        axis.title.x = element_text(size = rel(1.5), margin = margin(10,0,0,0)),
        legend.title = element_text( size=16), 
        legend.text = element_text(size = 14)) # face = "bold"

