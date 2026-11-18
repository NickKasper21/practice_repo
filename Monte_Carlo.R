#'Monte Carlo Random Point Simulation    
#'Original Coder: Nick Kasper 

#'@description Generate random (x,y) points that fall within given bounds


#'@param x_min (num) the lowest bound for the x-coordinates
#'@param x_max (num) the highest bound for the x-coordinates
#'@param y_min (num) the lowest bound for the y-coordinates
#'@param y_max (num) the highest bound for the y-coordinates
#'@param n (int) the number of points that will be randomly placed 

#'@returns a data frame, that has a number of rows that is equivalent to n, 
#'with two columns, one for the random x and one for the random y values. 

#'@details the x-value and the y-value are both picked randomly between the bounds, 
#'every bound within the rectangle has an equal chance to get placed. These (x,y)
#'numbers are then placed in pairs at random. 
monte_points <- function(x_min, x_max, y_min, y_max, n){
  x_values <- sample(x = seq(from = x_min, to = x_max, by = 0.001), size = n, 
                     replace = TRUE) #randomly choose different x-values within
  #bounds

  y_values <- sample(x = seq(from = y_min, to = y_max, by = 0.001), size = n, 
                   replace = TRUE) #randomly choose different y-values within
#bounds

data_points <- data.frame(x = x_values, y = y_values)
return(data_points) #pair these randomly and make a data frame
}

#Monte Carlo Point Flagging and Data Frame 
##Flag the amount of points that are either below or are on the plot curve and 
##find the integral estimation using the given bounds 
##Needs the data frame created above 

#Load needed packages - tidyverse 
library(tidyverse)
library(patchwork)

create_plot <- function(n){

monte_data <- monte_points(x_min = 0, x_max = 4, y_min = 0, y_max = 0.8, n = n)

#Flag all of the points that are on the curve or below it given the shape 
monte_flagged <- monte_data |>
  mutate(
    y_curve = dweibull(x, shape = 1.5, scale = 1), 
    flag = if_else(y <= y_curve, "below/on", "above")
  )

#Estimate the integral 
area <- (4 - 0) * (0.8 - 0)
total_below <- sum(monte_flagged$flag == "below/on") #total that are below/on
percent_below <- total_below / nrow(monte_flagged) #the percent of the whole 
integral_estimate <- area * percent_below 

#Plot the curve, and then the points 
ggplot(data = monte_flagged,
       mapping = aes(
         x = x, 
         y = y, 
       )) + 
  geom_point(mapping = aes(
         color = flag), alpha = 0.5) +
  stat_function(
    fun = dweibull, 
    args = list(shape = 1.5, scale = 1), 
    xlim = c(0,4)
  ) + 
  scale_color_manual(values = c("above" = "blue", "below/on" = "red")) +
  labs(
    title = paste("Monte Plot for n=", nrow(
      monte_flagged)),
    x = "x", 
    y = "y", 
    color = "Flag", 
    caption = paste("Numerical Integration Est: ", round(integral_estimate, 3))
  ) + theme_bw()
}

#make all four of the plots 
plot_10 <- create_plot(10)
plot_100 <- create_plot(100)
plot_1000 <- create_plot(1000)
plot_10000 <- create_plot(10000)

#combine all of the plots into one picture 
plot_10 + plot_100 + plot_1000 + plot_10000