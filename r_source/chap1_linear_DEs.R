#######################################
### Linear Difference Equations
#######################################

# Clear the workspace and load libraries
rm(list = ls())
library(reshape2)
library(base)
library(ggplot2)
library(grid)
library(stringr)
library(tidyverse)


########### 
# Iterates forward a difference equation with known starting value
#     y_0 : starting value
#     a,b : parameters of the difference equation
#     T   : for how many periods to iterate
########### 
diff_eq <- function(y_0, a, b, T) {
  y <- y_0
  for (t in 2:(T+1)) {
      y[t] = a*y[t-1] + b
  }
  return(y)
} 


########### 
# Plots a forward simulated difference equation
#     y_0 : starting value
#     a,b : parameters of the difference equation
#     T   : for how many periods to iterate
###########
plot_diff <- function(y_0, a, b, T) {
  
  # simulate equation forward
  y <- diff_eq(y_0, a, b, T)
  
  # generate plotting data
  dat <- data.frame(
    time = seq(0, T, 1),
    DE = y
  )
  
  # now generate the plot
  myplot <- ggplot(data=dat) + 
    geom_line(aes(x=time, y=DE), color= "darkblue", size=1) +
    geom_point(aes(x=time, y=DE), color= "darkblue", size=3, show.legend = FALSE) +
    coord_cartesian(xlim=c(0, T)) + 
    scale_x_continuous(breaks=seq(0, T, 2)) +
    labs(x = "Time t",
         y = "Solution of linear DE",
         title= bquote("Linear Difference Equation: " ~ a == .(a) ~ " , " ~ b == .(b) ~ " , " ~ y[0] == .(y_0))) +
    theme_bw()
  
  # print the plot
  print(myplot)
}


########### 
# Plots a forward simulated difference equation (incl. steady state)
#     y_0 : starting value
#     a,b : parameters of the difference equation
#     T   : for how many periods to iterate
########### 
plot_diff_steady <- function(y_0, a, b, T) {

  # simulate equation forward
  y <- diff_eq(y_0, a, b, T)
  
  # generate plotting data
  dat <- data.frame(
    time = seq(0, T, 1),
    DE = y,
    stst= array(b/(1-a), c(T+1))
  )
  
  # now generate the plot
  myplot <- ggplot(data=dat) + 
    geom_line(aes(x=time, y=stst, color="Steady State"), size=1) +
    geom_line(aes(x=time, y=DE, color="Solution DE"), size=1) +
    geom_point(aes(x=time, y=DE), color="darkblue", size=3, show.legend = FALSE) +
    scale_color_manual(name = "", values = c("Steady State"="red", "Solution DE"="darkblue")) +
    coord_cartesian(xlim=c(0, T)) + 
    scale_x_continuous(breaks=seq(0, T, 2)) +
    labs(x = "Time t",
         y = "Solution of linear DE",
         title= bquote("Linear Difference Equation: " ~ a == .(a) ~ " , " ~ b == .(b) ~ " , " ~ y[0] == .(y_0))) +
    theme_bw()
  
  # print the plot
  print(myplot)
}



########### 
# Generate different plots
########### 

T = 14;

# Values 1
a = 2 ; b = 10 ; y_0 = 0
plot_diff(y_0, a, b, T)

# Values 2
a = -2 ; b = 10 ; y_0 = 0
plot_diff(y_0, a, b, T)

# Values 3
a = 1; b = 10 ; y_0 = 0
plot_diff(y_0, a, b, T)

# Values 4
a = -1; b = 10 ; y_0 = 0
plot_diff(y_0, a, b, T)

# Values 5
a = 2 ; b = 10 ; y_0 = 0
plot_diff_steady(y_0, a, b, T)

# Values 6
a = 2 ; b = 10 ; y_0 = b/(1-a)
plot_diff_steady(y_0, a, b, T)

# Values 7
a = 2 ; b = 10 ; y_0 = b/(1-a)-0.01
plot_diff_steady(y_0, a, b, T)

# Values 8
a = -1; b = 10 ; y_0 = b/(1-a)
plot_diff_steady(y_0, a, b, T)

# Values 9
a = -1; b = 10 ; y_0 = b/(1-a)-1
plot_diff_steady(y_0, a, b, T)

# Values 10
a = 0.75; b = 10 ; y_0 = 500
plot_diff_steady(y_0, a, b, T)

# Values 11
a = 0.50; b = 10 ; y_0 = 0
plot_diff_steady(y_0, a, b, T)

# Values 12
a = 0.25; b = 10 ; y_0 = 0
plot_diff_steady(y_0, a, b, T)

# Values 13
a = -0.75; b = 10 ; y_0 = 0
plot_diff_steady(y_0, a, b, T)

# Values 14
a = -0.50; b = 10 ; y_0 = 0
plot_diff_steady(y_0, a, b, T)

# Values 15
a = -0.25; b = 10 ; y_0 = 0
plot_diff_steady(y_0, a, b, T)
