#######################################
### Non-Linear Difference Equations
#######################################

# Clear the workspace and load libraries
rm(list = ls())
library(reshape2)
library(base)
library(ggplot2)
library(grid)
library(stringr)
library(tidyverse)
library(gridExtra) 


########### 
# Iterates forward the non-difference equation with f(y) = y^alpha
#     y_0   : starting value
#     alpha : curvature parameter
#     T     : for how many periods to iterate
########### 
diff_eq <- function(y_0, alpha, T) {
  y <- y_0
  if(T > 0) {
    for (t in 2:(T+1)) {
      y[t] = y[t-1]^alpha
    }
  }
  return(y)
} 


########### 
# Generates a phase diagram with some starting value for T periods
#     y_0   : starting value
#     alpha : parameter of the difference equation
#     T     : for how many periods to iterate
###########
plot_phase <- function(y_0, alpha, T) {
  
  # simulate equation forward
  y <- diff_eq(y_0, alpha, T)
  
  # First plot the function itself
  dat <- data.frame(
    x = seq(0.0, 2, by=0.01),
    f = seq(0.0, 2, by=0.01)^alpha
  )
  
  myplot <- ggplot(data=dat) + 
    geom_line(aes(x=x, y=f), color="darkblue", size=1) +
    geom_line(aes(x=x, y=x), color="darkblue", size=0.5) +
    coord_cartesian(xlim=c(0, 2), ylim=c(0, 2)) + 
    scale_x_continuous(breaks=seq(0, 2, 0.2)) +
    labs(x = expression('y'['t']),
         y = expression('y'['t+1']),
         title= bquote("Phase Diagram: " ~ alpha == .(alpha) ~ " , " ~ y[0] == .(y_0))) +
    theme_bw()
  
  # add single steps to the phase diagram
  if(T >= 1) {
    for (t in 1:T) {
      
      # add the vertical line
      dat <- data.frame(d1 = c(y[t], y[t]),
                        d2 = c(y[t], y[t+1]))
      myplot <- myplot + geom_path(data=dat, aes(x=d1, y=d2), color = "red", size=0.5, arrow = arrow(length = unit(0.02, "npc")))
      
      # add the horizontal line
      dat <- data.frame(d1 = c(y[t], y[t+1]),
                        d2 = c(y[t+1], y[t+1]))
      myplot <- myplot + geom_path(data=dat, aes(x=d1, y=d2), color = "red", size=0.5, arrow = arrow(length = unit(0.02, "npc")))
    }
  }
  
  # print the plot
  return(myplot)
}


########### 
# Plots a forward simulated difference equation
#     y_0   : starting value
#     alpha : parameter of the difference equation
#     T     : for how many periods to iterate
########### 
plot_diff <- function(y_0, alpha, T) {
  
  # simulate equation forward
  y <- diff_eq(y_0, alpha, T)
  
  # generate plotting data
  dat <- data.frame(
    time = seq(0, T, 1),
    DE = y,
    stst = array(1, c(T+1))
  )
  
  aval <- sprintf("%5.2f", alpha)
  yval <- sprintf("%5.2f", y_0)
  
  # now generate the plot
  myplot <- ggplot(data=dat) + 
    geom_line(aes(x=time, y=stst), color="red", size=1) +
    geom_line(aes(x=time, y=DE), color="darkblue", size=1) +
    geom_point(aes(x=time, y=DE), color="darkblue", size=3, show.legend = FALSE) +
    coord_cartesian(xlim=c(0, T), ylim=c(0,2)) + 
    scale_x_continuous(breaks=seq(0, T, 2)) +
    labs(x = "Time t",
         y = expression(paste("Solution of non-linear DE ", y[t])),
         title= bquote("Time  diagram: " ~ alpha == .(alpha) ~ " , " ~ y[0] == .(y_0))) +
    theme_bw()
  
  # print the plot
  return(myplot)
}


########### 
# Plot Phase and Time Diagram into one graph
#     y_0   : starting value
#     alpha : parameter of the difference equation
#     T     : for how many periods to iterate
###########
plot_both <- function(y_0, alpha, T) {
  
  #  phase diagram
  phase_diagram <- plot_phase(y_0, alpha, T)
  
  # time diagram
  time_diagram <- plot_diff(y_0, alpha, T)
  
  # plot one next to the other
  grid.arrange(phase_diagram, time_diagram, ncol=2)
}



########### 
# Generate different plots
########### 

T = 14;

# Monotone convergence from left
plot_both(0.25, 0.5, T)

# Monotone convergence from right
plot_both(1.75, 0.5, T)

# Alternating convergence
plot_both(0.25, -0.5, T)

# Monotone convergence to other steady state
plot_both(0.9, 1.5, T)

# Monotone divergence
plot_both(1.01, 1.5, T)

# Alternating divergence
plot_both(1.01, -1.5, T)
