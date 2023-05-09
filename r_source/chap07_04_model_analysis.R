#######################################
### Model Analysis
#######################################

# Clear the workspace and load libraries
rm(list = ls())
library(reshape2)
library(base)
library(ggplot2)
library(grid)
library(scales)
library(stringr)
library(tidyverse)
library(formattable)

# should graphs be exported to pdf
export_pdf <- FALSE

# define some colors
mygreen <- "#00BA38"
myblue  <- "#619CFF"
myred   <- "#F8766D"


########### 
# Load parameters, log-linearization coefficients and empirical data
###########

# get parameters stored in the RData set
load("chap07_parameters.RData")

# get coefficients stored in the RData set
load("chap07_coefficients.RData")

# get empirical data moments
load("chap07_empirics.RData")

########### 
# Impulse responses of a 1 percent productivity shock
###########

# number of quarters
n_quarter <- 80

# initial shock at date t = 0 (in percent)
shock <- matrix(c(0, 1, 0))

# initialize vectors x_t and y_t
x <- matrix(nrow=3, ncol=n_quarter+1)
y <- matrix(nrow=6, ncol=n_quarter+1)

# calculate x_1 and y_1
x[, 1] <- shock
y[, 1] <- B %*% x[, 1]

# iterate forward x_t and y_t
for(i in c(2:(n_quarter+1))) {
  x[, i] <- A %*% x[, i-1]
  y[, i] <- B %*% x[, i]
}

# generate dataframe from results
impulse <- data.frame(quarter=c(0:n_quarter), K=x[1, ], A=x[2, ], G=x[3, ], Y=y[1, ], C=y[2, ], I=y[3, ], r=y[4, ], w=y[5, ], L=y[6, ])


# Plot impulse response of production sector
myplot <- ggplot(data = impulse) + 
  geom_hline(yintercept=0, color="gray", linewidth=0.5) +
  geom_line(aes(x=quarter, y=K, color="capital"), linewidth=1) +
  geom_line(aes(x=quarter, y=A, color="tech"), linewidth=1) +
  geom_line(aes(x=quarter, y=L, color="labor"), linewidth=1) +
  labs(x = "Quarter t",
       y = "Deviation from Steady State (in %)") +
  scale_color_manual(breaks = c("capital", "tech", "labor"),
                     labels = c("Capital K", "Technology A", "Labor Input L"),
                     values = c(mygreen, myblue, myred), name='') +
  coord_cartesian(ylim=c(-0.25, 1)) +
  scale_y_continuous(breaks=seq(-0.25, 1, 0.25)) +  
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig9.pdf", width = height*aspect_ratio, height = height)
}


# Plot impulse response of final goods
myplot <- ggplot(data = impulse) + 
  geom_hline(yintercept=0, color="gray", linewidth=0.5) +
  geom_line(aes(x=quarter, y=C, color="cons"), linewidth=1) +
  geom_line(aes(x=quarter, y=I, color="inv"), linewidth=1) +
  geom_line(aes(x=quarter, y=G, color="gov"), linewidth=1) +
  labs(x = "Quarter t",
       y = "Deviation from Steady State (in %)") +
  scale_color_manual(breaks = c("cons", "inv", "gov"),
                     labels = c("Consumption C", "Investment I", "Government Exp. G"),
                     values = c(mygreen, myblue, myred), name='') +
  coord_cartesian(ylim=c(-1, 5)) +
  scale_y_continuous(breaks=seq(-1, 5, 1)) +  
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig10.pdf", width = height*aspect_ratio, height = height)
}


# Plot impulse response of prices
myplot <- ggplot(data = impulse) + 
  geom_hline(yintercept=0, color="gray", linewidth=0.5) +
  geom_line(aes(x=quarter, y=r, color="r"), linewidth=1) +
  geom_line(aes(x=quarter, y=w, color="w"), linewidth=1) +
  labs(x = "Quarter t",
       y = "Deviation from Steady State (in %)") +
  scale_color_manual(breaks = c("r", "w"),
                     labels = c("Interest rate r", "Wage rate w"),
                     values = c(mygreen, myblue, myred), name='') +
  coord_cartesian(ylim=c(-1, 3)) +
  scale_y_continuous(breaks=seq(-1, 3, 1)) +  
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig11.pdf", width = height*aspect_ratio, height = height)
}




########### 
# Impulse responses of a 1 percent government expenditure shock
###########

# number of quarters
n_quarter <- 80

# initial shock at date t = 0 (in percent)
shock <- matrix(c(0, 0, 1))

# initialize vectors x_t and y_t
x <- matrix(nrow=3, ncol=n_quarter+1)
y <- matrix(nrow=6, ncol=n_quarter+1)

# calculate x_1 and y_1
x[, 1] <- shock
y[, 1] <- B %*% x[, 1]

# iterate forward x_t and y_t
for(i in c(2:(n_quarter+1))) {
  x[, i] <- A %*% x[, i-1]
  y[, i] <- B %*% x[, i]
}

# generate dataframe from results
impulse <- data.frame(quarter=c(0:n_quarter), K=x[1, ], A=x[2, ], G=x[3, ], Y=y[1, ], C=y[2, ], I=y[3, ], r=y[4, ], w=y[5, ], L=y[6, ])


# Plot impulse response of production sector
myplot <- ggplot(data = impulse) + 
  geom_hline(yintercept=0, color="gray", linewidth=0.5) +
  geom_line(aes(x=quarter, y=K, color="capital"), linewidth=1) +
  geom_line(aes(x=quarter, y=A, color="tech"), linewidth=1) +
  geom_line(aes(x=quarter, y=L, color="labor"), linewidth=1) +
  labs(x = "Quarter t",
       y = "Deviation from Steady State (in %)") +
  scale_color_manual(breaks = c("capital", "tech", "labor"),
                     labels = c("Capital K", "Technology A", "Labor Input L"),
                     values = c(mygreen, myblue, myred), name='') +
  coord_cartesian(ylim=c(-0.05, 0.1)) +
  scale_y_continuous(breaks=seq(-0.05, 0.1, 0.05)) +  
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig12.pdf", width = height*aspect_ratio, height = height)
}


# Plot impulse response of final goods
myplot <- ggplot(data = impulse) + 
  geom_hline(yintercept=0, color="gray", linewidth=0.5) +
  geom_line(aes(x=quarter, y=C, color="cons"), linewidth=1) +
  geom_line(aes(x=quarter, y=I, color="inv"), linewidth=1) +
  geom_line(aes(x=quarter, y=G, color="gov"), linewidth=1) +
  labs(x = "Quarter t",
       y = "Deviation from Steady State (in %)") +
  scale_color_manual(breaks = c("cons", "inv", "gov"),
                     labels = c("Consumption C", "Investment I", "Government Exp. G"),
                     values = c(mygreen, myblue, myred), name='') +
  coord_cartesian(ylim=c(-1, 1)) +
  scale_y_continuous(breaks=seq(-1, 1, 0.5)) +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig13.pdf", width = height*aspect_ratio, height = height)
}


# Plot impulse response of prices
myplot <- ggplot(data = impulse) + 
  geom_hline(yintercept=0, color="gray", linewidth=0.5) +
  geom_line(aes(x=quarter, y=r, color="r"), linewidth=1) +
  geom_line(aes(x=quarter, y=w, color="w"), linewidth=1) +
  labs(x = "Quarter t",
       y = "Deviation from Steady State (in %)") +
  scale_color_manual(breaks = c("r", "w"),
                     labels = c("Interest rate r", "Wage rate w"),
                     values = c(mygreen, myblue, myred), name='') +
  coord_cartesian(ylim=c(-0.05, 0.2)) +
  scale_y_continuous(breaks=seq(-0.05, 0.2, 0.05)) +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig14.pdf", width = height*aspect_ratio, height = height)
}



########### 
# Hansen and Wright (1992) simulation methodology
###########

# set the random seed to generate the same sequence of random variables
set.seed(351224332)

# number of simulations
n_sim <- 100

# number of quarters to simulate
n_quarter <- 296

# set standard deviation of a (calibration target is sd(Y) = 1.93%)
sigma_A = 0.0063

# initializes relevant statistics elements
hansen_data <- array(0, dim=c(6, 2))

# iterate over all simulated paths
for(k in c(1:n_sim)) {
  
  # draw a series of N(0,1) iid shocks
  a_shocks <- rnorm(n_quarter)
  g_shocks <- rnorm(n_quarter)
  
  # initialize vectors x_t and y_t
  x <- matrix(nrow=3, ncol=n_quarter)
  y <- matrix(nrow=6, ncol=n_quarter)
  
  # calculate x_1 and y_1
  x[, 1] <- matrix(c(0, sigma_A*a_shocks[1], sigma_G*g_shocks[1]))
  y[, 1] <- B %*% x[, 1]
  
  # iterate forward x_t and y_t
  for(i in c(2:(n_quarter))) {
    x[, i] <- A %*% x[, i-1] + matrix(c(0, sigma_A*a_shocks[i], sigma_G*g_shocks[i]))
    y[, i] <- B %*% x[, i]
  }
  
  # update data
  hansen_data[1, 1] <- hansen_data[1, 1] + sd(y[1, ])/n_sim
  hansen_data[2, 1] <- hansen_data[2, 1] + sd(y[2, ])/sd(y[1, ])/n_sim
  hansen_data[3, 1] <- hansen_data[3, 1] + sd(y[3, ])/sd(y[1, ])/n_sim
  hansen_data[4, 1] <- hansen_data[4, 1] + sd(y[5, ])/sd(y[1, ])/n_sim
  hansen_data[5, 1] <- hansen_data[5, 1] + sd(y[6, ])/sd(y[1, ])/n_sim
  hansen_data[6, 1] <- hansen_data[6, 1] + sd(y[6, ])/sd(y[5, ])/n_sim
  
  hansen_data[1, 2] <- 1
  hansen_data[2, 2] <- hansen_data[2, 2] + cor(y[2, ], y[1, ])/n_sim
  hansen_data[3, 2] <- hansen_data[3, 2] + cor(y[3, ], y[1, ])/n_sim
  hansen_data[4, 2] <- hansen_data[4, 2] + cor(y[5, ], y[1, ])/n_sim
  hansen_data[5, 2] <- hansen_data[5, 2] + cor(y[6, ], y[1, ])/n_sim
  hansen_data[6, 2] <- hansen_data[6, 2] + cor(y[6, ], y[5, ])/n_sim
}


########### 
# Simulation with (almost) irrelevant initial conditions
###########

# set the random seed to generate the same sequence of random variables
set.seed(351224332)

# number of quarters to simulate
n_quarter <- 100000

# set standard deviation of a (calibration target is sd(Y) = 1.93%)
sigma_A = 0.00565

# initializes relevant statistics elements
sim_data <- array(0, dim=c(6, 2))
  
# draw a series of N(0,1) iid shocks
a_shocks <- rnorm(n_quarter)
g_shocks <- rnorm(n_quarter)

# initialize vectors x_t and y_t
x <- matrix(nrow=3, ncol=n_quarter)
y <- matrix(nrow=6, ncol=n_quarter)

# calculate x_1 and y_1
x[, 1] <- matrix(c(0, sigma_A*a_shocks[1], sigma_G*g_shocks[1]))
y[, 1] <- B %*% x[, 1]

# iterate forward x_t and y_t
for(i in c(2:(n_quarter))) {
  x[, i] <- A %*% x[, i-1] + matrix(c(0, sigma_A*a_shocks[i], sigma_G*g_shocks[i]))
  y[, i] <- B %*% x[, i]
}

# update data
sim_data[1, 1] <- sd(y[1, ])
sim_data[2, 1] <- sd(y[2, ])/sd(y[1, ])
sim_data[3, 1] <- sd(y[3, ])/sd(y[1, ])
sim_data[4, 1] <- sd(y[5, ])/sd(y[1, ])
sim_data[5, 1] <- sd(y[6, ])/sd(y[1, ])
sim_data[6, 1] <- sd(y[6, ])/sd(y[5, ])

sim_data[1, 2] <- 1
sim_data[2, 2] <- cor(y[2, ], y[1, ])
sim_data[3, 2] <- cor(y[3, ], y[1, ])
sim_data[4, 2] <- cor(y[5, ], y[1, ])
sim_data[5, 2] <- cor(y[6, ], y[1, ])
sim_data[6, 2] <- cor(y[6, ], y[5, ])
  
# generate data table
table_data <- data.frame("Statistics" = 
                           c("sd(Y) in %", 
                             "sd(C)/sd(Y)", "corr(C, Y)",
                             "sd(I)/sd(Y)", "corr(I, Y)",
                             "sd(w)/sd(Y)", "corr(w, Y)",
                             "sd(L)/sd(Y)", "corr(L, Y)",
                             "sd(L)/sd(w)", "corr(L, w)"))
table_data <- cbind(table_data, "Empirical Moments" = 
                      c(emp_data[1, 1]*100, emp_data[2, 1], emp_data[2, 2], emp_data[3, 1], 
                        emp_data[3, 2], emp_data[4, 1], emp_data[4, 2], emp_data[5, 1], 
                        emp_data[5, 2], emp_data[6, 1], emp_data[6, 2]))
table_data <- cbind(table_data, "Hansen and Wright (1992)" = 
                      c(hansen_data[1, 1]*100, hansen_data[2, 1], hansen_data[2, 2], hansen_data[3, 1], 
                        hansen_data[3, 2], hansen_data[4, 1], hansen_data[4, 2], hansen_data[5, 1], 
                        hansen_data[5, 2], hansen_data[6, 1], hansen_data[6, 2]))
table_data <- cbind(table_data, "No Initial Conditions" = 
                      c(sim_data[1, 1]*100, sim_data[2, 1], sim_data[2, 2], sim_data[3, 1], 
                        sim_data[3, 2], sim_data[4, 1], sim_data[4, 2], sim_data[5, 1], 
                        sim_data[5, 2], sim_data[6, 1], sim_data[6, 2]))
  
# output table
formattable(table_data, digits = 2,
            align= c("l", "r", "r", "r"))

