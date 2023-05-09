#######################################
### AR(1) processes
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

# should graphs be exported to pdf
export_pdf <- FALSE

# define some colors
mygreen <- "#00BA38"
myblue  <- "#619CFF"
myred   <- "#F8766D"
  
# standard deviation of epsilon
sd_eps = 0.1


########### 
# Impulse response functions for different AR(1) coefficients
###########

# generate sequence of 20 quarters
x <- seq(1, 20, 1)
dat <- data.frame(quarter=x)

# iterate over different values
i <- 1
for(rho in c(-0.9, -0.5, 0, 0.5, 0.9)) {
  dat[paste("rho", i, sep="")] <- sd_eps*rho^c(0:(length(x)-1))
  i <- i+1
}

# create plot for positive rhos
myplot <- ggplot(data = dat) + 
  geom_line(aes(x=quarter, y=rho3, color="l1"), linewidth=1) +
  geom_line(aes(x=quarter, y=rho4, color="l2"), linewidth=1) +
  geom_line(aes(x=quarter, y=rho5, color="l3"), linewidth=1) +
  scale_x_continuous(breaks=seq(0, 20, 4), expand=c(0, 0)) +  
  scale_y_continuous(breaks=seq(0, 0.1, 0.025)) +  
  labs(x = "Quarter t",
       y = expression(hat(x)[t])) +
  scale_color_manual(breaks = c("l1", "l2", "l3"),
                     labels = c(expression(paste(rho," = 0.0")), expression(paste(rho," = 0.5")), expression(paste(rho," = 0.9"))),
                     values = c(mygreen, myblue, myred), name='') +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig1.pdf", width = height*aspect_ratio, height = height)
}


# create plot for negative rhos
myplot <- ggplot(data = dat) + 
  geom_line(aes(x=quarter, y=rho2, color="l1"), linewidth=1) +
  geom_line(aes(x=quarter, y=rho1, color="l2"), linewidth=1) +
  scale_x_continuous(breaks=seq(0, 20, 4), expand=c(0, 0)) +  
  scale_y_continuous(breaks=seq(-0.1, 0.1, 0.025)) +  
  labs(x = "Quarter t",
       y = expression(hat(x)[t])) +
  scale_color_manual(breaks = c("l1", "l2"),
                     labels = c(expression(paste(rho," = -0.5")), expression(paste(rho," = -0.9"))),
                     values = c(mygreen, myblue), name='') +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig2.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Distribution for log(X_t) for different values of rho
###########

x = seq(-1, 1, 0.01)
dat <- data.frame(x)

# iterate over different values
i <- 1
for(rho in c(0, 0.5, 0.9)) {
  dat[paste("rho", i, sep="")] <- dnorm(x, mean=0, sd=sd_eps/sqrt(1-rho^2))
  i <- i+1
}

# create plot for different values of rho
myplot <- ggplot(data = dat) + 
  geom_line(aes(x=x, y=rho1, color="l1"), linewidth=1) +
  geom_line(aes(x=x, y=rho2, color="l2"), linewidth=1) +
  geom_line(aes(x=x, y=rho3, color="l3"), linewidth=1) +
  scale_x_continuous(breaks=seq(-1, 1, 0.25), expand=c(0, 0)) +  
  labs(x = expression(log(x[t])),
       y = "Density") +
  scale_color_manual(breaks = c("l1", "l2", "l3"),
                     labels = c(expression(paste(rho," = 0.0")), expression(paste(rho," = 0.5")), expression(paste(rho," = 0.9"))),
                     values = c(mygreen, myblue, myred), name='') +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig3.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Distribution for X_t for different values of rho
###########

# create plot for different values of rho
myplot <- ggplot(data = dat) + 
  geom_line(aes(x=exp(x), y=rho1, color="l1"), linewidth=1) +
  geom_line(aes(x=exp(x), y=rho2, color="l2"), linewidth=1) +
  geom_line(aes(x=exp(x), y=rho3, color="l3"), linewidth=1) +
  scale_x_continuous(breaks=seq(0, 3, 0.5), expand=c(0, 0)) +  
  labs(x = expression(x[t]),
       y = "Density") +
  scale_color_manual(breaks = c("l1", "l2", "l3"),
                     labels = c(expression(paste(rho," = 0.0")), expression(paste(rho," = 0.5")), expression(paste(rho," = 0.9"))),
                     values = c(mygreen, myblue, myred), name='') +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig4.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Distribution for log(X_t) for different values of X_bar
###########

x = seq(-1, 1.5, 0.01)
dat <- data.frame(x)

# iterate over different values
i <- 1
for(X_bar in c(0, 0.25, 0.5)) {
  y <- c(0:(length(x)-1))
  dat[paste("rho", i, sep="")] <- dnorm(x, mean=X_bar, sd=sd_eps/sqrt(1-rho^2))
  i <- i+1
}

# create plot for different values of rho
myplot <- ggplot(data = dat) + 
  geom_line(aes(x=x, y=rho1, color="l1"), linewidth=1) +
  geom_line(aes(x=x, y=rho2, color="l2"), linewidth=1) +
  geom_line(aes(x=x, y=rho3, color="l3"), linewidth=1) +
  scale_x_continuous(breaks=seq(-1, 1.5, 0.25), expand=c(0, 0)) +  
  labs(x = expression(log(x[t])),
       y = "Density") +
  scale_color_manual(breaks = c("l1", "l2", "l3"),
                     labels = c(expression(paste(bar(X)," = 0.00")), expression(paste(bar(X)," = 0.25")), expression(paste(bar(X)," = 0.50"))),
                     values = c(mygreen, myblue, myred), name='') +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig5.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Distribution for X_t for different values of rho
###########

# create plot for different values of rho
myplot <- ggplot(data = dat) + 
  geom_line(aes(x=exp(x), y=rho1, color="l1"), linewidth=1) +
  geom_line(aes(x=exp(x), y=rho2, color="l2"), linewidth=1) +
  geom_line(aes(x=exp(x), y=rho3, color="l3"), linewidth=1) +
  scale_x_continuous(breaks=seq(0, 4.5, 0.5), expand=c(0, 0)) +  
  
  labs(x = expression(x[t]),
       y = "Density") +
  scale_color_manual(breaks = c("l1", "l2", "l3"),
                     labels = c(expression(paste(bar(X)," = 0.00")), expression(paste(bar(X)," = 0.25")), expression(paste(bar(X)," = 0.50"))),
                     values = c(mygreen, myblue, myred), name='') +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig6.pdf", width = height*aspect_ratio, height = height)
}