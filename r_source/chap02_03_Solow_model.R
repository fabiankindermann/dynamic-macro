#######################################
### The Solow Growth Model
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
library(pwt10)

# should graphs be exported to pdf
export_pdf <- FALSE

# define some colors
mygreen <- "#00BA38"
myblue  <- "#619CFF"
myred   <- "#F8766D"


########### 
# Load Penn World Table data
#
# Documentation of Penn World Tables: https://www.rug.nl/ggdc/productivity/pwt/
########### 

# load data and extract US data
data("pwt10.0")
pwt_sub <- subset(pwt10.0, isocode=="USA")


########### 
# Growth in Total Labor Input
###########

# calculate log-total-hours
pwt_sub$total_hours <- log(pwt_sub$emp*pwt_sub$avh)

# run a linear regression
reg <- lm(total_hours ~ year, pwt_sub)
summary(reg)

# for graph dimensions and annotation
xrng <- range(pwt_sub$year)
yrng <- range(pwt_sub$total_hours)
ymin <- (yrng[1]+yrng[2])/2 - 0.018*(xrng[2]-xrng[1])/2
ymax <- (yrng[1]+yrng[2])/2 + 0.018*(xrng[2]-xrng[1])/2
lab  <- paste("growth rate = ", format(round(reg$coefficients[2]*100, 2), nsmall=2), "% / R2 = ", format(round(summary(reg)$r.squared, 2), nsmall=2))

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=total_hours), color="darkblue", size=1) +
  geom_smooth(aes(x=year, y=total_hours), method="lm", formula="y ~ x", se=FALSE, color=myred) +
  geom_label(aes(x = xrng[1], y = ymax, label = lab), 
            hjust = 0, vjust = 1, label.r = unit(0, "lines"), label.padding = unit(0.35, "lines")) +
  coord_cartesian(xlim=c(1950, 2020), ylim=c(ymin, ymax)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  labs(x = "Year t",
       y = "Log(Total Hours)") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig13.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Share of investment (gross capital formation) in GDP
###########

# for annotating
xrng <- range(pwt_sub$year)
yrng <- range(pwt_sub$csh_i)
ymin <- 0.15
ymax <- 0.30
lab  <- paste("Long-run Level = ", format(round(mean(pwt_sub$csh_i), 3), nsmall=3))

myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=csh_i), color="darkblue", size=1) +
  geom_smooth(aes(x=year, y=csh_i), method="lm", formula="y ~ 1", se=FALSE, color=myred) +
  geom_label(aes(x = xrng[2], y = ymax, label = lab),
             hjust = 1, vjust = 1, label.r = unit(0, "lines"), label.padding = unit(0.35, "lines")) +
  coord_cartesian(xlim=c(1950, 2020), ylim=c(ymin, ymax)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  labs(x = "Year t",
       y = "Gross Capital Formation / GDP") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig14.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Solow Model setup
###########

# Parameter choice
n     <- 0.0122
h     <- 0.0169
delta <- 0.044
s     <- 0.245
alpha <- 0.381

# initial labor input
reg <- lm(total_hours ~ year, pwt_sub)
L_1950 <- exp(reg$coefficients[1] + reg$coefficients[2]*1950)

# initial level of GDP
pwt_sub$logGDP <- log(pwt_sub$rgdpe)
reg <- lm(logGDP ~ year, pwt_sub)
Y_1950 <- exp(reg$coefficients[1] + reg$coefficients[2]*1950)

# steady state capital intensity 
g <- (1+n)*(1+h)-1
k_star <- (s/(g+delta))^(1/(1-alpha))

# initial technology level
A_1950 <- Y_1950/(L_1950*k_star^alpha)

# initial capital level
K_1950 <- k_star*A_1950*L_1950


########### 
# Solow model performance
###########

pwt_sub$solow_Y <- Y_1950*(1+g)^(pwt_sub$year-1950)
pwt_sub$solow_K <- K_1950*(1+g)^(pwt_sub$year-1950)
pwt_sub$solow_L <- L_1950*(1+n)^(pwt_sub$year-1950)


########### 
# Plot aggregate GDP (data vs. model)
###########

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=rgdpe), color="darkblue", size=1) +
  geom_line(aes(x=year, y=solow_Y), color="#00BA38", size=1) +
  coord_cartesian(xlim=c(1950, 2020)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  scale_y_continuous(labels = unit_format(unit = "T", scale = 1e-6)) +
  labs(x = "Year t",
       y = "Real GDP (in constant 2017 US$)") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig15.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Plot aggregate labor input
###########

pwt_sub$total_hours <- pwt_sub$emp*pwt_sub$avh

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=total_hours), color="darkblue", size=1) +
  geom_line(aes(x=year, y=solow_L), color="#00BA38", size=1) +
  coord_cartesian(xlim=c(1950, 2020)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-3)) +
  labs(x = "Year t",
       y = "Total hours worked") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig16.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Plot aggregate capital stock
###########

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=rnna), color="darkblue", size=1) +
  geom_line(aes(x=year, y=solow_K), color="#00BA38", size=1) +
  coord_cartesian(xlim=c(1950, 2020)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  scale_y_continuous(labels = unit_format(unit = "T", scale = 1e-6)) +
  labs(x = "Year t",
       y = "Capital Stock (constant 2017 US$)") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig17.pdf", width = height*aspect_ratio, height = height)
}



########### 
# Golden Rule level of capital
###########

k_max <- (alpha/(g+delta))^(1/(1-alpha))
s_max <- (g+delta)*k_max^(1-alpha)

sprintf("Current capital intensity     : k^*           = %5.3f", k_star)
sprintf("Current net return to capital : f'(k^*)-delta = %5.3f", alpha*k_star^(alpha-1)-delta)
sprintf("Growth in labor input         : g             = %5.3f", g)
sprintf("Golden Rule level of capital  : k_max         = %5.3f", k_max)
sprintf("Consumption-max savings rate  : s_max         = %5.3f", s_max)


########### 
# Simulate transition path towards Golden Rule growth
###########

# number of transition periods to simulate
T0 <- -50
T1 <- 200

ind <- function(t) {
  return(t + 1 + abs(T0))
}

# start at old k_star
k <- k_star
k[ind(T0):ind(0)] <- k_star

# iterate using new savings rate
for (t in ind(0):ind(T1-1)) {
  k[t+1] <- (s_max*k[t]^alpha + (1-delta)*k[t])/(1+g)
}

# calculate production and consumption along the way
y <- k^alpha
c <- 0
c[ind(T0):ind(0)] <- (1-s)*y[ind(T0):ind(0)]
c[ind(1):ind(T1)] <- (1-s_max)*y[ind(1):ind(T1)]
ir <- (g + delta)*k
i <- y - c

transition <- data.frame(year=c(T0:T1), k, y, c, i, ir)


########### 
# Plot dynamics of the capital stock
###########

myplot <- ggplot(data = transition) + 
  geom_hline(yintercept = transition$k[ind(T0)], color=myred, linetype="dashed", size=1) + 
  geom_hline(yintercept = transition$k[ind(T1)], color="#00BA38", linetype="dashed", size=1) + 
  geom_line(aes(x=year, y=k), color="darkblue", size=1) +
  coord_cartesian(xlim=c(T0, T1), ylim=c(6, 16)) + 
  scale_x_continuous(breaks=seq(T0, T1, 25), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "Capital Intensity") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig21.pdf", width = height*aspect_ratio, height = height)
}

########### 
# Plot GDP and its components
###########

myplot <- ggplot(data = transition) + 
  geom_hline(yintercept = transition$c[1], color="#00BA38", linetype="dashed", size=0.5) + 
  geom_ribbon(aes(x=year, ymin=0, ymax=c,    fill= "1c", color="1c") , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c, ymax=c+i-ir, fill= "3di", color="3di"), alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c+i-ir, ymax=y, fill= "2ir", color="2ir")  , alpha=0.4) +
  geom_line(aes(x=year, y=y), color="darkblue", size=1) +
  coord_cartesian(xlim=c(T0, T1), ylim=c(0, 3)) + 
  scale_x_continuous(breaks=seq(T0, T1, 25), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "GDP and its components per\n effective unit of labor") +
  scale_fill_manual(breaks = c("1c", "2ir", "3di"), name = "", 
                    labels = c("Consumption", "Replacement Investment", "Capital Augmenting Inv."),
                    values = c(mygreen, myblue, myred)) +
  scale_color_manual(breaks = c("1c", "2ir", "3di"),
                     values = c(mygreen, myblue, myred)) +
  guides(colour = "none") +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig22.pdf", width = height*aspect_ratio, height = height)
}


########### 
# On the other side of the Golden rule
###########

# some other side of the Golden rule savings rate
s = 0.5
k_star <- (s/(g+delta))^(1/(1-alpha))

# number of transition periods to simulate
T0 <- -50
T1 <- 200

ind <- function(t) {
  return(t + 1 + abs(T0))
}

# start at old k_star
k <- 0
k[ind(T0):ind(0)] <- k_star

# iterate using new savings rate
for (t in ind(0):ind(T1-1)) {
  k[t+1] <- (s_max*k[t]^alpha + (1-delta)*k[t])/(1+g)
}

# calculate production and consumption along the way
y <- k^alpha
c <- 0
c[ind(T0):ind(0)] <- (1-s)*y[ind(T0):ind(0)]
c[ind(1):ind(T1)] <- (1-s_max)*y[ind(1):ind(T1)]
ir <- (g + delta)*k
i <- y - c

transition <- data.frame(year=c(T0:T1), k, y, c, i, ir)


########### 
# Plot dynamics of the capital stock
###########

myplot <- ggplot(data = transition) + 
  geom_hline(yintercept = transition$k[ind(T0)], color=myred, linetype="dashed", size=1) + 
  geom_hline(yintercept = transition$k[ind(T1)], color="#00BA38", linetype="dashed", size=1) + 
  geom_line(aes(x=year, y=k), color="darkblue", size=1) +
  coord_cartesian(xlim=c(T0, T1), ylim=c(12, 24)) + 
  scale_y_continuous(breaks=seq(12, 24, 2)) +
  scale_x_continuous(breaks=seq(T0, T1, 25), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "Capital Intensity") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig23.pdf", width = height*aspect_ratio, height = height)
}

########### 
# Plot GDP and its components
###########

myplot <- ggplot(data = transition) + 
  geom_hline(yintercept = transition$c[1], color="#00BA38", linetype="dashed", size=0.5) + 
  geom_ribbon(aes(x=year, ymin=0, ymax=c,    fill= "1c", color="1c") , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c, ymax=c+ir, fill= "2ir", color="2ir")  , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c+ir, ymax=y, fill= "3di", color="3di"), alpha=0.4) +
  geom_line(aes(x=year, y=y), color="darkblue", size=1) +
  coord_cartesian(xlim=c(T0, T1), ylim=c(0, 4)) + 
  scale_x_continuous(breaks=seq(T0, T1, 25), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "GDP and its components per\n effective unit of labor") +
  scale_fill_manual(breaks = c("1c", "2ir", "3di"), name = "", 
                    labels = c("Consumption", "Replacement Investment", "Capital Augmenting Inv."),
                    values = c(mygreen, myblue, myred)) +
  scale_color_manual(breaks = c("1c", "2ir", "3di"),
                     values = c(mygreen, myblue, myred)) +
  guides(colour = "none") +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig24.pdf", width = height*aspect_ratio, height = height)
}