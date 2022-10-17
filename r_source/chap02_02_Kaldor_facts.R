#######################################
### The Kaldor Growth Facts
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
export_pdf = FALSE


########### 
# Load Penn World Table data and inspect
#
# Documentation of Penn World Tables: https://www.rug.nl/ggdc/productivity/pwt/
########### 

# load data and extract US data
data("pwt10.0")
pwt_sub = subset(pwt10.0, isocode=="USA")


########### 
# Kaldor Fact 1: Output per worker grows at constant rate
###########

# calculate log-Output-per-Worker
pwt_sub$output_per_worker = log(pwt_sub$rgdpe/pwt_sub$emp)

# run a linear regression
reg <- lm(output_per_worker ~ year, pwt_sub)
summary(reg)

# for graph dimensions and annotation
xrng <- range(pwt_sub$year)
yrng <- range(pwt_sub$output_per_worker)
ymin <- (yrng[1]+yrng[2])/2 - 0.018*(xrng[2]-xrng[1])/2
ymax <- (yrng[1]+yrng[2])/2 + 0.018*(xrng[2]-xrng[1])/2
lab  <- paste("growth rate = ", format(round(reg$coefficients[2]*100, 2), nsmall=2), "% / R2 = ", format(round(summary(reg)$r.squared, 2), nsmall=2))

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=output_per_worker), color= "darkblue", size=1) +
  geom_smooth(aes(x=year, y=output_per_worker), method="lm", formula="y ~ x", se=FALSE, color= "darkred") +
  geom_label(aes(x = xrng[1], y = ymax, label = lab), 
            hjust = 0, vjust = 1, label.r = unit(0, "lines"), label.padding = unit(0.35, "lines")) +
  coord_cartesian(xlim=c(1950, 2020), ylim=c(ymin, ymax)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  labs(x = "Year t",
       y = "Log(Output/Worker)") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig7.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Kaldor Fact 2: Capital per worker grows over time
###########

# calculate capital per worker
pwt_sub$capital_per_worker = log(pwt_sub$rnna/pwt_sub$emp)

# run a linear regression
reg <- lm(capital_per_worker ~ year, pwt_sub)
summary(reg)

# for graph dimensions
xrng <- range(pwt_sub$year)
yrng <- range(pwt_sub$capital_per_worker)
ymin <- (yrng[1]+yrng[2])/2 - 0.018*(xrng[2]-xrng[1])/2
ymax <- (yrng[1]+yrng[2])/2 + 0.018*(xrng[2]-xrng[1])/2
lab  <- paste("growth rate = ", format(round(reg$coefficients[2]*100, 2), nsmall=2), "% / R2 = ", format(round(summary(reg)$r.squared, 2), nsmall=2))

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=capital_per_worker), color= "darkblue", size=1) +
  geom_smooth(aes(x=year, y=capital_per_worker), method="lm", formula="y ~ x", se=FALSE, color= "darkred") +
  geom_label(aes(x = xrng[1], y = ymax, label = lab), 
             hjust = 0, vjust = 1, label.r = unit(0, "lines"), label.padding = unit(0.35, "lines")) +
  coord_cartesian(xlim=c(1950, 2020), ylim=c(ymin, ymax)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  labs(x = "Year t",
       y = "Log(Capital/Worker)") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig8.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Kaldor Fact 3: Capital-to-output ratio is roughly constant
###########

# calculate capital-to-output ratio
pwt_sub$capital_to_output = log(pwt_sub$rnna/pwt_sub$rgdpna)

# run a linear regression
reg <- lm(capital_to_output ~ year, pwt_sub)
summary(reg)

# for graph dimensions
xrng <- range(pwt_sub$year)
yrng <- range(pwt_sub$capital_to_output)
ymin <- (yrng[1]+yrng[2])/2 - 0.018*(xrng[2]-xrng[1])/2
ymax <- (yrng[1]+yrng[2])/2 + 0.018*(xrng[2]-xrng[1])/2
lab  <- paste("growth rate = ", format(round(reg$coefficients[2]*100, 2), nsmall=2), "% / R2 = ", format(round(summary(reg)$r.squared, 2), nsmall=2))

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=capital_to_output), color= "darkblue", size=1) +
  geom_smooth(aes(x=year, y=capital_to_output), method="lm", formula="y ~ x", se=FALSE, color= "darkred") +
  geom_label(aes(x = xrng[1], y = ymax, label = lab), 
             hjust = 0, vjust = 1, label.r = unit(0, "lines"), label.padding = unit(0.35, "lines")) +
  coord_cartesian(xlim=c(1950, 2020), ylim=c(ymin, ymax)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  labs(x = "Year t",
       y = "Log(Capital/Output)") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig9.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Kaldor Fact 4: Gross rate of return on capital
###########

# calculate gross rate of return on capital
pwt_sub$gross_return = pwt_sub$irr + pwt_sub$delta

# for annotating
xrng <- range(pwt_sub$year)
yrng <- range(pwt_sub$gross_return)
ymin <- 0.06
ymax <- 0.17
lab  <- paste("Long-run Level = ", format(round(mean(pwt_sub$gross_return), 3), nsmall=3))

myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=gross_return), color= "darkblue", size=1) +
  geom_smooth(aes(x=year, y=gross_return), method="lm", formula="y ~ 1", se=FALSE, color= "darkred") +
  geom_label(aes(x = xrng[2], y = ymax, label = lab),
             hjust = 1, vjust = 1, label.r = unit(0, "lines"), label.padding = unit(0.35, "lines")) +
  coord_cartesian(xlim=c(1950, 2020), ylim=c(ymin, ymax)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  labs(x = "Year t",
       y = "Gross Return on Capital") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig10.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Kaldor Fact 5: Constant share of labor compensation in GDP
###########

# for annotating
xrng <- range(pwt_sub$year)
yrng <- range(pwt_sub$labsh)
ymin <- 0.5
ymax <- 0.7
lab  <- paste("Long-run Level = ", format(round(mean(pwt_sub$labsh), 3), nsmall=3))

myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=labsh), color= "darkblue", size=1) +
  geom_smooth(aes(x=year, y=labsh), method="lm", formula="y ~ 1", se=FALSE, color= "darkred") +
  geom_label(aes(x = xrng[2], y = ymax, label = lab), 
             hjust = 1, vjust = 1, label.r = unit(0, "lines"), label.padding = unit(0.35, "lines")) +
  coord_cartesian(xlim=c(1950, 2020), ylim=c(ymin, ymax)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  labs(x = "Year t",
       y = "Share of Labor Compensation in GDP") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig11.pdf", width = height*aspect_ratio, height = height)
}
