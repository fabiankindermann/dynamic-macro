#######################################
### The Penn World Tables
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

# load data and take a look
data("pwt10.0")
View(pwt10.0)

# now extract US data only
pwt_sub = subset(pwt10.0, isocode=="USA")


########### 
# Plot aggregate GDP
###########

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=rgdpe), color= "darkblue", size=1) +
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
  ggsave("fig1.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Plot number of employed
###########

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=emp), color= "darkblue", size=1) +
  coord_cartesian(xlim=c(1950, 2020)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1)) +
  labs(x = "Year t",
       y = "Number of Employed") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig2.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Plot hours worked per employed
###########

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=avh), color= "darkblue", size=1) +
  coord_cartesian(xlim=c(1950, 2020)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  labs(x = "Year t",
       y = "Annual average hours worked") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig3.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Plot total hours worked
###########

# calculate total hours worked
pwt_sub$total_hours = pwt_sub$emp*pwt_sub$avh

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=total_hours), color= "darkblue", size=1) +
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
  ggsave("fig4.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Plot human capital per worker
###########

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=hc), color= "darkblue", size=1) +
  coord_cartesian(xlim=c(1950, 2020)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 10)) +
  labs(x = "Year t",
       y = "Human Capital per Worker (Index Value)") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig5.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Plot aggregate capital stock
###########

# generate plot
myplot <- ggplot(data = pwt_sub) + 
  geom_line(aes(x=year, y=rnna), color= "darkblue", size=1) +
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
  ggsave("fig6.pdf", width = height*aspect_ratio, height = height)
}