#######################################
### Consumption under Certainty
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
library(httr)

# should graphs be exported to pdf
export_pdf <- FALSE

# define some colors
mygreen <- "#00BA38"
myblue  <- "#619CFF"
myred   <- "#F8766D"


########### 
# Model setup
###########

# number of model periods
T   <- 80

# permanent income
y_p <- 1


########### 
# Function for solving permanent income consumption model
###########

permanent_consumption <- function(y) {
  
  # calculate permanent income
  y_p <- mean(y)
  
  # consumption (only depends on permanent income)
  c <- rep(y_p, T)
  
  # calculate savings
  s <- y - y_p
  
  # derive asset path
  a <- 0
  for(t in 2:T) {
    a[t] <- a[t-1] + s[t-1]
  }
  
  # create data set to return
  res <- data.frame(year=c(1:T), c, y_p, y, s, a)
  
  return(res)
}


########### 
# Case 1: Identical income in all periods
###########

# income profile
y <- rep(y_p, T)

# create data set
consumption <- permanent_consumption(y)

# Plot GDP and its components
myplot <- ggplot(data = consumption) + 
  geom_ribbon(aes(x=year, ymin=0, ymax=c,    fill= "1c", color="1c") , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c, ymax=c+s, fill= "2s", color="2s")  , alpha=0.4) +
  geom_line(aes(x=year, y=y), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(1, T), ylim=c(0, 2)) + 
  scale_x_continuous(breaks=seq(0, T, 10), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "Consumption and Savings") +
  scale_fill_manual(breaks = c("1c", "2s"), name = "", 
                    labels = c("Consumption", "Savings"),
                    values = c(mygreen, myblue)) +
  scale_color_manual(breaks = c("1c", "2s"),
                     values = c(mygreen, myblue)) +
  guides(colour = "none") +
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


# Plot dynamics of individual wealth
myplot <- ggplot(data = consumption) + 
  geom_line(aes(x=year, y=a), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(1, T), ylim=c(0, 2)) + 
  scale_x_continuous(breaks=seq(0, T, 10), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "Individual Wealth") +
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
# Case 2: All income in period 1
###########

# income profile
y <- c(y_p*T, rep(0, T-1))

# create data set
consumption <- permanent_consumption(y)

# Plot GDP and its components
myplot <- ggplot(data = consumption) + 
  geom_ribbon(aes(x=year, ymin=0, ymax=c,    fill= "1c", color="1c") , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c, ymax=c+s, fill= "2s", color="2s")  , alpha=0.4) +
  geom_line(aes(x=year, y=y), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(1, T), ylim=c(0, 80)) + 
  scale_x_continuous(breaks=seq(0, T, 10), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "Consumption and Savings") +
  scale_fill_manual(breaks = c("1c", "2s"), name = "", 
                    labels = c("Consumption", "Savings"),
                    values = c(mygreen, myblue)) +
  scale_color_manual(breaks = c("1c", "2s"),
                     values = c(mygreen, myblue)) +
  guides(colour = "none") +
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


# Plot dynamics of individual wealth
myplot <- ggplot(data = consumption) + 
  geom_line(aes(x=year, y=a), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(1, T), ylim=c(0, 80)) + 
  scale_x_continuous(breaks=seq(0, T, 10), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "Individual Wealth") +
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
# Case 3: Only income in first half
###########

# income profile
y <- c(rep(2*y_p, T/2), rep(0, T/2))

# create data set
consumption <- permanent_consumption(y)

# Plot GDP and its components
myplot <- ggplot(data = consumption) + 
  geom_ribbon(aes(x=year, ymin=0, ymax=c,   fill= "1c", color="1c") , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c, ymax=c+s, fill= "2s", color="2s")  , alpha=0.4) +
  geom_line(aes(x=year, y=y), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(1, T), ylim=c(0, 2)) + 
  scale_x_continuous(breaks=seq(0, T, 10), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "Consumption and Savings") +
  scale_fill_manual(breaks = c("1c", "2s"), name = "", 
                    labels = c("Consumption", "Savings"),
                    values = c(mygreen, myblue)) +
  scale_color_manual(breaks = c("1c", "2s"),
                     values = c(mygreen, myblue)) +
  guides(colour = "none") +
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


# Plot dynamics of individual wealth
myplot <- ggplot(data = consumption) + 
  geom_line(aes(x=year, y=a), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(1, T), ylim=c(0, 40)) + 
  scale_x_continuous(breaks=seq(0, T, 10), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "Individual Wealth") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig6.pdf", width = height*aspect_ratio, height = height)
}



########### 
# Empirical Analysis 1: Consumption over time
###########

# load Penn World Tables and extract US data
data("pwt10.0")
pwt_sub <- subset(pwt10.0, isocode=="USA")

# calculate consumption and income per capita
pwt_sub$cons <- pwt_sub$csh_c*pwt_sub$cgdpe/pwt_sub$pop
pwt_sub$inc  <- pwt_sub$cgdpe/pwt_sub$pop

# regress consumption on income per capita
reg_agg <- lm(cons ~ inc, pwt_sub)
summary(reg_agg)


########### 
# Empirical Analysis 2: Consumption in the cross section
###########

# download 2020 CEX from BLS (ATTENTION: 51Mb)
zipfile <- paste(getwd(), "/intrvw20.zip", sep="")
headers = c(`user-agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36')
GET(url = 'https://www.bls.gov/cex/pumd/data/comma/intrvw20.zip', add_headers(.headers=headers), write_disk(zipfile, overwrite=TRUE))
unzip(zipfile)
unlink(zipfile)

# extract fourth quarter consumption
CEX <- read.csv("./intrvw20/fmli204.csv", header=TRUE, sep=",")

# extrapolate quarterly consumption to full year
CEX$cons <- CEX$TOTEXPCQ*4
CEX$inc  <- CEX$FINCBTAX
CEX$race <- CEX$REF_RACE

# regression for all individuals
reg_cross <- lm(cons ~ inc, CEX[CEX$cons > 0, ])
summary(reg_cross)


########### 
# Plot the two consumption functions
###########

# generate consumption functions
y       <- seq(0, 50000)
c_agg   <- reg_agg$coefficients[1] + reg_agg$coefficients[2]*y
c_cross <- reg_cross$coefficients[1] + reg_cross$coefficients[2]*y
cons_reg <- data.frame(y, c_agg, c_cross)

# plot consumption functions
myplot <- ggplot(data = cons_reg) + 
  geom_line(aes(x=y, y=y), color="black", linewidth=0.3) +
  geom_line(aes(x=y, y=c_agg, color="1"), linewidth=1) +
  geom_line(aes(x=y, y=c_cross, color="2"), linewidth=1) +
  coord_cartesian(xlim=c(0, 50000), ylim=c(0, 50000)) + 
  scale_color_manual(breaks = c("1", "2"), name = "", 
                    labels = c("Macro Data", "Micro Data"),
                    values = c(mygreen, myblue)) +
  scale_x_continuous(breaks=seq(0, 50000, 5000)) +
  scale_y_continuous(breaks=seq(0, 50000, 10000)) +
  labs(x = "Income Y",
       y = "Consumption C") +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 4.8;
  ggsave("fig7.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Empirical Analysis 3: Consumption by Race
###########

# regression only for white reference persons
reg_White <- lm(cons ~ inc, CEX[CEX$cons > 0 & CEX$race == 1, ])
summary(reg_White)

# regression only for Black reference persons
reg_Black <- lm(cons ~ inc, CEX[CEX$cons > 0 & CEX$race == 2, ])
summary(reg_Black)

# generate consumption functions
y         <- seq(0, 50000)
c_White   <- reg_White$coefficients[1] + reg_White$coefficients[2]*y
c_Black   <- reg_Black$coefficients[1] + reg_Black$coefficients[2]*y
cons_race <- data.frame(y, c_White, c_Black)

# plot consumption functions
myplot <- ggplot(data = cons_race) + 
  geom_line(aes(x=y, y=y), color="black", linewidth=0.3) +
  geom_line(aes(x=y, y=c_White, color="1"), linewidth=1) +
  geom_line(aes(x=y, y=c_Black, color="2"), linewidth=1) +
  coord_cartesian(xlim=c(0, 50000), ylim=c(0, 50000)) + 
  scale_color_manual(breaks = c("1", "2"), name = "", 
                     labels = c("Whites", "Blacks"),
                     values = c(mygreen, myblue)) +
  scale_x_continuous(breaks=seq(0, 50000, 5000)) +
  scale_y_continuous(breaks=seq(0, 50000, 10000)) +
  labs(x = "Income Y",
       y = "Consumption C") +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 4.8;
  ggsave("fig8.pdf", width = height*aspect_ratio, height = height)
}