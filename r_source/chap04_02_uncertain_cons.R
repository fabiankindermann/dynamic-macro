#######################################
### Consumption under Uncertainty
#######################################

# Clear the workspace and load libraries
rm(list = ls())
library(xts)
library(zoo)
library(dynlm)
library(reshape2)
library(base)
library(ggplot2)
library(grid)
library(scales)
library(stringr)
library(tidyverse)
library(pwt10)
library(fredr)

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


########### 
# Function for modeling unexpected income shocks
###########

income_shock <- function(T, t_beg, t_end, shock) {
  
  # generate expected income path in baseline
  y <- rep(1, T)
  
  # expected permanent income as of date t = 1
  E_y_p <- rep(mean(y), T)
  
  # calculate baseline consumption path
  c_base <- E_y_p
  
  # generate income shock
  y[t_beg:t_end] = y[t_beg:t_end] + shock
  
  # update expectations
  E_y_p[t_beg:T] = mean(y[t_beg:T])
  
  # calculate consumption path
  c <- E_y_p
  
  # calculate savings
  s <- y - c
  
  # derive asset path
  a <- 0
  for(t in 2:T) {
    a[t] = a[t-1] + s[t-1]
  }
  
  # create data set to return
  res <- data.frame(year=c(1:T), c_base, c, E_y_p, y, s, a)
  
  return(res)
}


########### 
# Unexpected unemployment in period 11-15
###########

# simulate shock
consumption <- income_shock(T, 11, 15, -1)

# Plot GDP and its components
myplot <- ggplot(data = consumption) + 
  geom_ribbon(aes(x=year, ymin=0, ymax=c,    fill= "1c", color="1c") , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c, ymax=c+s, fill= "2s", color="2s")  , alpha=0.4) +
  geom_line(aes(x=year, y=y), color="darkblue", size=1) +
  coord_cartesian(xlim=c(1, T), ylim=c(0, 1.5)) + 
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
  ggsave("fig9.pdf", width = height*aspect_ratio, height = height)
}


# Plot dynamics of individual wealth
myplot <- ggplot(data = consumption) + 
  geom_line(aes(x=year, y=a), color="darkblue", size=1) +
  coord_cartesian(xlim=c(1, T), ylim=c(-5, 1)) + 
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
  ggsave("fig10.pdf", width = height*aspect_ratio, height = height)
}


########### 
# The marginal propensity to consume out of permanent income shock
###########

# date at which shock happens
t_beg = 11

# simulate shock
consumption <- income_shock(T, t_beg, T, 1)

# calculate MPC
lab  <- paste("MPC = ", format(round((consumption$c[t_beg]/consumption$c_base[t_beg]-1)*100, 2), nsmall=2), "%")

# Plot GDP and its components
myplot <- ggplot(data = consumption) + 
  geom_line(aes(x=year, y=c_base, color="1"), color=myblue, size=1) +
  geom_line(aes(x=year, y=c, color="2"), color=mygreen, size=1) +
  coord_cartesian(xlim=c(1, T), ylim=c(0, 2.5)) + 
  scale_x_continuous(breaks=seq(0, T, 10), expand=c(0, 0)) +
  geom_label(aes(x = 5, y = 2.5, label = lab), 
             hjust = 0, vjust = 1, label.r = unit(0, "lines"), label.padding = unit(0.35, "lines")) +
  labs(x = "Year t",
       y = "Consumption") +
  scale_color_manual(breaks = c("1", "2"),
                     labels = c("Consumption (Baseline)", "Consumption (Permanent Shock)"),
                     values = c(mygreen, myblue)) +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig11.pdf", width = height*aspect_ratio, height = height)
}


########### 
# The marginal propensity to consume out of transitory income shock
###########

# date at which shock happens
t_beg = 11

# simulate shock
consumption <- income_shock(T, t_beg, t_beg, 1)

# calculate MPC
lab  <- paste("MPC = ", format(round((consumption$c[t_beg]/consumption$c_base[t_beg]-1)*100, 2), nsmall=2), "%")

# Plot GDP and its components
myplot <- ggplot(data = consumption) + 
  geom_line(aes(x=year, y=c_base, color="1"), color=myblue, size=1) +
  geom_line(aes(x=year, y=c, color="2"), color=mygreen, size=1) +
  coord_cartesian(xlim=c(1, T), ylim=c(0, 2.5)) + 
  scale_x_continuous(breaks=seq(0, T, 10), expand=c(0, 0)) +
  geom_label(aes(x = 5, y = 2.5, label = lab), 
             hjust = 0, vjust = 1, label.r = unit(0, "lines"), label.padding = unit(0.35, "lines")) +
  labs(x = "Year t",
       y = "Consumption") +
  scale_color_manual(breaks = c("1", "2"),
                     labels = c("Consumption (Baseline)", "Consumption (Permanent Shock)"),
                     values = c(mygreen, myblue)) +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig12.pdf", width = height*aspect_ratio, height = height)
}



########### 
# Testing random walk hypothesis in macro data
###########

# In order to be able to download data from FRED, you have to go to
#     https://fred.stlouisfed.org/docs/api/api_key.html
# Create an account and request an API key for downloading data. Then 
# uncomment the statement above and insert your API key.
# All data series available in FRED can be investigated at 
#     https://fred.stlouisfed.org.

#fredr_set_key("type-you-key")

# extract real personal consumption expenditures (quarterly)
cons_real <- fredr(series_id = "PCECC96",
                   observation_start = as.Date("1960-01-01"),
                   observation_end = as.Date("2020-01-01")
)

# extract real disposable personal income (quarterly)
inc_real  <- fredr(series_id = "DPIC96",
                   observation_start = as.Date("1960-01-01"),
                   observation_end = as.Date("2020-01-01")
)

# extract total share prices (growth rates, quarterly)
stock_price <- fredr(series_id = "SPASTT01USQ657N",
                    observation_start = as.Date("1960-01-01"),
                    observation_end = as.Date("2020-01-01")
)

# create time series from consumption data
cons <- xts(x = cons_real$value, order.by = as.Date(cons_real$date))

# create time series from income data
inc  <- xts(x = inc_real$value, order.by = as.Date(inc_real$date))

# create time series from share price data
stock <- xts(x = stock_price$value, order.by = as.Date(stock_price$date))


# TEST 1: consumption on lags of consumption and income
res1 <- lm(cons - stats::lag(cons) ~ stats::lag(cons) + stats::lag(inc))
summary(res1)

# TEST 2: consumption on consumption and multiple lags of income
res2 <- lm(cons - stats::lag(cons) ~ stats::lag(cons, k=1) + stats::lag(inc, k=1) + stats::lag(inc, k=2) + stats::lag(inc, k=3) + stats::lag(inc, k=4))
summary(res2)

# TEST 3: consumption on multiple lags of consumption
res3 <- lm(cons - stats::lag(cons) ~ stats::lag(cons, k=1) + stats::lag(cons, k=2) + stats::lag(cons, k=3) + stats::lag(cons, k=4) + stats::lag(cons, k=5))
summary(res3)

# TEST 4: consumption on lags of consumption and share price
res4 <- lm(cons - stats::lag(cons) ~ stats::lag(cons) + stats::lag(stock))
summary(res4)

