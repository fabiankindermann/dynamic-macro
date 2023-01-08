#######################################
### Filtering GDP
#######################################

# Clear the workspace and load libraries
rm(list = ls())
library(zoo)
library(xts)
library(dynlm)
library(reshape2)
library(base)
library(ggplot2)
library(grid)
library(scales)
library(stringr)
library(tidyverse)
library(fredr)
library(mFilter)
library(formattable)

# should graphs be exported to pdf
export_pdf <- FALSE

# define some colors
mygreen <- "#00BA38"
myblue  <- "#619CFF"
myred   <- "#F8766D"
  
# define plotting breaks
xbreaks <- c(seq(from = as.Date("1950-01-01"), to = as.Date("2020-01-01"),by = "10 years"))


########### 
# US quarterly GDP
###########

# In order to be able to download data from FRED, you have to go to
#     https://fred.stlouisfed.org/docs/api/api_key.html
# Create an account and request an API key for downloading data. Then 
# uncomment the statement above and insert your API key.
# All data series available in FRED can be investigated at 
#     https://fred.stlouisfed.org.

#fredr_set_key("type-you-key")

# extract real GDP for the US (quarterly)
gdp_real <- fredr(series_id = "GDPC1",
                  observation_start = as.Date("1948-01-01"),
                  observation_end = as.Date("2021-12-31")
)

# calculate log gdp
gdp_real$log_gdp = log(gdp_real$value)



########### 
# Plot log GDP
###########

# Plot GDP and estimated trends
myplot <- ggplot(data = gdp_real) + 
  geom_line(aes(x=date, y=log_gdp), color="darkblue", size=1) +
  labs(x = "Year t",
       y = "Log of real GDP") +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  theme_bw() +
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig1.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Decomposition into trend and cycle
###########

# create time series for log GDP
gdp_ts <- xts(x = gdp_real$log_gdp, order.by = as.Date(gdp_real$date))


# TREND 1: A linear trend
res_lin <- lm(gdp_ts ~ as.numeric(index(gdp_ts)))
summary(res_lin)

# calculate trend and cycle
gdp_real$g_lin <- predict(res_lin)
gdp_real$c_lin <- gdp_real$log_gdp - gdp_real$g_lin


# TREND 2: The Hamilton (2018) filter

# run regression with h = 8 and p = 4
h <- 8
res_ham <- lm(stats::lag(gdp_ts, k=-8) ~ stats::lag(gdp_ts, k=0) + stats::lag(gdp_ts, k=1) + stats::lag(gdp_ts, k=2) + stats::lag(gdp_ts, k=3))
summary(res_ham)

# calculate trend and cycle
gdp_real$g_ham <- NA
gdp_real$c_ham <- NA
gdp_real$g_ham[12:length(gdp_real$log_gdp)] <- predict(res_ham)
gdp_real$c_ham[12:length(gdp_real$log_gdp)] = gdp_real$log_gdp[12:length(gdp_real$log_gdp)] - gdp_real$g_ham[12:length(gdp_real$log_gdp)]


# TREND 3: The Hodrick-Prescott (1997) filter

res_hpf <- hpfilter(gdp_ts, freq=1600, type="lambda")
summary(res_hpf)

# calculate trend and cycle
gdp_real$g_hpf <- res_hpf$trend
gdp_real$c_hpf <- res_hpf$cycle


########### 
# Plot GDP and estimated trends
###########

# Plot GDP and estimated trends
myplot <- ggplot(data = gdp_real) + 
  geom_line(aes(x=date, y=log_gdp), color="darkblue", size=1) +
  geom_line(aes(x=date, y=g_lin, color="linear"), size=0.5) +
  geom_line(aes(x=date, y=g_ham, color="hamilton"), size=0.5) +
  geom_line(aes(x=date, y=g_hpf, color="hpfilter"), size=0.5) +
  labs(x = "Year t",
       y = "Log of real GDP") +
  scale_color_manual(breaks = c("linear", "hamilton", "hpfilter"),
                     labels = c("Linear", "Hamilton", "HP-Filter"),
                     values = c(mygreen, myblue, myred), name='') +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
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
# Extract and date the official NBER recessions
###########

# extract NBER recession indicator (including peak quarter)
recession <- fredr(series_id = "USRECQP",
                   observation_start = as.Date("1948-01-01"),
                   observation_end = as.Date("2021-12-31")
)

# add quarter numbers to recession dataset
recession$quarter <- c(1:length(recession$date))

# generate differences and trace out recession dates
recession$diff <- 0
recession$diff[2:length(recession$diff)] = diff(recession$value)

# date at beginning and end of recessions
rstart <- recession$date[recession$diff == 1]
rend   <- recession$date[recession$diff == -1]
rec_df <- data.frame(rstart,rend)



########### 
# Plot cycle of log GDP
###########

# set minimum and maximum
ymin <- -0.25
ymax <-  0.15
dy   <- (abs(ymin)+abs(ymax))/2

# plot cycle
myplot <- ggplot(data = gdp_real) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_hline(yintercept=0, color="gray", size=0.5) +
  geom_line(aes(x=date, y=c_lin, color="linear"), size=0.5) +
  geom_line(aes(x=date, y=c_ham, color="hamilton"), size=0.5) +
  geom_line(aes(x=date, y=c_hpf, color="hpfilter"), size=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 0.05), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Log of real GDP (cyclical component)") +
  scale_color_manual(breaks = c("linear", "hamilton", "hpfilter"),
                     labels = c("Linear", "Hamilton", "HP-Filter"),
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
# Cycle after HP-filtering
###########

# set minimum and maximum
ymin <- -0.1
ymax <-  0.05
dy   <- (abs(ymin)+abs(ymax))/2

# plot cycle
myplot <- ggplot(data = gdp_real) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_hline(yintercept=0, color="gray", size=0.5) +
  geom_line(aes(x=date, y=c_hpf), color=myred, size=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 0.025), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Log of real GDP (cyclical component)") +
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
# Recession dates and cyclical component
###########

# calculate NBER dates recession statistics
rec_data <- data.frame("Starting Quarter"=recession$date[recession$diff == 1])
names(rec_data)[1] <- "Starting Quarter" 
rec_data <- cbind(rec_data, "Number of Quarters" = 
                      (recession$quarter[recession$diff == -1]-recession$quarter[recession$diff == 1]))
rec_data <- cbind(rec_data, "Log-point decline in cyclical component (in %)" = 
                      percent(gdp_real$c_hpf[recession$diff == -1]-gdp_real$c_hpf[recession$diff == 1]))

# output table
formattable(rec_data,
              align= c("c", "c", "c"))