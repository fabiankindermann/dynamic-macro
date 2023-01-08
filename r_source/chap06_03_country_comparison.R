#######################################
### Comparison with Germany
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
library(formattable)

# should graphs be exported to pdf
export_pdf <- FALSE

# define some colors
mygreen <- "#00BA38"
myblue  <- "#619CFF"
myred   <- "#F8766D"

# define plotting breaks
xbreaks <- c(seq(from = as.Date("1970-01-01"), to = as.Date("2020-01-01"),by = "10 years"))


########### 
# Quarterly data from the FRED database
###########

# Load function to get FRED data
source("chap06_func_get_fred_data.R")

# US recession dates
selection <- data.frame(series = "DEURECDP", names = "RECESSION")

# real time series
selection <- rbind(selection, c("GDPC1", "GDP_US"))
selection <- rbind(selection, c("NAEXKP01DEQ661S", "GDP_DE"))

# additional series
selection <- rbind(selection, c("CE16OV", "EMPL_US"))
selection <- rbind(selection, c("LFEMTTTTDEQ647S", "EMPL_DE"))
selection <- rbind(selection, c("UNRATE", "UNEMP_US"))
selection <- rbind(selection, c("LRUNTTTTDEQ156S", "UNEMP_DE"))
selection <- rbind(selection, c("CPALTT01DEQ659N", "INFL_DE"))
selection <- rbind(selection, c("TB3MS", "NOMRATE_US"))
selection <- rbind(selection, c("IRLTLT01DEQ156N", "NOMRATE_DE"))

# pull all data series from FRED
fred <- get_fred_data(selection$series, selection$names, "1970-01-01", "2021-12-31", "q")

# calculate inflation for US
deflator <- fredr(series_id = "GDPDEF",
                  observation_start = as.Date("1969-01-01"),
                  observation_end = as.Date("2021-12-31")
)

fred$INFL_US <- diff(deflator$value, lag = 4)/deflator$value[1:(length(deflator$value)-4)]*100

########### 
# Create different cyclical plots
###########

# calculate regression beginnings and ends
fred$REC_DATES <- 0
fred$REC_DATES[2:length(fred$REC_DATES)] <- diff(fred$RECESSION)

# date at beginning and end of recessions
rstart <- fred$date[fred$REC_DATES == 1]
rend   <- fred$date[fred$REC_DATES == -1]
rec_df <- data.frame(rstart, rend)

# calculate log gdp for US
fred$log_gdp_US = log(fred$GDP_US)

gdp_ts <- xts(x = fred$log_gdp_US, order.by = as.Date(fred$date))
res_hpf <- hpfilter(gdp_ts, freq=1600, type="lambda")
fred$c_hpf_US <- res_hpf$cycle

# calculate log gdp for Germany
fred$log_gdp_DE = log(fred$GDP_DE)

gdp_ts <- xts(x = fred$log_gdp_DE, order.by = as.Date(fred$date))
res_hpf <- hpfilter(gdp_ts, freq=1600, type="lambda")
fred$c_hpf_DE <- res_hpf$cycle


########### 
# Cycle after HP-filtering
###########

# set minimum and maximum
ymin <- -0.1
ymax <-  0.05
dy   <- (abs(ymin)+abs(ymax))/2

# plot cycle
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_hline(yintercept=0, color="gray", size=0.5) +
  geom_line(aes(x=date, y=c_hpf_US, color="US"), size=0.5) +
  geom_line(aes(x=date, y=c_hpf_DE, color="DE"), size=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 0.025), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Log of real GDP (cyclical component)") +
  scale_color_manual(breaks = c("US", "DE"),
                     labels = c("US", "Germany"),
                     values = c("darkblue", myred), name='') +
  theme_bw() +
  theme(legend.position="bottom")

# print the plot
print(myplot)


# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig13.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Employment
###########

# calculate employment changes for US
fred$D_EMPL_US <- NA
fred$D_EMPL_US[1:(length(fred$EMPL_US)-1)] <- diff(fred$EMPL_US)/fred$EMPL_US[1:(length(fred$EMPL_US)-1)]*100

# calculate employment changes for Germany
fred$D_EMPL_DE <- NA
fred$D_EMPL_DE[1:(length(fred$EMPL_DE)-1)] <- diff(fred$EMPL_DE)/fred$EMPL_DE[1:(length(fred$EMPL_DE)-1)]*100

# set minimum and maximum
ymin <- -14
ymax <- 8
dy   <- (abs(ymin)+abs(ymax))/2

# plot cycle
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_hline(yintercept=0, color="gray", size=0.5) +
  geom_line(aes(x=date, y=D_EMPL_US, color="US"), size=0.5) +
  geom_line(aes(x=date, y=D_EMPL_DE, color="DE"), size=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 2), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Employment Change (in %)") +
  scale_color_manual(breaks = c("US", "DE"),
                     labels = c("US", "Germany"),
                     values = c("darkblue", myred), name='') +
  theme_bw() +
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig14.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Unemployment
###########

# set minimum and maximum
ymin <- 0
ymax <- 15
dy   <- (abs(ymin)+abs(ymax))/2

# plot cycle
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_hline(yintercept=0, color="gray", size=0.5) +
  geom_line(aes(x=date, y=UNEMP_US, color="US"), size=0.5) +
  geom_line(aes(x=date, y=UNEMP_DE, color="DE"), size=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 2.5), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Unemployment Rate (quarterly, in %)") +
  scale_color_manual(breaks = c("US", "DE"),
                     labels = c("US", "Germany"),
                     values = c("darkblue", myred), name='') +
  theme_bw() +
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig15.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Inflation
###########

# set minimum and maximum
ymin <- -2
ymax <- 12
dy   <- (abs(ymin)+abs(ymax))/2

# plot cycle
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_hline(yintercept=0, color="gray", size=0.5) +
  geom_line(aes(x=date, y=INFL_US, color="US"), size=0.5) +
  geom_line(aes(x=date, y=INFL_DE, color="DE"), size=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 2), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Inflation Relative to Previous Year (in %)") +
  scale_color_manual(breaks = c("US", "DE"),
                     labels = c("US", "Germany"),
                     values = c("darkblue", myred), name='') +
  theme_bw() +
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig16.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Nominal interest rate on 3-month interbank market
###########

# set minimum and maximum
ymin <- 0
ymax <- 16
dy   <- (abs(ymin)+abs(ymax))/2

# plot cycle
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_hline(yintercept=0, color="gray", size=0.5) +
  geom_line(aes(x=date, y=NOMRATE_US, color="US"), size=0.5) +
  geom_line(aes(x=date, y=NOMRATE_DE, color="DE"), size=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 2), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "3-month nominal rate (in %)") +
  scale_color_manual(breaks = c("US", "DE"),
                     labels = c("US", "Germany"),
                     values = c("darkblue", myred), name='') +
  theme_bw() +
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.8;
  ggsave("fig17.pdf", width = height*aspect_ratio, height = height)
}