#######################################
### Business Cycle Statistics
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

# define start and end date
start_date <- as.Date("1948-01-01")
end_date   <- as.Date("2024-12-31")

# define plotting breaks
xbreaks <- c(seq(from = as.Date("1950-01-01"), to = as.Date("2020-01-01"),by = "10 years"))


########### 
# Quarterly data from the FRED database
###########

# Load function to get FRED data
source("chap06_func_get_fred_data.R")

# US recession dates
selection <- data.frame(series = "USRECQP", names = "RECESSION")

# nominal time series
selection <- rbind(selection, c("GDP", "GDP"))
selection <- rbind(selection, c("PCEC", "CONS"))
selection <- rbind(selection, c("PCDG", "CONS_D"))
selection <- rbind(selection, c("PCND", "CONS_N"))
selection <- rbind(selection, c("PCESV", "CONS_S"))
selection <- rbind(selection, c("GPDI", "INV"))
selection <- rbind(selection, c("PRFI", "INV_R"))
selection <- rbind(selection, c("PNFI", "INV_N"))
selection <- rbind(selection, c("CBI", "INV_I"))
selection <- rbind(selection, c("GCE", "GOV"))
selection <- rbind(selection, c("EXPGS", "EXP"))
selection <- rbind(selection, c("IMPGS", "IMP"))

# price deflators
selection <- rbind(selection, c("GDPDEF", "P_GDP"))
selection <- rbind(selection, c("DPCERD3Q086SBEA", "P_CONS"))
selection <- rbind(selection, c("DDURRD3Q086SBEA", "P_CONS_D"))
selection <- rbind(selection, c("DNDGRD3Q086SBEA", "P_CONS_N"))
selection <- rbind(selection, c("DSERRD3Q086SBEA", "P_CONS_S"))
selection <- rbind(selection, c("A006RD3Q086SBEA", "P_INV"))
selection <- rbind(selection, c("A011RD3Q086SBEA", "P_INV_R"))
selection <- rbind(selection, c("A008RD3Q086SBEA", "P_INV_N"))
selection <- rbind(selection, c("A371RD3Q086SBEA", "P_INV_I"))
selection <- rbind(selection, c("A822RD3Q086SBEA", "P_GOV"))
selection <- rbind(selection, c("A020RD3Q086SBEA", "P_EXP"))
selection <- rbind(selection, c("A021RD3Q086SBEA", "P_IMP"))

# additional series
selection <- rbind(selection, c("CE16OV", "EMPL"))
selection <- rbind(selection, c("UNRATE", "UNEMP"))
selection <- rbind(selection, c("AWHMAN", "HOURS"))
selection <- rbind(selection, c("PRS85006092", "LPROD"))
selection <- rbind(selection, c("PRS85006151", "REALCOMP"))
selection <- rbind(selection, c("TB3MS", "NOMRATE"))

# pull all data series from FRED
fred <- get_fred_data(selection$series, selection$names, start_date, end_date, "q")


# calculate real variables by deflating nominal ones
for(var in c("GDP", "CONS_D", "CONS_N", "CONS_S", "INV_R", "INV_N", "INV_I", "GOV", "EXP", "IMP")) {
  fred[paste("R_", var, sep="")] = fred[var]/fred[paste("P_", var, sep="")]*100
}

# calculate adjusted real GDP (because of different deflators that don't finally add up correctly)
factor <- fred$R_GDP/(fred$R_CONS_D + fred$R_CONS_N + fred$R_CONS_S +fred$R_INV_R + fred$R_INV_N + fred$R_INV_I + fred$R_GOV + fred$R_EXP - fred$R_IMP)

# scale series
for(var in c("CONS_D", "CONS_N", "CONS_S", "INV_R", "INV_N", "INV_I", "GOV", "EXP", "IMP")) {
  fred[paste("R_", var, sep="")] = fred[paste("R_", var, sep="")]*factor
}

# calculate aggregate real consumption and investment
fred["R_CONS"] = fred["R_CONS_D"] + fred["R_CONS_N"] + fred["R_CONS_S"]
fred["R_INV"] = fred["R_INV_R"] + fred["R_INV_N"] + fred["R_INV_I"]

# determine net exports
fred$XM   <- fred$EXP   - fred$IMP
fred$R_XM <- fred$R_EXP - fred$R_IMP


########### 
# Calculate Statistics Table
###########

# calculate recession beginnings and ends
fred$REC_DATES <- 0
fred$REC_DATES[2:length(fred$REC_DATES)] <- diff(fred$RECESSION)

# calculate normal GDP growth (in quarterly terms)
y_normal <- mean(diff(fred$R_GDP)/fred$R_GDP[1:length(fred$R_GDP)-1])

# GDP growth in recessions relative to normal growth
y_rec <- vector();
for(t in 1:length(fred$R_GDP)) {
  if(fred$RECESSION[t] == 1 & fred$REC_DATES[t] != -1) {
    y_rec <- append(y_rec, (fred$R_GDP[t+1] - fred$R_GDP[t])/fred$R_GDP[t])
  }
}

# calculate mean across recessions
y_rec <- mean(y_rec)


# here statistics are stored
dat <- array(dim=c(10,2))

# iterate over relevant variables
i <- 1
for(var in c("CONS", "CONS_D", "CONS_N", "CONS_S", "INV", "INV_R", "INV_N", "INV_I", "GOV", "XM")) {
  
  # get mean share in GDP
  dat[i, 1] <- mean(unlist(fred[var]/fred["GDP"]))
  
  # get real variable name
  rvar <- paste("R_", var, sep="")
  
  # calculate normal share in GDP growth of variable
  v_normal <- mean(diff(unlist(fred[rvar]))/unlist(fred[1:length(fred$R_GDP)-1,]["R_GDP"]))

  # variable share of GDP growth in recessions
  v_rec <- vector();
  for(t in 1:length(fred$R_GDP)) {
    if(fred$RECESSION[t] == 1 & fred$REC_DATES[t] != -1) {
      v_rec <- append(v_rec, unlist(fred[t+1,][rvar] - fred[t,][rvar])/fred$R_GDP[t])
    }
  }
  
  # calculate mean across recessions
  v_rec <- mean(v_rec)
  
  # calculate share in GDP fall relative to normal growth
  dat[i, 2] <- (v_rec - v_normal)/(y_rec - y_normal)
  
  # increment i
  i = i + 1
}


########### 
# Generate Table
###########

table_data <- data.frame("Component" = 
                          c("Consumption", " - Durables", " - Nondurables", " - Services",
                            "Investment", " - Residential", " - Fixed Nonresidential", " - Inventories",
                            "Government Purchases", "Net Exports"))
table_data <- cbind(table_data, "Average share in GDP" = percent(dat[, 1]))
table_data <- cbind(table_data, "Average share in fall in GDP in recessions relative to normal growth" = percent(dat[, 2]))

# output table
formattable(table_data,
            align= c("l", "c", "c"))


########### 
# Create different cyclical plots
###########

# date at beginning and end of recessions
rstart <- fred$date[fred$REC_DATES == 1]
rend   <- fred$date[fred$REC_DATES == -1]
rec_df <- data.frame(rstart, rend)


########### 
# Employment
###########

# calculate employment changes
fred$D_EMPL <- NA
fred$D_EMPL[1:(length(fred$EMPL)-1)] <- diff(fred$EMPL)/fred$EMPL[1:(length(fred$EMPL)-1)]*100

# set minimum and maximum
ymin <- -14
ymax <- 8
dy   <- (abs(ymin)+abs(ymax))/2

# create plot
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_hline(yintercept=0, color="gray", linewidth=0.5) +
  geom_line(aes(x=date, y=D_EMPL), color="darkblue", linewidth=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 2), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Employment Change (in %)") +
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
# Unemployment
###########

# set minimum and maximum
ymin <- 0
ymax <- 15
dy   <- (abs(ymin)+abs(ymax))/2

# create plot
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_line(aes(x=date, y=UNEMP), color="darkblue", linewidth=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 2.5), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Unemployment Rate (quarterly, in %)") +
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
# Average weekly hours of production workers in manufacturing
###########

# set minimum and maximum
ymin <- 36
ymax <- 44
dy   <- (abs(ymin)+abs(ymax))/2

# create plot
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_line(aes(x=date, y=HOURS), color="darkblue", linewidth=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 2), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Average Weekly Hours, Prod. Workers") +
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
# Change in output per hour, non-farm business
###########

# set minimum and maximum
ymin <- -8
ymax <- 18
dy   <- (abs(ymin)+abs(ymax))/2

# create plot
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_hline(yintercept=0, color="gray", linewidth=0.5) +
  geom_line(aes(x=date, y=LPROD), color="darkblue", linewidth=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 4), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Change in Output per Hour (in %)") +
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
# Change in real compensation per hour, non-farm business
###########

# set minimum and maximum
ymin <- -4
ymax <- 10
dy   <- (abs(ymin)+abs(ymax))/2

# create plot
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_hline(yintercept=0, color="gray", linewidth=0.5) +
  geom_line(aes(x=date, y=REALCOMP), color="darkblue", linewidth=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 4), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Change in Real Compensation per Hour (in %)") +
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
# Inflation
###########

# calculate annualized inflation from GDP deflator
fred$INFL <- NA
fred$INFL[5:(length(fred$P_GDP))] <- diff(fred$P_GDP, lag = 4)/fred$P_GDP[1:(length(fred$P_GDP)-4)]*100

# set minimum and maximum
ymin <- -2
ymax <- 12
dy   <- (abs(ymin)+abs(ymax))/2

# create plot
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_hline(yintercept=0, color="gray", linewidth=0.5) +
  geom_line(aes(x=date, y=INFL), color="darkblue", linewidth=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 2), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Inflation Relative to Previous Year (in %)") +
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
# Nominal interest rate on 3-month treasuries
###########

# set minimum and maximum
ymin <- 0
ymax <- 16
dy   <- (abs(ymin)+abs(ymax))/2

# create plot
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_line(aes(x=date, y=NOMRATE), color="darkblue", linewidth=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 2), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Nominal Interest on 3-month Treasuries (in %)") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig11.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Ex post real interest rate on 3-month treasuries
###########

# set minimum and maximum
ymin <- -8
ymax <- 8
dy   <- (abs(ymin)+abs(ymax))/2

# create plot
myplot <- ggplot(data = fred) + 
  geom_rect(data=rec_df, aes(xmin=rstart, xmax=rend, ymin=ymin-0.05*dy, ymax=ymax+0.05*dy), fill='gray', alpha=0.5) + 
  geom_hline(yintercept=0, color="gray", linewidth=0.5) +
  geom_line(aes(x=date, y=NOMRATE-INFL), color="darkblue", linewidth=0.5) +
  scale_x_date(breaks = xbreaks, date_labels = "%Y", expand=c(0, 0)) +
  scale_y_continuous(breaks=seq(ymin, ymax, 2), expand=c(0, 0)) +  
  labs(x = "Year t",
       y = "Real Interest on 3-month Treasuries (in %)") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig12.pdf", width = height*aspect_ratio, height = height)
}