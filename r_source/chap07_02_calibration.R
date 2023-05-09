#######################################
### Model Calibration
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
library(nortsTest)

# should graphs be exported to pdf
export_pdf <- FALSE

# define some colors
mygreen <- "#00BA38"
myblue  <- "#619CFF"
myred   <- "#F8766D"
  
# define plotting breaks
xbreaks <- c(seq(from = as.Date("1950-01-01"), to = as.Date("2020-01-01"),by = "10 years"))


########### 
# Pull data from FRED database
###########

source("chap06_func_get_fred_data.R")


########### 
# Quarterly time series
###########

# nominal time series
selection <- data.frame(series = "GDP", names = "GDP")
selection <- rbind(selection, c("PCEC", "CONS"))
selection <- rbind(selection, c("PCND", "CONS_N"))
selection <- rbind(selection, c("PCESV", "CONS_S"))
selection <- rbind(selection, c("GPDI", "INV"))
selection <- rbind(selection, c("FPI", "INV_F"))
selection <- rbind(selection, c("GCE", "GOV"))
selection <- rbind(selection, c("TB3MS", "NOMRATE"))
selection <- rbind(selection, c("EXPGS", "EXP"))
selection <- rbind(selection, c("IMPGS", "IMP"))

# price deflators
selection <- rbind(selection, c("GDPDEF", "P_GDP"))
selection <- rbind(selection, c("DPCERD3Q086SBEA", "P_CONS"))
selection <- rbind(selection, c("DNDGRD3Q086SBEA", "P_CONS_N"))
selection <- rbind(selection, c("DSERRD3Q086SBEA", "P_CONS_S"))
selection <- rbind(selection, c("A006RD3Q086SBEA", "P_INV"))
selection <- rbind(selection, c("A007RD3Q086SBEA", "P_INV_F"))
selection <- rbind(selection, c("A822RD3Q086SBEA", "P_GOV"))
selection <- rbind(selection, c("A020RD3Q086SBEA", "P_EXP"))
selection <- rbind(selection, c("A021RD3Q086SBEA", "P_IMP"))

# employment and labor compensation
selection <- rbind(selection, c("HOABS", "HOURS"))
selection <- rbind(selection, c("HOANBS", "HOURS_NF"))
selection <- rbind(selection, c("A358RX1Q020SBEA", "VA_NF"))
selection <- rbind(selection, c("A439RC1Q027SBEA", "VA_NET"))
selection <- rbind(selection, c("A442RC1Q027SBEA", "EMPCOMP"))

# pull all quarterly data series from FRED
fred_q <- get_fred_data(selection$series, selection$names, "1948-01-01", "2021-12-31", "q")


########### 
# Annual time series
###########

selection <- data.frame(series = "GDP", names = "GDP")
selection <- rbind(selection, c("K1TTOTL1ES000", "CAPSTOCK"))
selection <- rbind(selection, c("AVHWPEUSA065NRUG", "HOURS"))
selection <- rbind(selection, c("A442RC1A027NBEA", "COMPENSATION"))

# pull all annual data series from FRED
fred_a <- get_fred_data(selection$series, selection$names, "1950-01-01", "1991-12-31", "a")


########### 
# Get calibration target data
###########

# calculate real variables by deflating nominal ones
for(var in c("GDP", "CONS", "CONS_N", "CONS_S", "INV", "INV_F", "GOV", "EXP", "IMP")) {
  fred_q[paste("R_", var, sep="")] = fred_q[var]/fred_q[paste("P_", var, sep="")]*100
}

# calculate adjusted real GDP (because of different deflators that don't finally add up correctly)
factor <- fred_q$R_GDP/(fred_q$R_CONS +fred_q$R_INV + fred_q$R_GOV + fred_q$R_EXP - fred_q$R_IMP)

# scale series
for(var in c("GDP", "CONS", "INV", "GOV", "EXP", "IMP")) {
  fred_q[paste("R_", var, sep="")] = fred_q[paste("R_", var, sep="")]*factor
}


# filter government spending
ts <- xts(x = log(fred_q$R_GOV), order.by = as.Date(fred_q$date))
hpf <- hpfilter(ts, freq=1600, type="lambda")
fred_q$gov_cycle <- hpf$cycle
gov_exp <- hpf$cycle[40:length(hpf$cycle)]

# Plot cycle of government expenditure
myplot <- ggplot(data = fred_q) + 
  geom_vline(xintercept=40, color="gray", linewidth=0.5) +
  geom_hline(yintercept=0, color="gray", linewidth=0.5) +
  geom_line(aes(x=quarter, y=gov_cycle), color="darkblue", linewidth=1) +
  labs(x = "Quarter t",
       y = "Cycle of log(G_t)") +
  coord_cartesian(ylim=c(-0.15, 0.15)) +
  scale_y_continuous(breaks=seq(-0.15, 0.15, 0.05), label=comma) +  
  theme_bw()

# print the plot
print(myplot)

if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig7.pdf", width = height*aspect_ratio, height = height)
}


# generate partial autocorrelation plot
myplot <- ggpacf(gov_exp[40:length(gov_exp)]) +
  coord_cartesian(ylim=c(-0.5, 1)) +  
  scale_y_continuous(breaks=seq(-0.5, 1, 0.25)) +  
  labs(x = "Lag",
       y = "Partial Auto-Correlation") +
  theme_bw()
print(myplot)

if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig8.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Calculate calibration targets
###########

# calculate relevant statistics
dat <- array(dim=c(8))

# consumption share in GDP
dat[1] <- mean(fred_q$CONS/fred_q$GDP)
dat[2] <- mean(fred_q$INV/fred_q$GDP)
dat[3] <- mean(fred_q$GOV/fred_q$GDP)
dat[4] <- mean(fred_a$HOURS)/(365*(24-8))
dat[5] <- mean(fred_q$EMPCOMP/fred_q$VA_NET)
dat[6] <- mean(fred_a$CAPSTOCK/(fred_a$GDP/4))

# calculate autocorrelation of government spending
reg <- lm(gov_exp ~ lag(gov_exp))
dat[7] <- reg$coefficients[2]
dat[8] <- sd(gov_exp)*sqrt(1 - dat[7]^2)


# generate data table
table_data <- data.frame("Data" = 
                           c("Consumption C/Y", "Investment I/Y", 
                             "Government purchases G/Y", "Hours worked (share of endowment)",
                             "Labor share corporate sector wL/Y", "Capital K/Y (quarterly)",
                             "Government purchases: autocorrelation rho_G", "Government purchases: sd sigma_G"))
table_data <- cbind(table_data, "Average (1948-2021)" = dat)


# output table
formattable(table_data, digits = 2,
            align= c("l", "r"))


########### 
# Calculate calibrated parameters
###########

# share or labor compensation -> production share
alpha <- 1 - dat[5]

# investment share in GDP -> depreciation
delta <- dat[2]/dat[6]

# capital share in production -> time preference rate
theta <- alpha/dat[6] - delta

# time discount factor
beta  <- 1/(1+theta) 

# labor hours share -> disutility of labor
chi   <- (1-alpha)/(1 - dat[2] - dat[3]) *(1-dat[4])/dat[4]

# government share in production
g_y   <- dat[3]

# government expenditure/GDP -> G_bar
G_bar <- log(dat[3]*dat[6]^(alpha/(1-alpha))*dat[4])

# autocorrelation parameter government expenditure
rho_G    <- dat[7]

# variance of government expenditure
sigma_G  <- dat[8]

# autocorrelation parameter of technology shocks
rho_A    <- 0.95


# generate calibrated value table
table_data <- data.frame("Parameter" = 
                           c("Production parameter (alpha)", "Depreciation rate (delta)", 
                             "Time discount factor (beta)", "Disutility of labor (chi)",
                             "Government share (g_y)", "Mean gov. expend. process (G_bar)",
                             "Autocorrelation gov. expend. rho_G", "Standard deviation gov. expend. sigma_G"))
table_data <- cbind(table_data, "Value" = c(alpha, delta, beta, chi, g_y, G_bar, rho_G, sigma_G))


# output table
formattable(table_data, digits=2,
            align= c("l", "r"))


########### 
# Calculate the steady state
###########

# interest rate
r  <- theta

# capital intensity
KY <- alpha/(theta + delta)

# investment share
IY <- delta*KY

# government share
GY <- dat[3]

# consumption share
CY <- 1 - IY - GY

# labor supply
L  <- (1-alpha)/(1-alpha + chi*CY)

# output level
Y  <- KY^(alpha/(1-alpha)) * L

# wage rate
w = (1-alpha)*Y/L

# share of labor compensation
wL <- w*L/Y


# generate data table
table_data <- data.frame("Data" = 
                           c("Consumption C/Y", "Investment I/Y", 
                             "Government purchases G/Y", "Hours worked (share of endowment)",
                             "Labor share corporate sector wL/Y", "Capital K/Y (quarterly)",
                             "Interest rate (quarterly)"))
table_data <- cbind(table_data, "Model Value" = c(CY, IY, GY, L, wL, KY, r))

# output table
formattable(table_data,
            align= c("l", "r"), digits = 3)


########### 
# Save parameters and steady state values to file
###########

save(file="chap07_parameters.RData", 
     list=c("alpha", "delta", "beta", "chi", "g_y", "G_bar", "rho_G", "sigma_G", "rho_A",
            "CY", "IY", "GY", "L", "r", "w", "KY", "Y"))


########### 
# Filter time series using HP filter to replicate Hansen and Wright (1992, Tab. 2)
###########

# filter GDP
ts <- xts(x = log(fred_q$R_GDP), order.by = as.Date(fred_q$date))
hpf <- hpfilter(ts, freq=1600, type="lambda")
fred_q$GDP_cycle <- hpf$cycle 

# filter consumption
ts <- xts(x = log(fred_q$R_CONS_N + fred_q$R_CONS_S), order.by = as.Date(fred_q$date))
hpf <- hpfilter(ts, freq=1600, type="lambda")
fred_q$CONS_cycle <- hpf$cycle 

# filter investment
ts <- xts(x = log(fred_q$R_INV_F), order.by = as.Date(fred_q$date))
hpf <- hpfilter(ts, freq=1600, type="lambda")
fred_q$INV_cycle <- hpf$cycle 

# filter employment in non farm business
ts <- xts(x = log(fred_q$HOURS_NF), order.by = as.Date(fred_q$date))
hpf <- hpfilter(ts, freq=1600, type="lambda")
fred_q$HOURS_cycle <- hpf$cycle 

# filter productivity in non-farm business
ts <- xts(x = log(fred_q$VA_NF/fred_q$HOURS_NF), order.by = as.Date(fred_q$date))
hpf <- hpfilter(ts, freq=1600, type="lambda")
fred_q$PROD_cycle <- hpf$cycle 


# calculate relevant statistics
emp_data <- array(dim=c(6, 2))

# consumption share in GDP
emp_data[1, 1] <- sd(fred_q$GDP_cycle)
emp_data[2, 1] <- sd(fred_q$CONS_cycle) /sd(fred_q$GDP_cycle)
emp_data[3, 1] <- sd(fred_q$INV_cycle)  /sd(fred_q$GDP_cycle)
emp_data[4, 1] <- sd(fred_q$PROD_cycle) /sd(fred_q$GDP_cycle)
emp_data[5, 1] <- sd(fred_q$HOURS_cycle)/sd(fred_q$GDP_cycle)
emp_data[6, 1] <- sd(fred_q$HOURS_cycle) /sd(fred_q$PROD_cycle)

emp_data[1, 2] <- 1
emp_data[2, 2] <- cor(fred_q$CONS_cycle, fred_q$GDP_cycle)
emp_data[3, 2] <- cor(fred_q$INV_cycle , fred_q$GDP_cycle)
emp_data[4, 2] <- cor(fred_q$PROD_cycle, fred_q$GDP_cycle)
emp_data[5, 2] <- cor(fred_q$HOURS_cycle,fred_q$GDP_cycle)
emp_data[6, 2] <- cor(fred_q$HOURS_cycle,fred_q$PROD_cycle)


# generate data table
table_data <- data.frame("Statistics" = 
                           c("sd(Y) in %", 
                             "sd(C)/sd(Y)", "corr(C, Y)",
                             "sd(I)/sd(Y)", "corr(I, Y)",
                             "sd(w)/sd(Y)", "corr(w, Y)",
                             "sd(L)/sd(Y)", "corr(L, Y)",
                             "sd(L)/sd(w)", "corr(L, w)"))
table_data <- cbind(table_data, "US Data 1948-2021" = c(emp_data[1, 1]*100, emp_data[2, 1],
                                emp_data[2, 2], emp_data[3, 1], emp_data[3, 2], emp_data[4, 1], emp_data[4, 2],
                                emp_data[5, 1], emp_data[5, 2], emp_data[6, 1], emp_data[6, 2]))


# output table
formattable(table_data, digits = 2,
            align= c("l", "r"))


# save empirical data
save(file="chap07_empirics.RData", 
     list=c("emp_data"))