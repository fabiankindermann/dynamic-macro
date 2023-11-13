#######################################
### Discussion of Solow growth model
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

# load data and extract 2019 data
data("pwt10.0")
pwt_sub <- subset(pwt10.0, year=="2019")


########### 
# Output per worker across countries
###########

# calculate output per worker
pwt_sub$output_per_worker <- pwt_sub$rgdpe/pwt_sub$emp

# calculate deciles of the output-per-worker distribution
qo <- quantile(pwt_sub[!is.na(pwt_sub$output_per_worker), ]$output_per_worker, 
               probs = seq(.1, .9, by = .2), names = TRUE)

# generate plot
myplot <- ggplot(data = pwt_sub[!is.na(pwt_sub$output_per_worker), ]) + 
  geom_histogram(aes(x=output_per_worker), bins = 20, color=mygreen, fill=mygreen, alpha = 0.4, boundary = 0) +
  labs(x = expression(paste("GDP per worker ", Y[2019],"/", L[2019], " (in 2017 USD PPP)")),
       y = "Frequency") +
  coord_cartesian(xlim=c(0, 250000), ylim=c(0, 40)) + 
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig25.pdf", width = height*aspect_ratio, height = height)
}

########### 
# Capital per worker across countries
###########

# calculate output per worker
pwt_sub$capital_per_worker <- pwt_sub$rnna/pwt_sub$emp

# calculate deciles of the output-per-worker distribution
qk <- quantile(pwt_sub[!is.na(pwt_sub$capital_per_worker), ]$capital_per_worker, 
               probs = seq(.1, .9, by = .2), names=TRUE)

# generate plot
myplot <- ggplot(data = pwt_sub[!is.na(pwt_sub$capital_per_worker), ]) + 
  geom_histogram(aes(x=capital_per_worker), bins = 20, color=myblue, fill=myblue, alpha = 0.4, boundary = 0) +
  labs(x = expression(paste("Capital per worker ", K[2019],"/", L[2019], " (in 2017 USD PPP)")),
       y = "Frequency") +
  coord_cartesian(xlim=c(0, 1000000), ylim=c(0, 40)) + 
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig26.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Quantiles of the distributions
###########

sprintf("Distribution of GDP per worker:")
print(qo)
sprintf("Distribution of capital per worker:")
print(qk)
sprintf("Percentile ratios : Y/L = %5.3f, K/L = %5.3f", qo["90%"]/qo["10%"], qk["90%"]/qk["10%"])



########### 
# Growth accounting exercise for the US
###########

# load data and extract US data
data("pwt10.0")
pwt_sub <- subset(pwt10.0, isocode=="USA")


# calculate output, capital per worker and capital share
pwt_sub$output_per_worker  <- pwt_sub$rgdpe/pwt_sub$emp
pwt_sub$capital_per_worker <- pwt_sub$rnna/pwt_sub$emp
pwt_sub$capital_share      <- 1 - pwt_sub$labsh

# now calculate growth rates and solow residual
pwt_sub$gy <- lead(pwt_sub$output_per_worker)/pwt_sub$output_per_worker - 1
pwt_sub$gk <- pwt_sub$capital_share*(lead(pwt_sub$capital_per_worker)/pwt_sub$capital_per_worker - 1)
pwt_sub$rt <- pwt_sub$gy - pwt_sub$gk


# now create a plot to show growth and its components
myplot <- ggplot(data = pwt_sub[!is.na(pwt_sub$gy), ]) + 
  geom_ribbon(aes(x=year, ymin=0, ymax=gk*100,    fill= "1gk", color="1gk") , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=gk*100, ymax=(gk+rt)*100, fill= "2rt", color="2rt")  , alpha=0.4) +
  geom_line(aes(x=year, y=gy*100), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(1950, 2020), ylim=c(-3, 5)) + 
  scale_x_continuous(breaks=seq(1950, 2020, 20), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "Growth Decomposition for GDP per Worker (in %)") +
  scale_fill_manual(breaks = c("1gk", "2rt"), name = "", 
                    labels = c("Growth in Capital per Worker", "Solow Residual"),
                    values = c(myblue, mygreen)) +
  scale_color_manual(breaks = c("1gk", "2rt"),
                     values = c(myblue, mygreen)) +
  guides(colour = "none") +
  theme_bw() + 
  theme(legend.position="bottom")

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 4.5;
  ggsave("fig27.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Convergence test 1970 to 2015
###########

# use 1970 and 2015 data for all available countries
data("pwt10.0")
pwt_sub <- subset(pwt10.0, year=="1970" | year=="2015")

# make sure you are in the right order
pwt_sub <- pwt_sub[order(pwt_sub$isocode, pwt_sub$year), ]

# calculate output per capita (not worker due to data availability)
pwt_sub$output_per_capita <- log(pwt_sub$rgdpe/pwt_sub$pop)

# reorganize dataset in wide form
data <- reshape(pwt_sub[c("year", "country", "isocode", "output_per_capita")], 
             idvar = c("country", "isocode"), timevar = "year", direction = "wide")

# drop NA cases
data <- data[!is.na(data$output_per_capita.1970) & !is.na(data$output_per_capita.2015), ]

# calculate growth in output per worker
data$gy <- data$output_per_capita.2015 - data$output_per_capita.1970

# run regression
reg <- lm(gy ~ output_per_capita.1970, data)
summary(reg)

# generate scatter plot
lab  <- paste("b = ", format(round(reg$coefficients[2], 2), nsmall=2), " (", format(round(summary(reg)$coefficients[2, 2], 2), nsmall=2), ")",
              " / R2 = ", format(round(summary(reg)$r.squared, 2), nsmall=2))

myplot <- ggplot(data = data) + 
  geom_hline(yintercept = 0, color="black", linewidth=0.5) + 
  geom_point(aes(x=output_per_capita.1970, y=gy), color="darkblue", fill="darkblue", size=1) +
  geom_smooth(aes(x=output_per_capita.1970, y=gy), method="lm", formula="y ~ x", se=FALSE, color=myred) +
  geom_label(aes(x = 13, y = 3, label = lab), 
             hjust = 1, vjust = 1, label.r = unit(0, "lines"), label.padding = unit(0.35, "lines")) +
  coord_cartesian(xlim=c(6, 13), ylim=c(-1.5, 3)) + 
  labs(x = "Level of output per capita 1970 (in logs)",
       y = "Growth in output per capita 1970-2015") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 3.5;
  ggsave("fig28.pdf", width = height*aspect_ratio, height = height)
}

# now drop Brunei, United Arab Emirates and Qatar
data <- data[data$output_per_capita.1970 < 11, ]


# run regression
reg <- lm(gy ~ output_per_capita.1970, data)
summary(reg)

# generate scatter plot
lab  <- paste("b = ", format(round(reg$coefficients[2], 2), nsmall=2), " (", format(round(summary(reg)$coefficients[2, 2], 2), nsmall=2), ")",
              " / R2 = ", format(round(summary(reg)$r.squared, 2), nsmall=2))

myplot <- ggplot(data = data) + 
  geom_hline(yintercept = 0, color="black", linewidth=0.5) + 
  geom_point(aes(x=output_per_capita.1970, y=gy), color="darkblue", fill="darkblue", size=1) +
  geom_smooth(aes(x=output_per_capita.1970, y=gy), method="lm", formula="y ~ x", se=FALSE, color=myred) +
  geom_label(aes(x = 11, y = 3, label = lab), 
             hjust = 1, vjust = 1, label.r = unit(0, "lines"), label.padding = unit(0.35, "lines")) +
  coord_cartesian(xlim=c(6, 11), ylim=c(-1.5, 3)) + 
  labs(x = "Level of output per capita 1970 (in logs)",
       y = "Growth in output per capita 1970-2015") +
  theme_bw()

# print the plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3.5;
  height <- 3.5;
  ggsave("fig29.pdf", width = height*aspect_ratio, height = height)
}
