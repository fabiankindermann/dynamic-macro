#######################################
### Fiscal Policy in the Ramsey model
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

# should graphs be exported to pdf
export_pdf <- FALSE

# define some colors
mygreen <- "#00BA38"
myblue  <- "#619CFF"
myred   <- "#F8766D"


########### 
# Ramsey Model setup
###########

# data moments
lab_share = 0.381
inv_share = 0.245
gross_ret = 0.114

# parameter choice
n     <- 0.0122
h     <- 0.0169
g     <- (1+n)*(1+h)-1
alpha <- lab_share
delta <- inv_share*gross_ret/alpha - g
beta  <- (1+g)/(1 + gross_ret - delta)
gamma <- 0.5

# steady state capital intensity under old beta
k_old <- (alpha/((1+g)/beta - 1 + delta))^(1/(1-alpha))
c_old <- k_old^alpha - (g+delta)*k_old


########### 
# Function to guess initial consumption of the Ramsey model
###########
solve_ramsey <- function(k0, amount, policy, g, alpha, delta, gamma, beta) {
  
  # set numerical parameters
  T <- 10000          # number of simulation periods
  itermax <- 1000     # maximum number of iterations
  tol <- 1e-6         # tolerance level for difference to steady state
  
  # determine path of g_t
  g_t <- 0
  tau_k <- 0
  if(policy == 1) {
    g_t[1:T] = amount
    tau_k[1:T] = 0
  } else if (policy == 2) {
    g_t[1:T] = 0
    g_t[1] = amount
    tau_k[1:T] = 0
  } else if (policy == 3) {
    g_t[1:T] = 0
    tau_k[1:T] = amount
  }
  
  # determine steady state capital intensity and consumption
  kstar <- (alpha/(((1+g)/beta - 1)/(1-tau_k[length(tau_k)]) + delta))^(1/(1-alpha))
  cstar <- kstar**alpha - (g+delta)*kstar - g_t[length(g_t)]
  
  # set starting interval depending on k0
  if(k0 <= kstar) {
    c_min <- 0
    c_max <- cstar
  } else {
    c_min <- cstar
    c_max <- k0^alpha + (1-delta)*k0
  }
  
  # start iteration process
  for (i in 1:itermax) {
    
    # start a capital and consumption path
    k <- k0
    c <- (c_min+c_max)/2
    
    # now simulate the economy forward
    for (t in 1:T) {
      
      # calculate difference to steady state
      epsilon <- sqrt((c[t]/cstar-1)^2 + (k[t]/kstar-1)^2)
      
      # if difference is small enough, that's it, we return
      if(epsilon < tol) {
        return(c(c[1], epsilon, i))
      }
      
      # determine next period's capital stock and consumption
      k[t+1] <- max((k[t]**alpha - c[t] - g_t[t] + (1-delta)*k[t])/(1+g), 1e-4)
      c[t+1] <- (beta*(1 + (1-tau_k[t])*(alpha*k[t+1]^(alpha-1) - delta))/(1+g))^gamma*c[t]
      
      #  decision rule for capital
      if(k0 <= kstar & k[t+1] > kstar | k0 > kstar & c[t+1] < cstar) {
        c_min <- c[1]; break;
      }
      
      #  if consumption is too large, then initial consumption level was too small
      if(k0 <= kstar & c[t+1] > cstar | k0 > kstar & k[t+1] < kstar) {
        c_max <- c[1]; break;
      }
    }
  }
  
  # in any case, if you end up here, return c[1] and epsilon
  return(c(c[1], epsilon, i))
}


########### 
# Function to simulate Ramsey model forward
###########
ramsey <- function(T0, T1, c0, k0, g_t, tau_k, g, alpha, delta, gamma, beta) {
  
  # set numerical parameters
  tol <- 1e-6
  
  # determine steady state capital intensity and consumption
  kstar <- (alpha/(((1+g)/beta - 1)/(1-tau_k[length(tau_k)]) + delta))^(1/(1-alpha))
  cstar <- kstar**alpha - (g+delta)*kstar - g_t[length(g_t)]
    
  # start a capital and consumption path
  k <- k0
  c <- 0
  
  # assume economy was in steady state prior to date 0
  k[ind(T0):ind(0)] <- k0
  c[ind(T0):ind(0)] <- k0^alpha - (g+delta)*k0 - g_t[ind(T0):ind(0)]
  
  # then start to simulate new economy path
  c[ind(0)] <- c0
  
  # now simulate the economy forward
  for (t in ind(0):ind(T1-1)) {
    
    # determine next period's capital stock and consumption
    k[t+1] <- max((k[t]**alpha - c[t] - g_t[t] + (1-delta)*k[t])/(1+g), 1e-4)
    c[t+1] <- (beta*(1 + (1-tau_k[t])*(alpha*k[t+1]^(alpha-1) - delta))/(1+g))^gamma*c[t]
    
    #  move to steady state level when you are almost there
    epsilon <- sqrt((c[t+1]/cstar-1)^2 + (k[t+1]/kstar-1)^2)
    if(epsilon < tol) {
      k[(t+1):ind(T1)] = kstar
      c[(t+1):ind(T1)] = cstar
      break
    }
  }
  
  # calculate other macro statistics
  y <- k^alpha
  ir <- (g + delta)*k
  i <- y - c - g_t
  s <- i/y

  # return a data frame with macro path
  res <- data.frame(year=c(T0:T1), k, y, c, g_t, i, ir, s)
  return(res)
}

# indicator management function
ind <- function(t) {
  return(t + 1 + abs(T0))
}


########### 
# Simulate permanent change in government spending
###########

# steady state capital intensity without government spending
k_old <- (alpha/((1+g)/beta - 1 + delta))^(1/(1-alpha))
c_old <- k_old^alpha - (g+delta)*k_old

# determine size of government spending (20% of old GDP)
gov_spend <- 0.2*k_old^alpha

# set simulation periods
T0 <- -50
T1 <- 200

# solve transition path
res <- solve_ramsey(k_old, gov_spend, 1, g, alpha, delta, gamma, beta)

# get path of government spending
g_t <- 0
g_t[ind(T0):ind(-1)] = 0
g_t[ind(0):ind(T1)] = gov_spend

# get path of capital tax (not used here)
tau_k <- 0
tau_k[ind(T0):ind(T1)] = 0

# simulate the model for 250 periods
transition <- ramsey(T0, T1, res[1], k_old, g_t, tau_k, g, alpha, delta, gamma, beta)


# Plot dynamics of the capital stock
myplot <- ggplot(data = transition) + 
  geom_hline(yintercept=transition$k[ind(T0)], color=myred, linetype="dashed", linewidth=1) + 
  geom_hline(yintercept=transition$k[ind(T1)], color=mygreen, linetype="dashed", linewidth=1) + 
  geom_line(aes(x=year, y=k), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(T0, T1), ylim=c(6, 8)) + 
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
  ggsave("fig12.pdf", width = height*aspect_ratio, height = height)
}


# Plot GDP and its components
myplot <- ggplot(data = transition) + 
  geom_hline(yintercept = c_old, color=mygreen, linetype="dashed", linewidth=0.5) + 
  geom_ribbon(aes(x=year, ymin=0, ymax=c,    fill= "1c", color="1c") , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c, ymax=c+g_t, fill= "3di", color="3di"), alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c+g_t, ymax=y, fill= "2ir", color="2ir")  , alpha=0.4) +
  geom_line(aes(x=year, y=y), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(T0, T1), ylim=c(0, 3)) + 
  scale_x_continuous(breaks=seq(T0, T1, 25), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "GDP and its components per\n effective unit of labor") +
  scale_fill_manual(breaks = c("1c", "2ir", "3di"), name = "", 
                    labels = c("Consumption", "Investment", "Gov. Spending"),
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
  ggsave("fig13.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Simulate temporary change in government spending
###########

# steady state capital intensity without government spending
k_old <- (alpha/((1+g)/beta - 1 + delta))^(1/(1-alpha))
c_old <- k_old^alpha - (g+delta)*k_old

# determine size of government spending (20% of old GDP)
gov_spend <- 0.2*k_old^alpha

# set simulation periods
T0 <- -50
T1 <- 200

# solve transition path
res <- solve_ramsey(k_old, gov_spend, 2, g, alpha, delta, gamma, beta)

# get path of government spending
g_t <- 0
g_t[ind(T0):ind(T1)] = 0
g_t[ind(0)] = gov_spend

# get path of capital tax (not used here)
tau_k <- 0
tau_k[ind(T0):ind(T1)] = 0

# simulate the model for 250 periods
transition <- ramsey(T0, T1, res[1], k_old, g_t, tau_k, g, alpha, delta, gamma, beta)


# Plot dynamics of the capital stock
myplot <- ggplot(data = transition) + 
  geom_hline(yintercept=transition$k[ind(T0)], color=myred, linetype="dashed", linewidth=1) + 
  geom_hline(yintercept=transition$k[ind(T1)], color=mygreen, linetype="dashed", linewidth=1) + 
  geom_line(aes(x=year, y=k), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(T0, T1), ylim=c(6, 8)) + 
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
  ggsave("fig14.pdf", width = height*aspect_ratio, height = height)
}


# Plot GDP and its components
myplot <- ggplot(data = transition) + 
  geom_hline(yintercept = c_old, color=mygreen, linetype="dashed", linewidth=0.5) + 
  geom_ribbon(aes(x=year, ymin=0, ymax=c,    fill= "1c", color="1c") , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c, ymax=c+g_t, fill= "3di", color="3di"), alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c+g_t, ymax=y, fill= "2ir", color="2ir")  , alpha=0.4) +
  geom_line(aes(x=year, y=y), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(T0, T1), ylim=c(0, 3)) + 
  scale_x_continuous(breaks=seq(T0, T1, 25), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "GDP and its components per\n effective unit of labor") +
  scale_fill_manual(breaks = c("1c", "2ir", "3di"), name = "", 
                    labels = c("Consumption", "Investment", "Gov. Spending"),
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
  ggsave("fig15.pdf", width = height*aspect_ratio, height = height)
}


# Plot GDP and its components (zoom in)
myplot <- ggplot(data = transition) + 
  geom_hline(yintercept = c_old, color=mygreen, linetype="dashed", linewidth=0.5) + 
  geom_ribbon(aes(x=year, ymin=0, ymax=c,    fill= "1c", color="1c") , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c, ymax=c+g_t, fill= "3di", color="3di"), alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c+g_t, ymax=y, fill= "2ir", color="2ir")  , alpha=0.4) +
  geom_line(aes(x=year, y=y), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(-10, 50), ylim=c(1.5, 2.25)) + 
  scale_x_continuous(breaks=seq(T0, T1, 25), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "GDP and its components per\n effective unit of labor") +
  scale_fill_manual(breaks = c("1c", "2ir", "3di"), name = "", 
                    labels = c("Consumption", "Investment", "Gov. Spending"),
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
  ggsave("fig16.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Simulate capital tax/subsidy
###########

# steady state capital intensity without capital tax
k_old <- (alpha/((1+g)/beta - 1 + delta))^(1/(1-alpha))
c_old <- k_old^alpha - (g+delta)*k_old

# determine size of capital tax rate (2.5%)
tax_rate <- 0.25

# set simulation periods
T0 <- -50
T1 <- 200

# solve transition path
res <- solve_ramsey(k_old, tax_rate, 3, g, alpha, delta, gamma, beta)

# get path of government spending (not used)
g_t <- 0
g_t[ind(T0):ind(T1)] = 0

# get path of capital tax
tau_k <- 0
tau_k[ind(T0):ind(-1)] = 0
tau_k[ind(0):ind(T1)] = tax_rate

# simulate the model for 250 periods
transition <- ramsey(T0, T1, res[1], k_old, g_t, tau_k, g, alpha, delta, gamma, beta)

# calculate welfare number for initial equilibrium
Welfare_0 <- 0
for(t in 0:T1) {
  Welfare_0 <- Welfare_0 + beta^t*(transition$c[ind(T0)]^(1-1/gamma)/(1-1/gamma))
}

# calculate welfare number for raising capital tax
Welfare_1 <- 0
for(t in 0:T1) {
  Welfare_1 <- Welfare_1 + beta^t*(transition$c[ind(t)]^(1-1/gamma)/(1-1/gamma))
}


# Plot dynamics of the capital stock
myplot <- ggplot(data = transition) + 
  geom_hline(yintercept=transition$k[ind(T0)], color=myred, linetype="dashed", linewidth=1) + 
  geom_hline(yintercept=transition$k[ind(T1)], color=mygreen, linetype="dashed", linewidth=1) + 
  geom_line(aes(x=year, y=k), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(T0, T1), ylim=c(4, 8)) + 
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
  ggsave("fig17.pdf", width = height*aspect_ratio, height = height)
}


# Plot GDP and its components
myplot <- ggplot(data = transition) + 
  geom_hline(yintercept = c_old, color=mygreen, linetype="dashed", linewidth=0.5) + 
  geom_ribbon(aes(x=year, ymin=0, ymax=c,    fill= "1c", color="1c") , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c, ymax=y, fill= "2ir", color="2ir")  , alpha=0.4) +
  geom_line(aes(x=year, y=y), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(T0, T1), ylim=c(0, 3)) + 
  scale_x_continuous(breaks=seq(T0, T1, 25), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "GDP and its components per\n effective unit of labor") +
  scale_fill_manual(breaks = c("1c", "2ir"), name = "", 
                    labels = c("Consumption", "Investment"),
                    values = c(mygreen, myblue)) +
  scale_color_manual(breaks = c("1c", "2ir"),
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
  ggsave("fig18.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Simulate capital subsidy
###########

# steady state capital intensity without capital tax
k_old <- (alpha/((1+g)/beta - 1 + delta))^(1/(1-alpha))
c_old <- k_old^alpha - (g+delta)*k_old

# determine size of capital tax rate (2.5%)
tax_rate <- -0.25

# set simulation periods
T0 <- -50
T1 <- 200

# solve transition path
res <- solve_ramsey(k_old, tax_rate, 3, g, alpha, delta, gamma, beta)

# get path of government spending (not used)
g_t <- 0
g_t[ind(T0):ind(T1)] = 0

# get path of capital tax
tau_k <- 0
tau_k[ind(T0):ind(-1)] = 0
tau_k[ind(0):ind(T1)] = tax_rate

# simulate the model for 250 periods
transition <- ramsey(T0, T1, res[1], k_old, g_t, tau_k, g, alpha, delta, gamma, beta)

# calculate welfare number for raising capital subsidy
Welfare_2 <- 0
for(t in 0:T1) {
  Welfare_2 <- Welfare_2 + beta^t*transition$c[ind(t)]^(1-1/gamma)/(1-1/gamma)
}


# Plot dynamics of the capital stock
myplot <- ggplot(data = transition) + 
  geom_hline(yintercept=transition$k[ind(T0)], color=myred, linetype="dashed", linewidth=1) + 
  geom_hline(yintercept=transition$k[ind(T1)], color=mygreen, linetype="dashed", linewidth=1) + 
  geom_line(aes(x=year, y=k), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(T0, T1), ylim=c(6, 12)) + 
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
  ggsave("fig19.pdf", width = height*aspect_ratio, height = height)
}


# Plot GDP and its components
myplot <- ggplot(data = transition) + 
  geom_hline(yintercept = c_old, color=mygreen, linetype="dashed", linewidth=0.5) + 
  geom_ribbon(aes(x=year, ymin=0, ymax=c,    fill= "1c", color="1c") , alpha=0.4) +
  geom_ribbon(aes(x=year, ymin=c, ymax=y, fill= "2ir", color="2ir")  , alpha=0.4) +
  geom_line(aes(x=year, y=y), color="darkblue", linewidth=1) +
  coord_cartesian(xlim=c(T0, T1), ylim=c(0, 3)) + 
  scale_x_continuous(breaks=seq(T0, T1, 25), expand=c(0, 0)) +
  labs(x = "Year t",
       y = "GDP and its components per\n effective unit of labor") +
  scale_fill_manual(breaks = c("1c", "2ir"), name = "", 
                    labels = c("Consumption", "Investment"),
                    values = c(mygreen, myblue)) +
  scale_color_manual(breaks = c("1c", "2ir"),
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
  ggsave("fig20.pdf", width = height*aspect_ratio, height = height)
}


########### 
# Welfare comparison
###########

sprintf('Without capital taxes    : %12.6f %12.6f\n', Welfare_0, 0)
sprintf('With 25p capital tax     : %12.6f %12.6f\n', Welfare_1, ((Welfare_1/Welfare_0)^{1/(1-1/gamma)}-1)*100)
sprintf('With 25p capital subsidy : %12.6f %12.6f\n', Welfare_2, ((Welfare_2/Welfare_0)^{1/(1-1/gamma)}-1)*100)
