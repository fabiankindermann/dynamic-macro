#######################################
### Plotting some graphs in our Dynamic Programming Chapter
#######################################

# Clear the workspace and load libraries
rm(list = ls())
library(reshape2)
library(base)
library(ggplot2)
library(grid)
library(stringr)
library(tidyverse)

# should graphs be exported to pdf
export_pdf = FALSE


########### 
# Parameterize the problem
########### 

# curvature of utility function
gamma = 0.5

# time discount factor
beta = 0.95

# initial quantity of the resource
a0 = 100



########### 
# Time path for the all-in-one solution
########### 

# simulate path for 200 periods
t = c(0:200)

# generate data frame for plotting
dat <- data.frame(
  t = t,
  c = beta^(t*gamma)*(1-beta^gamma)*a0
)

# now generate time path plot
myplot <- ggplot(data=dat) + 
  geom_line(aes(x=t, y=c), color="darkblue", linewidth=1) +
  scale_x_continuous(breaks=seq(0, 200, 20)) +
  labs(x = expression(paste("Time ", t)),
       y = expression(paste("Consumption path ", c[t]))) +
  theme_bw()

# print plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig7.pdf", width = height*aspect_ratio, height = height)
}
  

########### 
# Now plot the policy and function
########### 

# generate discrete set for potential resources
a = seq(0, a0, 0.01)

# generate data frame for plotting
dat <- data.frame(
  a = a,
  c = a*(1-beta^gamma),
  V = (1-beta^gamma)^(-1/gamma)*a^(1-1/gamma)/(1-1/gamma)
)

# plot for the policy function
myplot <- ggplot(data=dat) + 
  geom_line(aes(x=a, y=c), color="darkblue", linewidth=1) +
  scale_x_continuous(breaks=seq(0, a0, 20)) +
  labs(x = expression(paste("Resource ", a[t])),
       y = expression(paste("Policy function ", c(a[t])))) +
  theme_bw()

# print plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig9.pdf", width = height*aspect_ratio, height = height)
}


# plot for the value function
myplot <- ggplot(data=dat) + 
  geom_line(aes(x=a, y=V), color="darkred", linewidth=1) +
  coord_cartesian(ylim=c(-1600, 0)) + 
  scale_x_continuous(breaks=seq(0, a0, 20)) +
  labs(x = expression(paste("Resource ", a[t])),
       y = expression(paste("Value function ", V(a[t])))) +
  theme_bw()

# print plot
print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig10.pdf", width = height*aspect_ratio, height = height)
}




########### 
# Calculates the optimal policy at state a
#     a     : the point in the state space where we are at
#     gamma : curvature of utility function
#     beta  : time discount factor
###########
policy <- function(a, gamma, beta) {
  
  # calculate the optimal policy
  c = (1-beta^gamma)*a
  
  # return the value
  return(c)
}

########### 
# Use the policy function to simulate the system forward
###########

# number of periods to simulate
T = 200

# start with the initial value of the resource
a_t = a0

# calculate the policy at this point
c_t = policy(a_t[1], gamma, beta)

# now simulate forward until date T
for(t in 2:(T+1)) {
  a_t[t] = a_t[t-1] - c_t[t-1]
  c_t[t] = policy(a_t[t], gamma, beta)
}

# the time periods
t = c(0:200)

# generate data frame for plotting
dat <- data.frame(
  t = t,
  c = c_t,
  a = a_t
)

# now generate time path plot of consumption
myplot <- ggplot(data=dat) + 
  geom_line(aes(x=t, y=c), color="darkblue", linewidth=1) +
  scale_x_continuous(breaks=seq(0, 200, 20)) +
  labs(x = expression(paste("Time ", t)),
       y = expression(paste("Consumption path ", c[t]))) +
  theme_bw()

print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig11.pdf", width = height*aspect_ratio, height = height)
}


# now generate time path plot of the resource
myplot <- ggplot(data=dat) + 
  geom_line(aes(x=t, y=a), color="darkred", linewidth=1) +
  scale_x_continuous(breaks=seq(0, 200, 20)) +
  labs(x = expression(paste("Time ", t)),
       y = expression(paste("Resource level ", a[t]))) +
  theme_bw()

print(myplot)

# save plot to pdf file (if needed)
if(export_pdf) {
  aspect_ratio <- 4/3;
  height <- 3.5;
  ggsave("fig12.pdf", width = height*aspect_ratio, height = height)
}
