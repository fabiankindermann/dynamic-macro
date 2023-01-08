#######################################
### Model Linearization
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
library(pracma)

# should graphs be exported to pdf
export_pdf <- TRUE

# define some colors
mygreen <- "#00BA38"
myblue  <- "#619CFF"
myred   <- "#F8766D"


########### 
# Load model parameters and steady state values
###########

# get parameters stored in the RData set
load("chap07_parameters.RData")


########### 
# Log-linear coefficients based on guess of kappa_LK, kappa_LA and kappa_LG
###########

coefficients <- function(x) {
  
  kappa <- list()
  
  # copy coefficients
  kappa$LK <- x[1]
  kappa$LA <- x[2]
  kappa$LG <- x[3]
  
  # calculate output coefficients
  kappa$YK <- alpha + (1-alpha)*kappa$LK
  kappa$YA <- (1-alpha)*(1+kappa$LA)
  kappa$YG <- (1-alpha)*kappa$LG
  
  # calculate consumption coefficients
  kappa$CK <- kappa$YK - kappa$LK/(1-L)
  kappa$CA <- kappa$YA - kappa$LA/(1-L)
  kappa$CG <- kappa$YG - kappa$LG/(1-L)
  
  # calculate tomorrow's capital coefficients
  kappa$KK <- 1/KY*kappa$YK - CY/KY*kappa$CK + 1 - delta
  kappa$KA <- 1/KY*kappa$YA - CY/KY*kappa$CA
  kappa$KG <- 1/KY*kappa$YG - CY/KY*kappa$CG - GY/KY
  
  # calculate investment coefficients
  kappa$IK <- KY/IY*(kappa$KK - (1-delta))
  kappa$IA <- KY/IY*kappa$KA
  kappa$IG <- KY/IY*kappa$KG
  
  # calculate interest rate coefficients
  kappa$rK <- (r+delta)/r*(kappa$YK-1)
  kappa$rA <- (r+delta)/r*kappa$YA
  kappa$rG <- (r+delta)/r*kappa$YG

  # calculate wage rate coefficients
  kappa$wK <- kappa$YK - kappa$LK
  kappa$wA <- kappa$YA - kappa$LA
  kappa$wG <- kappa$YG - kappa$LG
  
  # return list of kappas
  return(kappa)
    
}


########### 
# Function to determine equation system to solve for kappa_LK, kappa_LA and kappa_LG
###########

equation_system <- function(x) {

  # determine coefficients
  kappa <- coefficients(x)
  
  # determine residual equations
  eqs <- array(dim = 3)
  eqs[1] <- kappa$CK - (kappa$CK - (r+delta)/(1+r)*(kappa$YK-1))*kappa$KK
  eqs[2] <- kappa$CA - (kappa$CK - (r+delta)/(1+r)*(kappa$YK-1))*kappa$KA - (kappa$CA - (r+delta)/(1+r)*kappa$YA)*rho_A
  eqs[3] <- kappa$CG - (kappa$CK - (r+delta)/(1+r)*(kappa$YK-1))*kappa$KG - (kappa$CG - (r+delta)/(1+r)*kappa$YG)*rho_G
  
  return(eqs)
}


########### 
# Determine the correct values of kappa_LK, kappa_LA and kappa_LG
###########

# use fsolve from the pracma package to calculate the solution
fret <- fsolve(equation_system, c(0, 0, 0), J=NULL, tol=1e-10)

# calculate all the kappas
kappa <- coefficients(fret$x)

# generate A matrix
A = matrix(nrow=3, ncol=3)
A[1, ] <- c(kappa$KK, kappa$KA, kappa$KG)
A[2, ] <- c(       0,    rho_A,        0)
A[3, ] <- c(       0,        0,    rho_G)

# generate B matrix
B = matrix(nrow=6, ncol=3)
B[1, ] <- c(kappa$YK, kappa$YA, kappa$YG)
B[2, ] <- c(kappa$CK, kappa$CA, kappa$CG)
B[3, ] <- c(kappa$IK, kappa$IA, kappa$IG)
B[4, ] <- c(kappa$rK, kappa$rA, kappa$rG)
B[5, ] <- c(kappa$wK, kappa$wA, kappa$wG)
B[6, ] <- c(kappa$LK, kappa$LA, kappa$LG)


# save linearized model coefficients
save(file="chap07_coefficients.RData", 
     list=c("A", "B"))