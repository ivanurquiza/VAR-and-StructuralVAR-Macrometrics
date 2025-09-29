#------------------------------------------------------------------------------#
# Maestría en Economía
# Macroeconometrìa
# 2025, 2do trimestre 
# Profesor: Javier Garcia-Cicco
# Tutor: Franco Nuñez

#------------------------------------------------------------------------------#
remove(list = ls(all.names = TRUE))
gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(vars)


# Data ####

source("PS2_Data.R")

Yl.f <- cbind(pcom, er, pc)
Yl.f <- log(Yl.f) # log transformation
Yd.f <- 100 * diff(Yl.f) # log-diff transformation

Yl <- window(Yl.f, start = c(2004, 01), end = c(2019, 12))
Yd <- window(Yd.f, start = c(2004, 01), end = c(2019, 12))

# VAR Estimation (Reduced Form) ####

library(vars)

Y <- Yd

# Lag Order Selection
pmax <- 12 # Maximum lag order

popt <- VARselect(Y, lag.max = pmax, type = "const")
popt
p <- popt$selection[2] # HQIC (For log-levels series, see Kilian & Lutkepohl, pp. 373), p > 1 neccesary

# Estimation
Y <- Y[(pmax - p + 1):nrow(Y), ] # Starting in Jan-05

VAR <- VAR(Y, p = p, type = "const")

m <- VAR$K # Number of variables in the VAR
T <- VAR$obs # Number of effective sample observations, excluding "p" starting values

# Ad hoc Function
matC <- function(m, p, vx) {
  vy <- setdiff(1:m, vx)
  Cm <- matrix(1, m, m * p + 1)
  for (i in vx) {
    for (l in 1:p) {
      for (j in vy) {
        Cm[i, m * (l - 1) + j] <- 0
      }
    }
  }
  Cm
}

# Simplification (no feedback from local variables to PCOM)
constraints <- matC(m, p, 1)
VAR <- restrict(VAR, method = "man", resmat = constraints)
VAR

# Model Checking
roots(VAR, modulus = TRUE)

h.BG <- 6
serial.test(VAR, lags.bg = h.BG, type = "ES")




# VAR Estimation (Structural Form) ####

# A Matrix
Amat <- function(m){
  
  Amat <<- diag(m)
  for (i in 2:m) {
    for (j in 1:(i - 1)) {
      Amat[i, j] <- NA
    }
  }
  return(Amat)
}

# B Matrix
Bmat <- function(m){
  Bmat <<- matrix(0, m, m)
  for (i in 1:m) {
    Bmat[i, i] <- NA
  }
  return(Bmat)
}

Amat <-Amat(m)
Bmat <-Bmat(m)

# SVAR estimation (AB model configuration)
SVAR <- SVAR(VAR, Amat = Amat, Bmat = Bmat, lrtest = FALSE, max.iter = 1000)
SVAR

# Structural IRF
irf_one <- irf(SVAR, response = "er", impulse = "pcom", 
               n.ahead = 10, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(irf_one)

#Forecast Error Variance Decomposition
SVARfevd <- fevd(SVAR, n.ahead = 20)
#SVARfevd
plot(SVARfevd)

# For other identification strategies and other tools #####
# (Such as Historical Decomposition)
#install.packages("svars")
#library(svars)
# https://cran.r-project.org/web/packages/svars/vignettes/svars.pdf


