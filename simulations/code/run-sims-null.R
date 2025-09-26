rm(list = ls())

library('data.table')
library('foreach')
library('doRNG')
library('doParallel')

source('sim-utils.R')
source('ipw-function.R')

################################################################################
## Set Parameters
################################################################################

n <- 1000
time_points <- 24
n_lags <- 1

beta_L <- c(0, 0.5, 0.25, 0.5, 0)
beta_Z <- c(0.5, -1)
beta_A <- c(0.5, 0.5, -0.25, 0.5, 0)
beta_R <- c(-1, 0.5, 0.25, -0.5)
beta_Y <- c(0, 0, 0, 0, 2, 0, 0, 0, 0)
sigma_Y <- 5
U_lb <- -1; U_ub <- 1
m <- 2

# Optional, for coverage
coverage <- TRUE
n_boot <- 250
conf_level <- 0.95

n_cores <- 10
if (n_cores > 1){
  registerDoParallel(cores=n_cores)
}

outcome_times <- c(6, 12, 18, 24)

################################################################################
## Get Estimates
################################################################################

set.seed(1234)
n_reps <- 1000

A_model <- A ~ L + Z
R_model_numerator <- R ~ L0 + Z
R_model_denominator <- R ~ L + A + Z
Y_model_pooled <- Y ~ L0 * (time + Z)
Y_model_nonpooled <- Y ~ L0 * Z

source('sim-runner.R')

save.image('../results/res-null.RData')
