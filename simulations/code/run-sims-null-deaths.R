rm(list = ls())

library('data.table')
library('foreach')
library('doRNG')
library('doParallel')

source('helper-functions-deaths.R')
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
beta_D <- c(-3, 1)
sigma_Y <- 5
U_lb <- -1; U_ub <- 1
m <- 2

# Optional, for coverage
coverage <- TRUE
n_boot <- 250
conf_level <- 0.95

n_cores <- 20
if (n_cores > 1){
  registerDoParallel(cores=n_cores)
}

outcome_times <- c(6, 12, 18, 24)

################################################################################
## Get True Values
################################################################################

set.seed(1234)
n_reps_true <- 10000

results_true <- foreach(rep = 1:n_reps_true) %dorng% {
  df_z0 <- lapply(as.list(1:n), FUN=function(ind){
    datagen_true(i = ind, time_points = time_points,
                 beta_L = beta_L, Z = 0, beta_A = beta_A, beta_Y = beta_Y, beta_D = beta_D,
                 sigma_Y = sigma_Y, U_lb = U_lb, U_ub = U_ub, m = m, 
                 include_deaths = TRUE)
  })
  df_z0 <- rbindlist(df_z0)
  
  df_z1 <- lapply(as.list(1:n), FUN=function(ind){
    datagen_true(i = ind, time_points = time_points,
                 beta_L = beta_L, Z = 1, beta_A = beta_A, beta_Y = beta_Y, beta_D = beta_D,
                 sigma_Y = sigma_Y, U_lb = U_lb, U_ub = U_ub, m = m, 
                 include_deaths = TRUE)
  })
  df_z1 <- rbindlist(df_z1)
  
  true_z0 <- true_z1 <- rep(NA, times = length(outcome_times))
  for (i in 1:length(outcome_times)){
    true_z0[i] <- mean(df_z0[df_z0$time == outcome_times[i] & df_z0$D == 0, ]$Y)
    true_z1[i] <- mean(df_z1[df_z1$time == outcome_times[i] & df_z1$D == 0, ]$Y)
  }
  return(list(true_z0 = true_z0, true_z1 = true_z1))
}

true_z0 <- true_z1 <- matrix(NA, nrow = n_reps_true, ncol = length(outcome_times))
for (rep in 1:n_reps_true) {
  true_z0[rep, ] <- results_true[[rep]]$true_z0
  true_z1[rep, ] <- results_true[[rep]]$true_z1
}

truth_z0 <- colMeans(true_z0)
truth_z1 <- colMeans(true_z1)
truth_dif <- truth_z0 - truth_z1

print('Finished with getting true values')

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

source('run-sims-deaths.R')

save.image('../results/res-null-deaths.RData')
