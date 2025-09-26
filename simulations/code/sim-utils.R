expit <- function(x) {
  return(exp(x) / (1 + exp(x)))
}

datagen <- function(i, time_points, beta_L, beta_Z, beta_A, beta_R, beta_Y, beta_D, sigma_Y, U_lb, U_ub, m, 
                    include_deaths = FALSE){
  # Preallocate space in vectors
  time <- 0:time_points
  id <- rep(as.numeric(i), time_points+1)
  L <- rep(NA, time_points+1)
  L0 <- rep(NA, time_points+1)
  Z <- rep(NA, time_points+1)
  A <- rep(NA, time_points+1)
  lag1_A <- rep(NA, time_points+1)
  D <- rep(0, time_points+1)
  R <- rep(NA, time_points+1)
  Y <- rep(NA, time_points+1)
  C_artificial <- rep(NA, time_points+1)
  A_model_eligible <- rep(0, time_points+1)
  G <- rep(NA, time_points+1)
  U <- rep(NA, time_points+1)
  death_time <- NA

  # Baseline
  U[1:(time_points + 1)] <- runif(1, min = U_lb, max = U_ub)
  L[1] <- rbinom(1, 1, expit(beta_L[1] + beta_L[4] * U[1]))
  L0[1:(time_points + 1)] <- L[1]
  Z[1:(time_points + 1)] <- rbinom(1, 1, expit(beta_Z[1] + beta_Z[2] * L[1]))
  A[1] <- 1
  lag1_A[1] <- 0
  C_artificial[1] <- ifelse(A[1] == 0, 1, 0)
  count_not_adhered <- 0
  G[1] <- 0
  A_model_eligible[1] <- 0
  if (include_deaths){
    D[1] <- rbinom(1, 1, expit(beta_D[1] + beta_D[2] * U[1]))
  }
  if (D[1] == 1){
    death_time <- 0
  } else {
    R[1] <- rbinom(1, 1, expit(beta_R[1] + beta_R[2] * A[1] + beta_R[3] * Z[1] + beta_R[4] * L[1]))
    if (R[1] == 1){
      Y[1] <- rnorm(1, beta_Y[1] + beta_Y[2] * A[1] + beta_Y[3] * Z[1] + beta_Y[4] * L[1] + beta_Y[5] * U[1] +
                      beta_Y[6] * Z[1] * A[1] + beta_Y[7] * Z[1] * L[1] + beta_Y[8] * L[1] * A[1], sigma_Y)
    }
    
    # Follow-up times
    for (j in 2:(time_points + 1)){
      L[j] <- rbinom(1, 1, expit(beta_L[1] + beta_L[2] * A[j-1] + beta_L[3] * Z[1] + beta_L[4] * U[1] + beta_L[5] * (j - 1)))
      A[j] <- rbinom(1, 1, expit(beta_A[1] + beta_A[2] * A[j-1] +  beta_A[3] * Z[1] + beta_A[4] * L[j] + beta_A[5] * (j - 1)))
      count_not_adhered <- (count_not_adhered + 1) * (1 - A[j-1])
      G[j] <- ifelse(count_not_adhered == m, 1, 0)
      A_model_eligible[j] <- G[j] & (sum(C_artificial[1:(j-1)]) == 0)
      if ((A[j] == 0 & G[j] == 1) | C_artificial[j-1] == 1){
        C_artificial[j] <- 1
      } else {
        C_artificial[j] <- 0
      }
      lag1_A[j] <- A[j-1]
      if (include_deaths){
        D[j] <- rbinom(1, 1, expit(beta_D[1] + beta_D[2] * U[1]))
        if (D[j] == 1){
          death_time <- j - 1
          break
        }
      }
      R[j] <- rbinom(1, 1, expit(beta_R[1] + beta_R[2] * A[j] + beta_R[3] * Z[1] + beta_R[4] * L[j]))
      if (R[j] == 1){
        Y[j] <- rnorm(1, beta_Y[1] + beta_Y[2] * A[j] + beta_Y[3] * Z[1] + beta_Y[4] * L[j] + beta_Y[5] * U[1] +
                        beta_Y[6] * Z[1] * A[j] - beta_Y[7] * Z[1] * L[j] + beta_Y[8] * L[j] * A[j] + beta_Y[9] * (j - 1), sigma_Y)
      }
    }
  }

  # Consolidate data in a single data frame
  temp_data <- data.table(id = id, time = time, L = L, L0 = L0, Z = Z,
                          A = A, lag1_A = lag1_A, G = G, U = U,
                          C_artificial = C_artificial, R = R, Y = Y,
                          A_model_eligible = A_model_eligible, 
                          D = D)
  
  # If we simulated death, truncate the simulated data set to the death time
  if (!is.na(death_time)){
    temp_data <- temp_data[temp_data$time <= death_time, ]
  }
  if (!include_deaths){
    temp_data$D <- NULL
  }
  return(temp_data)
}


datagen_true <- function(i, time_points, beta_L, Z, beta_A, beta_Y, beta_D, sigma_Y, U_lb, U_ub, m, 
                         include_deaths = FALSE){
  # Preallocate space in vectors
  time <- 0:time_points
  id <- rep(as.numeric(i), time_points+1)
  L <- rep(NA, time_points+1)
  L0 <- rep(NA, time_points+1)
  Z <- rep(Z, time_points+1)
  A <- rep(NA, time_points+1)
  lag1_A <- rep(NA, time_points+1)
  D <- rep(0, time_points+1)
  R <- rep(NA, time_points+1)
  Y <- rep(NA, time_points+1)
  C_artificial <- rep(NA, time_points+1)
  G <- rep(NA, time_points+1)
  U <- rep(NA, time_points+1)
  death_time <- NA

  # Baseline
  U[1:(time_points + 1)] <- runif(1, min = U_lb, max = U_ub)
  L[1] <- rbinom(1, 1, expit(beta_L[1] + beta_L[4] * U[1]))
  L0[1:(time_points + 1)] <- L[1]
  Z[1:(time_points + 1)] <- Z
  A[1] <- 1
  lag1_A[1] <- 0
  count_not_adhered <- 0
  G[1] <- 0
  if (include_deaths){
    D[1] <- rbinom(1, 1, expit(beta_D[1] + beta_D[2] * U[1]))
  }
  if (D[1] == 1){
    death_time <- 0
  } else {
    R[1] <- 1
    if (R[1] == 1){
      Y[1] <- rnorm(1, beta_Y[1] + beta_Y[2] * A[1] + beta_Y[3] * Z[1] + beta_Y[4] * L[1] + beta_Y[5] * U[1] +
                      beta_Y[6] * Z[1] * A[1] + beta_Y[7] * Z[1] * L[1] + beta_Y[8] * L[1] * A[1], sigma_Y)
    }
    
    # Follow-up times
    for (j in 2:(time_points + 1)){
      L[j] <- rbinom(1, 1, expit(beta_L[1] + beta_L[2] * A[j-1] + beta_L[3] * Z[1] + beta_L[4] * U[1] + beta_L[5] * (j - 1)))
      A[j] <- rbinom(1, 1, expit(beta_A[1] + beta_A[2] * A[j-1] +  beta_A[3] * Z[1] + beta_A[4] * L[j] + beta_A[5] * (j - 1)))
      count_not_adhered <- (count_not_adhered + 1) * (1 - A[j-1])
      G[j] <- ifelse(count_not_adhered == m, 1, 0)
      if (A[j] == 0 & G[j] == 1){
        A[j] <- 1
      }
      lag1_A[j] <- A[j-1]
      if (include_deaths){
        D[j] <- rbinom(1, 1, expit(beta_D[1] + beta_D[2] * U[1]))
        if (D[j] == 1){
          death_time <- j - 1
          break
        }
      }
      R[j] <- 1
      if (R[j] == 1){
        Y[j] <- rnorm(1, beta_Y[1] + beta_Y[2] * A[j] + beta_Y[3] * Z[1] + beta_Y[4] * L[j] + beta_Y[5] * U[1] +
                        beta_Y[6] * Z[1] * A[j] - beta_Y[7] * Z[1] * L[j] + beta_Y[8] * L[j] * A[j] + beta_Y[9] * (j - 1), sigma_Y)
      }
    }
  }

  # Consolidate data in a single data frame
  temp_data <- data.table(id = id, time = time, L = L, L0 = L0, Z = Z,
                          A = A, lag1_A = lag1_A, G = G, U = U,
                          C_artificial = C_artificial, R = R, Y = Y, 
                          D = D)
  # If we simulated death, truncate the simulated data set to the death time
  if (!is.na(death_time)){
    temp_data <- temp_data[temp_data$time <= death_time, ]
  }
  if (!include_deaths){
    temp_data$D <- NULL
  }
  return(temp_data)
}
