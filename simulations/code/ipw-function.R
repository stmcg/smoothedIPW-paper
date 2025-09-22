ipw <- function(data,
                pooled = TRUE,
                pooling_method = 'nonstacked',
                outcome_times,
                A_model,
                R_model_numerator,
                R_model_denominator,
                Y_model,
                truncation_percentile = NULL,
                return_model_fits = TRUE,
                return_weights = TRUE,
                trim_returned_models = FALSE){
  # Check input
  if (missing(data)){
    stop('The argument data must be specified')
  }
  if (missing(A_model)){
    stop('The argument A_model must be specified')
  }
  if (missing(R_model_denominator)){
    stop('The argument R_model_denominator must be specified')
  }
  if (missing(Y_model)){
    stop('The argument Y_model must be specified')
  }
  if (!is.null(truncation_percentile)){
    if (truncation_percentile < 0 | truncation_percentile > 1){
      stop('The argument truncation_percentile must be beween 0 and 1')
    }
  }
  if (!'time' %in% colnames(data)){
    stop("The observed data must include a column called 'time' indicating the time interval.")
  }
  if (!'id' %in% colnames(data)){
    stop("The observed data must include a column called 'id' indicating the participant ID number.")
  }
  
  # Check A_model
  if (!inherits(A_model, "formula")) {
    stop("A_model must be a formula, e.g., A ~ L + Z")
  }
  lhs <- all.vars(A_model[[2]]); rhs <- all.vars(A_model[[3]])
  if (length(lhs) != 1 || lhs != "A") {
    stop("The left-hand side of A_model must be the variable 'A'.")
  }
  if (!"Z" %in% rhs) {
    warning("A_model does not include 'Z' as a predictor.")
  }
  if ("D" %in% rhs) {
    stop("A_model should not include 'D' as a predictor.")
  }
  
  # Check R_model_numerator (if applicable)
  if (!missing(R_model_numerator)){
    if (!inherits(R_model_numerator, "formula")) {
      stop("R_model_numerator must be a formula, e.g., R ~ L_baseline + Z")
    }
    lhs <- all.vars(R_model_numerator[[2]]); rhs <- all.vars(R_model_numerator[[3]])
    if (length(lhs) != 1 || lhs != "R") {
      stop("The left-hand side of R_model_numerator must be the variable 'R'.")
    }
    if (!"Z" %in% rhs) {
      warning("R_model_numerator does not include 'Z' as a predictor.")
    }
    if ("D" %in% rhs) {
      stop("R_model_numerator should not include 'D' as a predictor.")
    }
  }
  
  # Check R_model_denominator
  if (!inherits(R_model_denominator, "formula")) {
    stop("R_model_denominator must be a formula, e.g., R ~ L + A + Z")
  }
  lhs <- all.vars(R_model_denominator[[2]]); rhs <- all.vars(R_model_denominator[[3]])
  if (length(lhs) != 1 || lhs != "R") {
    stop("The left-hand side of R_model_denominator must be the variable 'R'.")
  }
  if (!"Z" %in% rhs) {
    warning("R_model_denominator does not include 'Z' as a predictor.")
  }
  if ("D" %in% rhs) {
    stop("R_model_denominator should not include 'D' as a predictor.")
  }
  
  # Check Y_model
  if (!inherits(Y_model, "formula")) {
    stop("Y_model must be a formula, e.g., R ~ L_baseline + Z")
  }
  lhs <- all.vars(Y_model[[2]]); rhs <- all.vars(Y_model[[3]])
  if (length(lhs) != 1 || lhs != "Y") {
    stop("The left-hand side of Y_model must be the variable 'Y'.")
  }
  if (!"Z" %in% rhs) {
    warning("Y_model does not include 'Z' as a predictor.")
  }
  if (!"time" %in% rhs & pooled) {
    warning("Y_model does not include 'time' as a predictor although the pooled IPW method is selected.")
  }
  if ("time" %in% rhs & !pooled) {
    warning("Y_model should not include 'time' as a predictor since the non-pooled IPW method is selected.")
  }
  if ("A" %in% rhs) {
    stop("Y_model should not include 'A' (or any other post-baseline covariate) as a predictor.")
  }
  if ("R" %in% rhs) {
    stop("Y_model should not include 'R' (or any other post-baseline covariate) as a predictor.")
  }
  if ("D" %in% rhs) {
    stop("Y_model should not include 'D' as a predictor.")
  }
  
  # Check person-time format of data
  if (!isTRUE(all.equal(sort(as.numeric(unique(data$id))),
                        1:length(unique(data$id))))){
    stop("Individual ID values (specified by the 'id' column in 'data') should range from 1 to n, where n denotes the number of unique individuals.")
  }
  problematic_ids <- tapply(data$time, data$id,
                            FUN = function(x){
                              !isTRUE(all.equal(x, 0:(length(x) - 1)))
                            })
  if (any(problematic_ids)){
    stop(paste0("For each individual, time intervals in the observed data (specified by the 'time' column in 'data') should begin with 0 and increase in increments of 1 in consecutive rows, where no time records are skipped. The data set includes ",
                sum(problematic_ids), ' individuals that do not follow this format correctly, including IDs ',
                paste(utils::head(names(problematic_ids)[problematic_ids]), collapse = ", "), '.'))
  }
  
  # Checking that suitable data processing was performed
  if (!'C_artificial' %in% colnames(data)){
    stop("The observed data must include a column called 'C_artificial' indicating when an individual should be artificially censored.")
  }
  if (!'A_model_eligible' %in% colnames(data)){
    stop("The observed data must include a column called 'A_model_eligible' indicating what records should be used for fitting the treatment adherence model.")
  }
  if (!'R_model_denominator_eligible' %in% colnames(data)){
    data$R_model_denominator_eligible <- !data$C_artificial
  }
  
  # Set parameters
  if (missing(outcome_times)){
    outcome_times <- 0:max(data$time)
  } else {
    if (!all(outcome_times %in% 0:max(data$time))){
      stop("All values in 'outcome_times' must be no greater than the maximum time interval in 'data' and no less than 0.")
    }
  }
  any_deaths <- 'D' %in% colnames(data)
  if (is.factor(data$Y)){
    outcome_type <- 'binary'
  } else {
    outcome_type <- 'continuous'
    if (length(unique(data$Y)) == 2){
      warning("The outcome is treated as a continuous variable, although only two levels where detected.
              To treat the outcome as a binary variable, the column 'Y' in the observed dataset should be a factor variable.")
    }
  }
  
  if (!any_deaths){
    # Pooled/nonpooled IPW without deaths
    res <- ipw_helper(data = data,
                      pooled = pooled,
                      outcome_times = outcome_times,
                      A_model = A_model,
                      R_model_numerator = R_model_numerator,
                      R_model_denominator = R_model_denominator,
                      Y_model = Y_model,
                      truncation_percentile = truncation_percentile,
                      return_model_fits = return_model_fits,
                      return_weights = return_weights,
                      trim_returned_models = trim_returned_models,
                      outcome_type = outcome_type)
    est <- res$est
    model_fits <- res$model_fits
    data_weights <- res$data_weights
  } else {
    if (!pooled){
      # Nonpooled IPW with deaths
      if (return_model_fits){
        model_fits <- vector(mode = "list", length = length(outcome_times))
      } else {
        model_fits <- NULL
      }
      if (return_weights){
        data_weights <- vector(mode = "list", length = length(outcome_times))
      } else {
        data_weights <- NULL
      }
      
      i <- 1
      for (outcome_time in outcome_times){
        newdata <- data[data$time <= outcome_time,]
        ids_to_keep <- newdata[newdata$time == outcome_time & newdata$D == 0, ]$id
        newdata <- newdata[newdata$id %in% ids_to_keep, ]
        
        res_temp <- ipw_helper(data = newdata,
                               pooled = FALSE,
                               outcome_times = outcome_time,
                               A_model = A_model,
                               R_model_numerator = R_model_numerator,
                               R_model_denominator = R_model_denominator,
                               Y_model = Y_model,
                               truncation_percentile = truncation_percentile,
                               return_model_fits = return_model_fits,
                               return_weights = return_weights,
                               trim_returned_models = trim_returned_models,
                               outcome_type = outcome_type)
        if (i == 1){
          est <- res_temp$est
        } else {
          est <- rbind(est, res_temp$est)
        }
        if (return_model_fits){
          model_fits[[i]] <- res_temp$model_fits
        }
        if (return_weights){
          data_weights[[i]] <- res_temp$data_weights
        }
        i <- i + 1
      }
    } else {
      # IPW with deaths, nonstacked pooling method
      if (pooling_method == 'nonstacked'){
        if (return_model_fits){
          model_fits <- vector(mode = "list", length = length(outcome_times))
        } else {
          model_fits <- NULL
        }
        if (return_weights){
          data_weights <- vector(mode = "list", length = length(outcome_times))
        } else {
          data_weights <- NULL
        }
        
        i <- 1
        for (outcome_time in outcome_times){
          newdata <- data[data$time <= outcome_time,]
          ids_to_keep <- newdata[newdata$time == outcome_time & newdata$D == 0, ]$id
          newdata <- newdata[newdata$id %in% ids_to_keep, ]
          
          res_temp <- ipw_helper(data = newdata,
                                 pooled = TRUE,
                                 outcome_times = outcome_time,
                                 A_model = A_model,
                                 R_model_numerator = R_model_numerator,
                                 R_model_denominator = R_model_denominator,
                                 Y_model = Y_model,
                                 truncation_percentile = truncation_percentile,
                                 return_model_fits = return_model_fits,
                                 return_weights = return_weights,
                                 trim_returned_models = trim_returned_models,
                                 outcome_type = outcome_type)
          if (i == 1){
            est <- res_temp$est
          } else {
            est <- rbind(est, res_temp$est)
          }
          if (return_model_fits){
            model_fits[[i]] <- res_temp$model_fits
          }
          if (return_weights){
            data_weights[[i]] <- res_temp$data_weights
          }
          i <- i + 1
        }
      } else if (pooling_method == 'stacked'){
        # IPW with deaths, stacked pooling method
        if (return_model_fits){
          model_fits <- vector(mode = "list", length = max(data$time) + 2)
        } else {
          model_fits <- NULL
        }
        
        # Step 1: Created stacked data set with weights
        for (j in 0:max(data$time)){
          newdata <- data[data$time <= j,]
          ids_to_keep <- newdata[newdata$time == j & newdata$D == 0, ]$id
          newdata <- newdata[newdata$id %in% ids_to_keep, ]
          
          res_temp <- ipw_helper(data = newdata,
                                 pooled = TRUE,
                                 outcome_times = j,
                                 A_model = A_model,
                                 R_model_numerator = R_model_numerator,
                                 R_model_denominator = R_model_denominator,
                                 Y_model = Y_model,
                                 truncation_percentile = truncation_percentile,
                                 return_model_fits = return_model_fits,
                                 return_weights = return_weights,
                                 only_compute_weights = TRUE,
                                 trim_returned_models = trim_returned_models,
                                 outcome_type = outcome_type)
          df_stack <- res_temp$df_stack
          if (return_model_fits){
            model_fits[[j + 1]] <- res_temp$model_fits
          }
          if (j == 0){
            dat_stacked <- df_stack
          } else {
            dat_stacked <- rbind(dat_stacked, df_stack)
          }
        }
        # Truncate IP weights, if applicable
        if (!is.null(truncation_percentile)){
          trunc_val <- stats::quantile(dat_stacked$weights, probs = truncation_percentile)
          dat_stacked$weights <- pmin(dat_stacked$weights, trunc_val)
        }
        if (return_weights){
          data_weights <- dat_stacked[, c('id', 'time', 'Z', 'weights', 'weights_A', 'weights_R')]
        } else {
          data_weights <- NULL
        }
        
        # Step 2: Fit weighted outcome model
        error_message_fit <- NULL
        fit_Y <- tryCatch(
          if (outcome_type == 'binary'){
            stats::glm(formula = Y_model, data = dat_stacked[dat_stacked$weights > 0,], family = stats::binomial(), weights = weights)
          } else {
            stats::glm(formula = Y_model, data = dat_stacked[dat_stacked$weights > 0,], family = stats::gaussian(), weights = weights)
          }
          ,
          error = function(e) {
            error_message_fit <<- paste0("Error in fitting the model for Y: ", conditionMessage(e))
            NULL
          }
        )
        if (return_model_fits){
          model_fits[[max(data$time) + 2]] <- trim_glm(fit_Y, trim_returned_models = trim_returned_models)
        }
        
        # Step 3: Estimating counterfactual outcome means
        z_levels <- sort(unique(data$Z))
        n_z <- length(z_levels)
        est <- matrix(NA, nrow = length(outcome_times), ncol = n_z + 1)
        est[, 1] <- outcome_times
        colnames(est) <- c('time', paste0('Z=', z_levels))
        data_baseline <- data[data$time == 0,]
        
        row_index <- 0
        for (outcome_time in outcome_times){
          row_index <- row_index + 1
          
          data_temp <- data_baseline
          data_temp$time <- outcome_time
          ids_to_keep <- data[data$time == outcome_time & data$D == 0, ]$id
          data_temp <- data_temp[data_temp$id %in% ids_to_keep, ]
          
          col_index <- 1
          for (z_val in z_levels){
            col_index <- col_index + 1
            data_temp$Z <- z_val
            est[row_index, col_index] <- mean(stats::predict(fit_Y, type = 'response', newdata = data_temp))
          }
        }
      }
    }
  }
  
  # Get all arguments supplied to the function, except the input data set
  args <- as.list(match.call())[-1]
  args$outcome_times <- outcome_times
  args$data <- NULL
  
  out <- list(est = est, args = args, model_fits = model_fits, data_weights = data_weights,
              outcome_type = outcome_type)
  class(out) <- 'ipw'
  return(out)
}



ipw_helper <- function(data,
                       pooled = TRUE,
                       outcome_times,
                       A_model,
                       R_model_numerator,
                       R_model_denominator,
                       Y_model,
                       truncation_percentile = NULL,
                       return_model_fits = TRUE,
                       return_weights = TRUE,
                       only_compute_weights = FALSE,
                       trim_returned_models,
                       outcome_type){
  
  time_points <- length(outcome_times)
  
  # Artificially censor individuals when they deviate from the treatment strategy
  data_censored <- data[data$C_artificial == 0,]
  
  # Treatment model and weights
  fit_model_A <- nrow(data[data$A_model_eligible == 1,]) >= 1
  error_message_fit <- NULL
  if (fit_model_A){
    fit_A <- tryCatch(
      stats::glm(A_model, family = 'binomial', data = data[data$A_model_eligible == 1,]),
      error = function(e) {
        error_message_fit <<- paste0("Error in fitting the model for A: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(error_message_fit)) {
      stop(error_message_fit)
    }
  } else {
    fit_A <- NULL
  }
  prob_A1 <- ifelse(data_censored$A_model_eligible == 1, stats::predict(fit_A, type = 'response', newdata = data_censored), 1)
  if (!return_model_fits){
    fit_A <- NULL
  } else {
    fit_A <- trim_glm(fit_A, trim_returned_models = trim_returned_models)
  }
  
  # Measurement model (denominator) and weights
  fit_R_denominator <- tryCatch(
    stats::glm(R_model_denominator, family = 'binomial', data = data[data$R_model_denominator_eligible == 1,]),
    error = function(e) {
      error_message_fit <<- paste0("Error in fitting the model for R (denominator): ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(error_message_fit)) {
    stop(error_message_fit)
  }
  prob_R1_denominator <- stats::predict(fit_R_denominator, type = 'response', newdata = data_censored)
  if (!return_model_fits){
    fit_R_denominator <- NULL
  } else {
    fit_R_denominator <- trim_glm(fit_R_denominator, trim_returned_models = trim_returned_models)
  }
  
  # Measurement model (numerator) and weights
  if (!missing(R_model_numerator)){
    fit_R_numerator <- tryCatch(
      stats::glm(R_model_numerator, family = 'binomial', data = data),
      error = function(e) {
        error_message_fit <<- paste0("Error in fitting the model for R (numerator): ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(error_message_fit)) {
      stop(error_message_fit)
    }
  } else {
    fit_R_numerator <- NULL
  }
  if (!missing(R_model_numerator)){
    prob_R1_numerator <- stats::predict(fit_R_numerator, type = 'response', newdata = data_censored)
    if (!return_model_fits){
      fit_R_numerator <- NULL
    } else {
      fit_R_numerator <- trim_glm(fit_R_numerator, trim_returned_models = trim_returned_models)
    }
  } else {
    prob_R1_numerator <- rep(1, times = nrow(data_censored))
  }
  
  # Compute IP weights based on censored data set
  weights_A <- unname(unlist(tapply(1 / prob_A1, data_censored$id, FUN = cumprod)))
  weights_R <- ifelse(data_censored$R == 1, prob_R1_numerator / prob_R1_denominator, 0)
  data_censored$weights <- weights_A * weights_R
  if (return_weights){
    data_censored$weights_A <- weights_A
    data_censored$weights_R <- weights_R
  }
  
  # End function if only needing weights
  if (only_compute_weights){
    if (return_model_fits){
      model_fits <- list(fit_A = fit_A,
                         fit_R_denominator = fit_R_denominator,
                         fit_R_numerator = fit_R_numerator)
    } else {
      model_fits <- NULL
    }
    return(list(est = NULL, model_fits = model_fits, df_stack = data_censored))
  }
  
  # Truncate IP weights, if applicable
  if (!is.null(truncation_percentile)){
    trunc_val <- stats::quantile(data_censored$weights, probs = truncation_percentile)
    data_censored$weights <- pmin(data_censored$weights, trunc_val)
  }
  
  # Preparing data sets for estimating counterfactual outcome means
  z_levels <- sort(unique(data$Z))
  n_z <- length(z_levels)
  est <- matrix(NA, nrow = time_points, ncol = n_z + 1)
  est[, 1] <- outcome_times
  colnames(est) <- c('time', paste0('Z=', z_levels))
  data_baseline <- data[data$time == 0,]
  
  # Estimating counterfactual outcome means
  row_index <- 0
  if (pooled){
    fit_Y <- tryCatch(
      if (outcome_type == 'binary'){
        stats::glm(formula = Y_model, data = data_censored[data_censored$weights > 0,], family = stats::binomial(), weights = weights)
      } else {
        stats::glm(formula = Y_model, data = data_censored[data_censored$weights > 0,], family = stats::gaussian(), weights = weights)
      }
      ,
      error = function(e) {
        error_message_fit <<- paste0("Error in fitting the model for Y: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(error_message_fit)) {
      stop(error_message_fit)
    }
    for (k in outcome_times){
      row_index <- row_index + 1
      data_temp <- data_baseline; data_temp$time <- k
      col_index <- 1
      for (z_val in z_levels){
        col_index <- col_index + 1
        data_temp$Z <- z_val
        est[row_index, col_index] <- mean(stats::predict(fit_Y, type = 'response', newdata = data_temp))
      }
    }
    if (return_model_fits){
      fit_Y <- trim_glm(fit_Y, trim_returned_models = trim_returned_models)
    }
  } else {
    if (return_model_fits){
      fit_Y_all <- vector(mode = "list", length = time_points)
    }
    for (k in outcome_times){
      row_index <- row_index + 1
      fit_Y <- tryCatch(
        if (outcome_type == 'binary'){
          stats::glm(Y_model, data = data_censored[data_censored$time == k & data_censored$weights > 0,],
                     family = stats::binomial(), weights = weights)
        } else {
          stats::glm(Y_model, data = data_censored[data_censored$time == k & data_censored$weights > 0,],
                     family = stats::gaussian(), weights = weights)
        }
        ,
        error = function(e) {
          error_message_fit <<- paste0(paste0("Error in fitting the model for Y at time ", k, ': '), conditionMessage(e))
          NULL
        }
      )
      if (!is.null(error_message_fit)) {
        stop(error_message_fit)
      }
      data_temp <- data_baseline; data_temp$time <- k
      col_index <- 1
      for (z_val in z_levels){
        col_index <- col_index + 1
        data_temp$Z <- z_val
        est[row_index, col_index] <- mean(stats::predict(fit_Y, type = 'response', newdata = data_temp))
      }
      if (return_model_fits){
        fit_Y_all[[k+1]] <- trim_glm(fit_Y, trim_returned_models = trim_returned_models)
      }
    }
  }
  
  if (return_model_fits){
    if (pooled){
      model_fits <- list(fit_A = fit_A,
                         fit_R_denominator = fit_R_denominator,
                         fit_R_numerator = fit_R_numerator,
                         fit_Y = fit_Y)
    } else {
      model_fits <- list(fit_A = fit_A,
                         fit_R_denominator = fit_R_denominator,
                         fit_R_numerator = fit_R_numerator,
                         fit_Y = fit_Y_all)
    }
  } else {
    model_fits <- NULL
  }
  if (return_weights){
    data_weights <- data_censored[, c('id', 'time', 'Z', 'weights', 'weights_A', 'weights_R')]
  } else {
    data_weights <- NULL
  }
  out <- list(est = as.data.frame(est), model_fits = model_fits, df_stack = NULL,
              data_weights = data_weights)
  
  return(out)
}

trim_glm <- function(fit, trim_returned_models) {
  if (trim_returned_models & !is.null(fit)){
    fit <- summary(fit)$coefficients
  }
  return(fit)
}

get_CI <- function(ipw_res, data, n_boot, conf_level = 0.95,
                   reference_z_value, contrast_type = 'difference',
                   show_progress = TRUE){
  # Check input
  if (missing(n_boot)){
    stop('The argument n_boot must be specified')
  }
  if (conf_level < 0 | conf_level > 1){
    stop('conf_level must be between 0 and 1')
  }
  if (!data.table::is.data.table(data)) {
    message("The argument 'data' must be a data.table. Converting 'data' to a data.table.")
    data <- data.table::as.data.table(data)
  }
  
  outcome_times <- eval(ipw_res$args$outcome_times)
  time_points <- length(outcome_times)
  z_levels <- sort(unique(data$Z))
  n_z <- length(z_levels)
  
  if (missing(reference_z_value)){
    reference_z_value <- z_levels[1]
  } else {
    if (!reference_z_value %in% z_levels){
      stop("Invalid value for 'reference_z_value'. The argument 'reference_z_value' must be set to a value of Z appearing in 'data'.")
    }
  }
  reference_z_index <- which(z_levels == reference_z_value)
  
  if (!contrast_type %in% c('difference', 'ratio')){
    stop("Invalid value for 'contrast_type'. The argument 'contrast_type' must be set to 'difference' or 'ratio'.")
  }
  
  # Step 1: Perform bootstrapping
  if (show_progress) {
    pb <- progress::progress_bar$new(
      total = n_boot,
      format = "  Bootstrapping [:bar] :percent | Elapsed: :elapsed | Time Remaining: :eta",
      clear = FALSE
    )
  }
  res_boot_all <- array(NA, dim = c(n_boot, time_points, n_z))
  for (i in 1:n_boot){
    tryCatch({
      data_boot <- resample_data(data = data)
      ipw_res_boot <- ipw(data = data_boot,
                          pooled = ipw_res$args$pooled,
                          pooling_method = ipw_res$args$pooling_method,
                          outcome_times = outcome_times,
                          A_model = eval(ipw_res$args$A_model),
                          R_model_numerator = eval(ipw_res$args$R_model_numerator),
                          R_model_denominator = eval(ipw_res$args$R_model_denominator),
                          Y_model = eval(ipw_res$args$Y_model),
                          truncation_percentile = eval(ipw_res$args$truncation_percentile),
                          return_model_fits = FALSE,
                          return_weights = FALSE,
                          trim_returned_models = TRUE)
      for (j in 1:n_z){
        res_boot_all[i, , j] <- ipw_res_boot$est[, paste0('Z=', z_levels[j])]
      }
    },
    error = function(e){
      warning(paste0('An error occured in bootstrap replicate ', i,
                     '. Bootstrap confidence intervals will be constructed excluding estimates from this bootstrap replicate.
                     The error message encountered is:\n', e))
    })
    if (show_progress){
      pb$tick()
    }
  }
  
  # Step 2: Compute CI
  alpha <- 1 - conf_level
  res_boot <- res_boot_contrast <- vector(mode = 'list', length = n_z)
  res_boot_z_reference <- res_boot_all[, , reference_z_index]
  
  for (j in 1:n_z){
    res_boot_single <- res_boot_contrast_single <- matrix(NA, nrow = time_points, ncol = 4)
    colnames(res_boot_single) <- colnames(res_boot_contrast_single) <- c('Time', 'Estimate', 'CI Lower', 'CI Upper')
    
    z_val <- z_levels[j]
    res_boot_z <- res_boot_all[, , j]
    
    res_boot_single[, 1] <- ipw_res$est[, 1]
    res_boot_single[, 2] <- ipw_res$est[, paste0('Z=', z_val)]
    
    res_boot_contrast_single[, 1] <- ipw_res$est[, 1]
    if (contrast_type == 'difference'){
      res_boot_contrast_single[, 2] <- ipw_res$est[, paste0('Z=', z_val)] - ipw_res$est[, paste0('Z=', reference_z_value)]
    } else if (contrast_type == 'ratio'){
      res_boot_contrast_single[, 2] <- ipw_res$est[, paste0('Z=', z_val)] / ipw_res$est[, paste0('Z=', reference_z_value)]
    }
    
    if (time_points > 1){
      for (i in 1:time_points){
        res_boot_single[i, c(3, 4)] <- stats::quantile(res_boot_z[, i], probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
        if (contrast_type == 'difference'){
          res_boot_contrast_single[i, c(3, 4)] <- stats::quantile(res_boot_z[, i] - res_boot_z_reference[, i], probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
        } else if (contrast_type == 'ratio'){
          res_boot_contrast_single[i, c(3, 4)] <- stats::quantile(res_boot_z[, i] / res_boot_z_reference[, i], probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
        }
      }
    } else {
      res_boot_single[1, c(3, 4)] <- stats::quantile(res_boot_z, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
      if (contrast_type == 'difference'){
        res_boot_contrast_single[1, c(3, 4)] <- stats::quantile(res_boot_z - res_boot_z_reference, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
      } else if (contrast_type == 'ratio'){
        res_boot_contrast_single[1, c(3, 4)] <- stats::quantile(res_boot_z / res_boot_z_reference, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
      }
    }
    if (j != reference_z_index){
      res_boot_contrast[[j]] <- res_boot_contrast_single
    }
    res_boot[[j]] <- res_boot_single
  }
  names(res_boot) <- names(res_boot_contrast) <- z_levels
  return(list(res_boot = res_boot, res_boot_contrast = res_boot_contrast,
              res_boot_all = res_boot_all))
}

resample_data <- function(data){
  n_id <- length(unique(data$id))
  
  # Sample IDs with replacement and call them new_id
  ids <- data.table::as.data.table(sample(1:n_id, n_id, replace = TRUE))
  ids[, 'new_id' := 1:n_id]
  colnames(ids) <- c("id", "new_id")
  
  # Merge data set with new_id values with the original dataset
  resample_data <- copy(data)
  setkey(resample_data, "id")
  resample_data <- resample_data[J(ids), allow.cartesian = TRUE]
  resample_data[, 'id' := resample_data$new_id]
  resample_data[, 'new_id' := NULL]
  
  return(resample_data)
}
