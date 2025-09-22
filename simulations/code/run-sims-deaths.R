results <- foreach(rep = 1:n_reps) %dorng% {
  print(rep)
  df <- lapply(as.list(1:n), FUN=function(ind){
    datagen(i = ind, time_points = time_points,
            beta_L = beta_L, beta_Z = beta_Z, beta_A = beta_A, beta_R = beta_R, beta_Y = beta_Y, beta_D = beta_D, 
            sigma_Y = sigma_Y, U_lb = U_lb, U_ub = U_ub, m = m, include_deaths = TRUE)
  })
  df <- rbindlist(df)
  
  res_stacked <- ipw(A_model = A_model,
                     R_model_numerator = R_model_numerator,
                     R_model_denominator = R_model_denominator,
                     Y_model = Y_model_pooled,
                     pooled = TRUE, pooling_method = 'stacked',
                     outcome_times = outcome_times, 
                     data = df, return_model_fits = FALSE)
  res_nonstacked <- ipw(A_model = A_model,
                        R_model_numerator = R_model_numerator,
                        R_model_denominator = R_model_denominator,
                        Y_model = Y_model_pooled,
                        pooled = TRUE, pooling_method = 'nonstacked',
                        outcome_times = outcome_times, 
                        data = df, return_model_fits = FALSE)
  res_nonpooled <- ipw(A_model = A_model,
                       R_model_numerator = R_model_numerator,
                       R_model_denominator = R_model_denominator,
                       Y_model = Y_model_nonpooled,
                       pooled = FALSE, 
                       outcome_times = outcome_times, 
                       data = df, return_model_fits = FALSE)
  
  if (coverage){
    res_stacked_ci <- get_CI(n_boot = n_boot, conf_level = conf_level,
                             data = df, ipw_res = res_stacked, 
                             reference_z_value = 1, show_progress = FALSE)
    res_nonstacked_ci <- get_CI(n_boot = n_boot, conf_level = conf_level,
                             data = df, ipw_res = res_nonstacked, 
                             reference_z_value = 1, show_progress = FALSE)
    res_nonpooled_ci <- get_CI(n_boot = n_boot, conf_level = conf_level,
                               data = df, ipw_res = res_nonpooled, 
                               reference_z_value = 1, show_progress = FALSE)
    
    return(list(est_stacked_z0 = res_stacked$est[, 'Z=0'],
                est_stacked_z1 = res_stacked$est[, 'Z=1'],
                est_nonstacked_z0 = res_nonstacked$est[, 'Z=0'],
                est_nonstacked_z1 = res_nonstacked$est[, 'Z=1'],
                est_nonpooled_z0 = res_nonpooled$est[, 'Z=0'],
                est_nonpooled_z1 = res_nonpooled$est[, 'Z=1'],
                ci_lb_stacked_z0 = res_stacked_ci$res_boot$`0`[, 'CI Lower'],
                ci_ub_stacked_z0 = res_stacked_ci$res_boot$`0`[, 'CI Upper'],
                ci_lb_stacked_z1 = res_stacked_ci$res_boot$`1`[, 'CI Lower'],
                ci_ub_stacked_z1 = res_stacked_ci$res_boot$`1`[, 'CI Upper'],
                ci_lb_stacked_dif = res_stacked_ci$res_boot_contrast$`0`[, 'CI Lower'],
                ci_ub_stacked_dif = res_stacked_ci$res_boot_contrast$`0`[, 'CI Upper'],
                ci_lb_nonstacked_z0 = res_nonstacked_ci$res_boot$`0`[, 'CI Lower'],
                ci_ub_nonstacked_z0 = res_nonstacked_ci$res_boot$`0`[, 'CI Upper'],
                ci_lb_nonstacked_z1 = res_nonstacked_ci$res_boot$`1`[, 'CI Lower'],
                ci_ub_nonstacked_z1 = res_nonstacked_ci$res_boot$`1`[, 'CI Upper'],
                ci_lb_nonstacked_dif = res_nonstacked_ci$res_boot_contrast$`0`[, 'CI Lower'],
                ci_ub_nonstacked_dif = res_nonstacked_ci$res_boot_contrast$`0`[, 'CI Upper'],
                ci_lb_nonpooled_z0 = res_nonpooled_ci$res_boot$`0`[, 'CI Lower'],
                ci_ub_nonpooled_z0 = res_nonpooled_ci$res_boot$`0`[, 'CI Upper'],
                ci_lb_nonpooled_z1 = res_nonpooled_ci$res_boot$`1`[, 'CI Lower'],
                ci_ub_nonpooled_z1 = res_nonpooled_ci$res_boot$`1`[, 'CI Upper'],
                ci_lb_nonpooled_dif = res_nonpooled_ci$res_boot_contrast$`0`[, 'CI Lower'],
                ci_ub_nonpooled_dif = res_nonpooled_ci$res_boot_contrast$`0`[, 'CI Upper']))
  } else {
    return(list(est_stacked_z0 = res_stacked$est[, 'Z=0'],
                est_stacked_z1 = res_stacked$est[, 'Z=1'],
                est_nonstacked_z0 = res_nonstacked$est[, 'Z=0'],
                est_nonstacked_z1 = res_nonstacked$est[, 'Z=1'],
                est_nonpooled_z0 = res_nonpooled$est[, 'Z=0'],
                est_nonpooled_z1 = res_nonpooled$est[, 'Z=1']))
  }
}

est_stacked_z0 <- est_stacked_z1 <-
  est_nonstacked_z0 <- est_nonstacked_z1 <-
  est_nonpooled_z0 <- est_nonpooled_z1 <- matrix(NA, nrow = n_reps, ncol = length(outcome_times))
if (coverage){
  ci_lb_stacked_dif <- ci_ub_stacked_dif <-
    ci_lb_stacked_z0 <- ci_ub_stacked_z0 <-
    ci_lb_stacked_z1 <- ci_ub_stacked_z1 <- matrix(NA, nrow = n_reps, ncol = length(outcome_times))
  ci_lb_nonstacked_dif <- ci_ub_nonstacked_dif <-
    ci_lb_nonstacked_z0 <- ci_ub_nonstacked_z0 <-
    ci_lb_nonstacked_z1 <- ci_ub_nonstacked_z1 <- matrix(NA, nrow = n_reps, ncol = length(outcome_times))
  ci_lb_nonpooled_dif <- ci_ub_nonpooled_dif <-
    ci_lb_nonpooled_z0 <- ci_ub_nonpooled_z0 <-
    ci_lb_nonpooled_z1 <- ci_ub_nonpooled_z1 <- matrix(NA, nrow = n_reps, ncol = length(outcome_times))
  
  for (rep in 1:n_reps) {
    est_stacked_z0[rep, ] <- results[[rep]]$est_stacked_z0
    est_stacked_z1[rep, ] <- results[[rep]]$est_stacked_z1
    est_nonstacked_z0[rep, ] <- results[[rep]]$est_nonstacked_z0
    est_nonstacked_z1[rep, ] <- results[[rep]]$est_nonstacked_z1
    est_nonpooled_z0[rep, ] <- results[[rep]]$est_nonpooled_z0
    est_nonpooled_z1[rep, ] <- results[[rep]]$est_nonpooled_z1
    ci_lb_stacked_z0[rep, ] = results[[rep]]$ci_lb_stacked_z0
    ci_ub_stacked_z0[rep, ] = results[[rep]]$ci_ub_stacked_z0
    ci_lb_stacked_z1[rep, ] = results[[rep]]$ci_lb_stacked_z1
    ci_ub_stacked_z1[rep, ] = results[[rep]]$ci_ub_stacked_z1
    ci_lb_stacked_dif[rep, ] = results[[rep]]$ci_lb_stacked_dif
    ci_ub_stacked_dif[rep, ] = results[[rep]]$ci_ub_stacked_dif
    ci_lb_nonstacked_z0[rep, ] = results[[rep]]$ci_lb_nonstacked_z0
    ci_ub_nonstacked_z0[rep, ] = results[[rep]]$ci_ub_nonstacked_z0
    ci_lb_nonstacked_z1[rep, ] = results[[rep]]$ci_lb_nonstacked_z1
    ci_ub_nonstacked_z1[rep, ] = results[[rep]]$ci_ub_nonstacked_z1
    ci_lb_nonstacked_dif[rep, ] = results[[rep]]$ci_lb_nonstacked_dif
    ci_ub_nonstacked_dif[rep, ] = results[[rep]]$ci_ub_nonstacked_dif
    ci_lb_nonpooled_z0[rep, ] = results[[rep]]$ci_lb_nonpooled_z0
    ci_ub_nonpooled_z0[rep, ] = results[[rep]]$ci_ub_nonpooled_z0
    ci_lb_nonpooled_z1[rep, ] = results[[rep]]$ci_lb_nonpooled_z1
    ci_ub_nonpooled_z1[rep, ] = results[[rep]]$ci_ub_nonpooled_z1
    ci_lb_nonpooled_dif[rep, ] = results[[rep]]$ci_lb_nonpooled_dif
    ci_ub_nonpooled_z1[rep, ] = results[[rep]]$ci_ub_nonpooled_z1
    ci_lb_nonpooled_dif[rep, ] = results[[rep]]$ci_lb_nonpooled_dif
    ci_ub_nonpooled_dif[rep, ] = results[[rep]]$ci_ub_nonpooled_dif
  }
} else {
  for (rep in 1:n_reps) {
    est_stacked_z0[rep, ] <- results[[rep]]$est_stacked_z0
    est_stacked_z1[rep, ] <- results[[rep]]$est_stacked_z1
    est_nonstacked_z0[rep, ] <- results[[rep]]$est_nonstacked_z0
    est_nonstacked_z1[rep, ] <- results[[rep]]$est_nonstacked_z1
    est_nonpooled_z0[rep, ] <- results[[rep]]$est_nonpooled_z0
    est_nonpooled_z1[rep, ] <- results[[rep]]$est_nonpooled_z1
  }
}
est_stacked_dif <- est_stacked_z0 - est_stacked_z1
est_nonstacked_dif <- est_nonstacked_z0 - est_nonstacked_z1
est_nonpooled_dif <- est_nonpooled_z0 - est_nonpooled_z1
