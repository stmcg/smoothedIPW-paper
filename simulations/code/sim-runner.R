results <- foreach(rep = 1:n_reps) %dorng% {
  print(rep)
  df <- lapply(as.list(1:n), FUN=function(ind){
    datagen(i = ind, time_points = time_points,
            beta_L = beta_L, beta_Z = beta_Z, beta_A = beta_A, beta_R = beta_R, beta_Y = beta_Y,
            sigma_Y = sigma_Y, U_lb = U_lb, U_ub = U_ub, m = m)
  })
  df <- rbindlist(df)

  res_pooled <- ipw(A_model = A_model,
                    R_model_numerator = R_model_numerator,
                    R_model_denominator = R_model_denominator,
                    Y_model = Y_model_pooled,
                    pooled = TRUE, data = df,
                    outcome_times = outcome_times, 
                    return_model_fits = FALSE)
  res_nonpooled <- ipw(A_model = A_model,
                       R_model_numerator = R_model_numerator,
                       R_model_denominator = R_model_denominator,
                       Y_model = Y_model_nonpooled,
                       pooled = FALSE, data = df,
                       outcome_times = outcome_times, 
                       return_model_fits = FALSE)

  if (coverage){
    res_pooled_ci <- get_CI(n_boot = n_boot, conf_level = conf_level,
                            data = df, ipw_res = res_pooled)
    res_nonpooled_ci <- get_CI(n_boot = n_boot, conf_level = conf_level,
                               data = df, ipw_res = res_nonpooled)

    # Z=0 - Z=1
    ci_lb_pooled_dif <- ci_ub_pooled_dif <-
      ci_lb_nonpooled_dif <- ci_ub_nonpooled_dif <-
      rep(NA, times = length(outcome_times))
    for (i in 1:length(outcome_times)){
      ci_lb_pooled_dif[i] <- quantile(res_pooled_ci$res_boot_all[, i, 1] - res_pooled_ci$res_boot_all[, i, 2],
                                      probs = 0.025)
      ci_ub_pooled_dif[i] <- quantile(res_pooled_ci$res_boot_all[, i, 1] - res_pooled_ci$res_boot_all[, i, 2],
                                      probs = 0.975)
      ci_lb_nonpooled_dif[i] <- quantile(res_nonpooled_ci$res_boot_all[, i, 1] - res_nonpooled_ci$res_boot_all[, i, 2],
                                      probs = 0.025)
      ci_ub_nonpooled_dif[i] <- quantile(res_nonpooled_ci$res_boot_all[, i, 1] - res_nonpooled_ci$res_boot_all[, i, 2],
                                      probs = 0.975)
    }

    return(list(est_pooled_z0 = res_pooled$est[, 'Z=0'],
                est_pooled_z1 = res_pooled$est[, 'Z=1'],
                est_nonpooled_z0 = res_nonpooled$est[, 'Z=0'],
                est_nonpooled_z1 = res_nonpooled$est[, 'Z=1'],
                ci_lb_pooled_z0 = res_pooled_ci$res_boot$`0`[, 'CI Lower'],
                ci_ub_pooled_z0 = res_pooled_ci$res_boot$`0`[, 'CI Upper'],
                ci_lb_pooled_z1 = res_pooled_ci$res_boot$`1`[, 'CI Lower'],
                ci_ub_pooled_z1 = res_pooled_ci$res_boot$`1`[, 'CI Upper'],
                ci_lb_pooled_dif = ci_lb_pooled_dif,
                ci_ub_pooled_dif = ci_ub_pooled_dif,
                ci_lb_nonpooled_z0 = res_nonpooled_ci$res_boot$`0`[, 'CI Lower'],
                ci_ub_nonpooled_z0 = res_nonpooled_ci$res_boot$`0`[, 'CI Upper'],
                ci_lb_nonpooled_z1 = res_nonpooled_ci$res_boot$`1`[, 'CI Lower'],
                ci_ub_nonpooled_z1 = res_nonpooled_ci$res_boot$`1`[, 'CI Upper'],
                ci_lb_nonpooled_dif = ci_lb_nonpooled_dif,
                ci_ub_nonpooled_dif = ci_ub_nonpooled_dif))
  } else {
    return(list(est_pooled_z0 = res_pooled$est[, 'Z=0'],
                est_pooled_z1 = res_pooled$est[, 'Z=1'],
                est_nonpooled_z0 = res_nonpooled$est[, 'Z=0'],
                est_nonpooled_z1 = res_nonpooled$est[, 'Z=1']))
  }
}

est_pooled_z0 <- est_pooled_z1 <-
  est_nonpooled_z0 <- est_nonpooled_z1 <- matrix(NA, nrow = n_reps, ncol = length(outcome_times))
if (coverage){
  ci_lb_pooled_dif <- ci_ub_pooled_dif <-
    ci_lb_pooled_z0 <- ci_ub_pooled_z0 <-
    ci_lb_pooled_z1 <- ci_ub_pooled_z1 <- matrix(NA, nrow = n_reps, ncol = length(outcome_times))
  ci_lb_nonpooled_dif <- ci_ub_nonpooled_dif <-
    ci_lb_nonpooled_z0 <- ci_ub_nonpooled_z0 <-
    ci_lb_nonpooled_z1 <- ci_ub_nonpooled_z1 <- matrix(NA, nrow = n_reps, ncol = length(outcome_times))

  for (rep in 1:n_reps) {
    est_pooled_z0[rep, ] <- results[[rep]]$est_pooled_z0
    est_pooled_z1[rep, ] <- results[[rep]]$est_pooled_z1
    est_nonpooled_z0[rep, ] <- results[[rep]]$est_nonpooled_z0
    est_nonpooled_z1[rep, ] <- results[[rep]]$est_nonpooled_z1
    ci_lb_pooled_z0[rep, ] = results[[rep]]$ci_lb_pooled_z0
    ci_ub_pooled_z0[rep, ] = results[[rep]]$ci_ub_pooled_z0
    ci_lb_pooled_z1[rep, ] = results[[rep]]$ci_lb_pooled_z1
    ci_ub_pooled_z1[rep, ] = results[[rep]]$ci_ub_pooled_z1
    ci_lb_pooled_dif[rep, ] = results[[rep]]$ci_lb_pooled_dif
    ci_ub_pooled_dif[rep, ] = results[[rep]]$ci_ub_pooled_dif
    ci_lb_nonpooled_z0[rep, ] = results[[rep]]$ci_lb_nonpooled_z0
    ci_ub_nonpooled_z0[rep, ] = results[[rep]]$ci_ub_nonpooled_z0
    ci_lb_nonpooled_z1[rep, ] = results[[rep]]$ci_lb_nonpooled_z1
    ci_ub_nonpooled_z1[rep, ] = results[[rep]]$ci_ub_nonpooled_z1
    ci_lb_nonpooled_dif[rep, ] = results[[rep]]$ci_lb_nonpooled_dif
    ci_ub_nonpooled_dif[rep, ] = results[[rep]]$ci_ub_nonpooled_dif
  }
} else {
  for (rep in 1:n_reps) {
    est_pooled_z0[rep, ] <- results[[rep]]$est_pooled_z0
    est_pooled_z1[rep, ] <- results[[rep]]$est_pooled_z1
    est_nonpooled_z0[rep, ] <- results[[rep]]$est_nonpooled_z0
    est_nonpooled_z1[rep, ] <- results[[rep]]$est_nonpooled_z1
  }
}
est_pooled_dif <- est_pooled_z0 - est_pooled_z1
est_nonpooled_dif <- est_nonpooled_z0 - est_nonpooled_z1
