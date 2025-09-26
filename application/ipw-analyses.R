################################################################################
## Continuous Outcome
################################################################################

rm(list = ls())
library('smoothedIPW')
load('dat_cleaned.RData')

covs_baseline <- c('age_t0', 'female', 'race_cat',
                   'hispanic', 'site_cat', 'year_t0',
                   'bmi_t0',
                   'wtchg_6mo_t0_catgain', 'wtchg_6mo_t0_catloss',
                   'medicaid_t0',
                   'smoke_t0',
                   'cov_steroid_t0',
                   'Hypothyroidism_t0_any',
                   'T2D_t0_any',
                   'Abnormal_glucose_t0_any',
                   'Bipolar_t0_any',
                   'Anxiety_t0_any',
                   'Mental_health_dis_t0_any',
                   'Neuropathic_pain_t0_any',
                   'Migraine_t0_any',
                   'Heart_Failure_t0_any',
                   'Chronic_Kidney_Disease_t0_any',
                   'CCI_Comorb_t0_any',
                   'Alcohol_abuse_t0_any',
                   'Drug_abuse_t0_any',
                   'AED_rxlt15_t0',
                   'antipsychotic_t0_any',
                   'antidiabetic_t0_any',
                   'HTN_rxlt15_t0',
                   'sum_encounter_6mo_t0'
)
covs_tv <- c('bmi_t',
             'wtchg_6mo_t_catgain', 'wtchg_6mo_t_catloss',
             'medicaid_t',
             'smoke_t',
             'cov_steroid_t',
             'Hypothyroidism_any',
             'T2D_any',
             'Abnormal_glucose_any',
             'Bipolar_any',
             'Anxiety_any',
             'Mental_health_dis_any',
             'Neuropathic_pain_any',
             'Migraine_any',
             'Heart_Failure_any',
             'Chronic_Kidney_Disease_any',
             'CCI_Comorb_any',
             'Alcohol_abuse_any',
             'Drug_abuse_any',
             'AED_rxlt15',
             'antipsychotic_t0_any',
             'antidiabetic_t0_any',
             'HTN_rxlt15',
             't_otherrx',
             'sum_encounter_6mo_t'
)
mycols <- c('id', 'time', 'Z', 'A', 'R', 'D', 'Y',
            covs_baseline, covs_tv, 'A_model_eligible', 'C_artificial')
dat_reduced <- dat_reduced[, ..mycols]

A_model <- as.formula(
  paste0('A ~ Z + time + I(time^2) + ',
         paste0(covs_baseline, collapse = ' + '),
         ' + ', paste0(covs_tv, collapse = ' + ')))
R_model_numerator <- as.formula(
  paste0('R ~ Z + time + I(time^2) + ',
         paste0(covs_baseline, collapse = ' + ')))
R_model_denominator <- as.formula(
  paste0('R ~ A + Z + time + I(time^2) + ',
         paste0(covs_baseline, collapse = ' + '),
         ' + ', paste0(covs_tv, collapse = ' + ')))
Y_model_nonpooled <- as.formula(
  paste0('Y ~ Z + ',
         paste0(covs_baseline, collapse = ' + ')))
Y_model_pooled <- as.formula(
  paste0('Y ~ Z + time + I(time^2) + ',
         paste0(covs_baseline, collapse = ' + ')))

res_est_nonpooled <-
  ipw(data = dat_reduced,
      time_smoothed = FALSE,
      outcome_times = c(5, 11, 17 ,23),
      A_model = A_model,
      R_model_numerator = R_model_numerator,
      R_model_denominator = R_model_denominator,
      Y_model = Y_model_nonpooled,
      truncation_percentile = 0.99,
      return_model_fits = TRUE,
      trim_returned_models = FALSE)

res_est_nonstacked <-
  ipw(data = dat_reduced,
      time_smoothed = TRUE,
      smoothing_method = 'nonstacked',
      outcome_times = c(5, 11, 17 ,23),
      A_model = A_model,
      R_model_numerator = R_model_numerator,
      R_model_denominator = R_model_denominator,
      Y_model = Y_model_pooled,
      truncation_percentile = 0.99,
      return_model_fits = TRUE,
      trim_returned_models = TRUE)

res_est_stacked <-
  ipw(data = dat_reduced,
      time_smoothed = TRUE,
      smoothing_method = 'stacked',
      outcome_times = c(5, 11, 17, 23),
      A_model = A_model,
      R_model_numerator = R_model_numerator,
      R_model_denominator = R_model_denominator,
      Y_model = Y_model_pooled,
      truncation_percentile = 0.99,
      return_model_fits = TRUE,
      trim_returned_models = TRUE)

round(res_est_nonpooled$est, 2)
round(res_est_nonstacked$est, 2)
round(res_est_stacked$est, 2)

n_boot <- 25

set.seed(1234)
res_ci_nonpooled <- get_CI(res_est_nonpooled, data = dat_reduced, n_boot = n_boot)

round(res_ci_nonpooled$res_boot$`0`, 2)
round(res_ci_nonpooled$res_boot$`1`, 2)
round(res_ci_nonpooled$res_boot_contrast$`1`, 2)


set.seed(1234)
res_ci_nonstacked <- get_CI(res_est_nonstacked, data = dat_reduced, n_boot = n_boot)

round(res_ci_nonstacked$res_boot$`0`, 2)
round(res_ci_nonstacked$res_boot$`1`, 2)
round(res_ci_nonstacked$res_boot_contrast$`1`, 2)

set.seed(1234)
res_ci_stacked <- get_CI(res_est_stacked, data = dat_reduced, n_boot = n_boot)

round(res_ci_stacked$res_boot$`0`, 2)
round(res_ci_stacked$res_boot$`1`, 2)
round(res_ci_stacked$res_boot_contrast$`1`, 2)


save.image('results-05-09-2025-withstacked-truncation.RData')







(res_ci_nonpooled$res_boot$`1`[, 'CI Upper'] -
    res_ci_nonpooled$res_boot$`1`[, 'CI Lower'])^2 /
  (res_ci_nonstacked$res_boot$`1`[, 'CI Upper'] -
     res_ci_nonstacked$res_boot$`1`[, 'CI Lower'])^2


(res_ci_nonpooled$res_boot$`0`[, 'CI Upper'] -
    res_ci_nonpooled$res_boot$`0`[, 'CI Lower'])^2 /
  (res_ci_nonstacked$res_boot$`0`[, 'CI Upper'] -
     res_ci_nonstacked$res_boot$`0`[, 'CI Lower'])^2


(res_ci_nonpooled$res_boot_contrast$`1`[, 'CI Upper'] -
    res_ci_nonpooled$res_boot_contrast$`1`[, 'CI Lower'])^2 /
  (res_ci_nonstacked$res_boot_contrast$`1`[, 'CI Upper'] -
     res_ci_nonstacked$res_boot_contrast$`1`[, 'CI Lower'])^2


dat_reduced_binary <- dat_reduced


round(res_ci_stacked$res_boot, 2)

hist(res_ci$res_boot_all[, 1, 1])
hist(res_ci$res_boot_all[, 2, 1])
hist(res_ci$res_boot_all[, 1, 2])
hist(res_ci$res_boot_all[, 2, 2])







################################################################################
## Binary Outcome
################################################################################

rm(list = ls())
load('dat_cleaned.RData')
library('smoothedIPW')


dat_reduced$Y <- as.factor(dat_reduced$wtchg_ge5_ind_t)

covs_baseline <- c('age_t0', 'female', 'race_cat',
                   'hispanic', 'site_cat', 'year_t0',
                   'bmi_t0',
                   'wtchg_6mo_t0_catgain', 'wtchg_6mo_t0_catloss',
                   'medicaid_t0',
                   'smoke_t0',
                   'cov_steroid_t0',
                   'Hypothyroidism_t0_any',
                   'T2D_t0_any',
                   'Abnormal_glucose_t0_any',
                   'Bipolar_t0_any',
                   'Anxiety_t0_any',
                   'Mental_health_dis_t0_any',
                   'Neuropathic_pain_t0_any',
                   'Migraine_t0_any',
                   'Heart_Failure_t0_any',
                   'Chronic_Kidney_Disease_t0_any',
                   'CCI_Comorb_t0_any',
                   'Alcohol_abuse_t0_any',
                   'Drug_abuse_t0_any',
                   'AED_rxlt15_t0',
                   'antipsychotic_t0_any',
                   'antidiabetic_t0_any',
                   'HTN_rxlt15_t0',
                   'sum_encounter_6mo_t0'
)
covs_tv <- c('bmi_t',
             'wtchg_6mo_t_catgain', 'wtchg_6mo_t_catloss',
             'medicaid_t',
             'smoke_t',
             'cov_steroid_t',
             'Hypothyroidism_any',
             'T2D_any',
             'Abnormal_glucose_any',
             'Bipolar_any',
             'Anxiety_any',
             'Mental_health_dis_any',
             'Neuropathic_pain_any',
             'Migraine_any',
             'Heart_Failure_any',
             'Chronic_Kidney_Disease_any',
             'CCI_Comorb_any',
             'Alcohol_abuse_any',
             'Drug_abuse_any',
             'AED_rxlt15',
             'antipsychotic_t0_any',
             'antidiabetic_t0_any',
             'HTN_rxlt15',
             't_otherrx',
             'sum_encounter_6mo_t'
)
mycols <- c('id', 'time', 'Z', 'A', 'R', 'D', 'Y',
            covs_baseline, covs_tv, 'A_model_eligible', 'C_artificial')
dat_reduced <- dat_reduced[, ..mycols]

A_model <- as.formula(
  paste0('A ~ Z + time + I(time^2) + ',
         paste0(covs_baseline, collapse = ' + '),
         ' + ', paste0(covs_tv, collapse = ' + ')))
R_model_numerator <- as.formula(
  paste0('R ~ Z + time + I(time^2) + ',
         paste0(covs_baseline, collapse = ' + ')))
R_model_denominator <- as.formula(
  paste0('R ~ A + Z + time + I(time^2) + ',
         paste0(covs_baseline, collapse = ' + '),
         ' + ', paste0(covs_tv, collapse = ' + ')))
Y_model_nonpooled <- as.formula(
  paste0('Y ~ Z + ',
         paste0(covs_baseline, collapse = ' + ')))
Y_model_pooled <- as.formula(
  paste0('Y ~ Z + time + I(time^2) + ',
         paste0(covs_baseline, collapse = ' + ')))

res_est_nonpooled <-
  ipw(data = dat_reduced,
      time_smoothed = FALSE,
      outcome_times = c(5, 11, 17 ,23),
      A_model = A_model,
      R_model_numerator = R_model_numerator,
      R_model_denominator = R_model_denominator,
      Y_model = Y_model_nonpooled,
      truncation_percentile = 0.99,
      return_model_fits = TRUE,
      trim_returned_models = FALSE)

res_est_nonstacked <-
  ipw(data = dat_reduced,
      time_smoothed = TRUE,
      smoothing_method = 'nonstacked',
      outcome_times = c(5, 11, 17 ,23),
      A_model = A_model,
      R_model_numerator = R_model_numerator,
      R_model_denominator = R_model_denominator,
      Y_model = Y_model_pooled,
      truncation_percentile = 0.99,
      return_model_fits = TRUE,
      trim_returned_models = TRUE)

res_est_stacked <-
  ipw(data = dat_reduced,
      time_smoothed = TRUE,
      smoothing_method = 'stacked',
      outcome_times = c(5, 11, 17, 23),
      A_model = A_model,
      R_model_numerator = R_model_numerator,
      R_model_denominator = R_model_denominator,
      Y_model = Y_model_pooled,
      truncation_percentile = 0.99,
      return_model_fits = TRUE,
      trim_returned_models = TRUE)

round(res_est_nonpooled$est, 2)
round(res_est_nonstacked$est, 2)
round(res_est_stacked$est, 2)

n_boot <- 25

set.seed(1234)
res_ci_nonpooled <- get_CI(res_est_nonpooled, data = dat_reduced, n_boot = n_boot, contrast_type = 'ratio')

round(res_ci_nonpooled$res_boot$`0`, 2)
round(res_ci_nonpooled$res_boot$`1`, 2)
round(res_ci_nonpooled$res_boot_contrast$`1`, 2)


set.seed(1234)
res_ci_nonstacked <- get_CI(res_est_nonstacked, data = dat_reduced, n_boot = n_boot, contrast_type = 'ratio')

round(res_ci_nonstacked$res_boot$`0`, 2)
round(res_ci_nonstacked$res_boot$`1`, 2)
round(res_ci_nonstacked$res_boot_contrast$`1`, 2)

set.seed(1234)
res_ci_stacked <- get_CI(res_est_stacked, data = dat_reduced, n_boot = n_boot, contrast_type = 'ratio')

round(res_ci_stacked$res_boot$`0`, 2)
round(res_ci_stacked$res_boot$`1`, 2)
round(res_ci_stacked$res_boot_contrast$`1`, 2)


save.image('results-05-13-2025-binary-stacked.RData')
