setwd('/apps/biostats_final_project_data1/admin/sean')

################################################################################
## Step 1: Renaming columns and subsetting the data
################################################################################

library("haven")
library('data.table')
dat <- read_sas('forsean_20250418.sas7bdat')

## Renaming columns
colnames(dat)[colnames(dat) == 'month'] <- 'time'
colnames(dat)[colnames(dat) == 'rx_t'] <- 'A'
colnames(dat)[colnames(dat) == 'weight_t1_ind'] <- 'R'
colnames(dat)[colnames(dat) == 'wtchg'] <- 'Y'
colnames(dat)[colnames(dat) == 'death_t1'] <- 'D'
colnames(dat)[colnames(dat) == 'subjid'] <- 'id'
dat$Z <- ifelse(dat$treatment == 'sertraline', 0, 1)

# Take a subsample for computational ease
dat_reduced <- dat[dat$depression_t0 == 1 &
                     dat$bmi_t0 < 30, ]

# Only need times 0 through 23
dat_reduced <- dat_reduced[dat_reduced$time <= 23, ]

dat_reduced <- as.data.table(dat_reduced)

save(dat_reduced, file = 'dat_depressed_nonobese.RData')


################################################################################
## Step 2: Forming the analytic subset
################################################################################
rm(list = ls())
load('dat_depressed_nonobese.RData')

dat_reduced$id <- match(dat_reduced$id, unique(dat_reduced$id))

# Remove problematic ids
problematic_ids <- tapply(dat_reduced$time, dat_reduced$id,
                          FUN = function(x){
                            !isTRUE(all.equal(x, 0:(length(x) - 1)))
                          })
dat_reduced <- dat_reduced[!dat_reduced$id %in% as.numeric(names(problematic_ids)[problematic_ids]), ]
dat_reduced$id <- match(dat_reduced$id, unique(dat_reduced$id))
problematic_ids <- NULL

# Intervention: Grace period of length 2
dat_reduced$grace_end_t <- NULL
dat_reduced[, lag_t_rx := shift(t_rx, 1L, type = 'lag'), by = id]
dat_reduced$grace_end_t <- ifelse(dat_reduced$lag_t_rx == 0, 1, 0) # 2 month grace period

dat_reduced[, protocol_violation := !is.na(grace_end_t) & grace_end_t & A == 0]
dat_reduced[, first_violation_time := {
  idx <- which(protocol_violation)
  if (length(idx) > 0) as.double(time[idx[1]]) else Inf
}, by = id]

dat_reduced[, A_model_eligible := ifelse(!is.na(grace_end_t) & grace_end_t == 1 & time <= first_violation_time, 1, 0)]
dat_reduced[, C_artificial := ifelse(time >= first_violation_time, 1, 0)]


# Create _any type covariates
vars_to_condense <- c('Hyperthyroidism', 'Hypothyroidism', 'T1D', 'T2D', 'Growth_conditions',
                      'Abnormal_glucose', 'PCOS', 'OCD', 'PTSD', 'Eating_disorders',
                      'Bipolar', 'Anxiety', 'Mental_health_dis', 'Neuropathic_pain', 'Migraine',
                      'ADHD', 'Asthma', 'Liver_Cirrhosis', 'Heart_Failure', 'Chronic_Kidney_Disease', 'CCI_Comorb',
                      'Alcohol_abuse', 'Drug_abuse', 'cancer', 'bariatric', 'pregnant')
for (var in vars_to_condense){
  old_var_t0_pt1 <- paste0(var, '_dxlt12_t0')
  old_var_t0_pt2 <- paste0(var, '_dxge12_t0')

  old_var_pt1 <- paste0(var, '_dxlt12')
  old_var_pt2 <- paste0(var, '_dxge12')

  new_var_t0 <- paste0(var, '_t0_any')
  new_var_t <- paste0(var, '_any')

  dat_reduced[, (new_var_t0) := ifelse(get(old_var_t0_pt1) + get(old_var_t0_pt2) >= 1, 1, 0)]
  dat_reduced[, (new_var_t) := ifelse(get(old_var_pt1) + get(old_var_pt2) >= 1, 1, 0)]
}

means <- rep(NA, times = length(vars_to_condense))
names(means) <- vars_to_condense
i <- 1
for (var in vars_to_condense){
  colname <- paste0(var, '_t0_any')
  means[i] <- sum(c(dat_reduced[dat_reduced$time == 0, ][[colname]]))
  i <- i + 1
}
sort(means)
round(sort(means) / length(unique(dat_reduced$id)), 2)

sum(dat_reduced[dat_reduced$time == 0, ]$cov_wtloss_t0)
sum(dat_reduced[dat_reduced$time == 0, ]$cov_stimulants_t0)
mean(dat_reduced[dat_reduced$time == 0, ]$cov_stimulants_t0)

# Further subset the data
dat_reduced <- dat_reduced[dat_reduced$cov_wtloss_t0 == 0 &
                             dat_reduced$cov_stimulants_t0 == 0 &
                             dat_reduced$Eating_disorders_t0_any == 0 &
                             dat_reduced$PCOS_t0_any == 0 &
                             dat_reduced$OCD_t0_any == 0 &
                             dat_reduced$cancer_t0_any == 0 &
                             dat_reduced$Hyperthyroidism_t0_any == 0 &
                             dat_reduced$Growth_conditions_any == 0 &
                             dat_reduced$Liver_Cirrhosis_t0_any == 0 &
                             dat_reduced$pregnant_t0_any == 0 &
                             dat_reduced$T1D_t0_any == 0 &
                             dat_reduced$ADHD_t0_any == 0 &
                             dat_reduced$PTSD_t0_any == 0
                           , ]
dat_reduced$id <- match(dat_reduced$id, unique(dat_reduced$id))

# Combine similar variables
dat_reduced$antipsychotic_t0_any <-
  ifelse(dat_reduced$FGA_rxlt15_t0 == 1 |
           dat_reduced$SGA_rxlt15_t0 == 1, 1, 0)
dat_reduced$antipsychotic_any <-
  ifelse(dat_reduced$FGA_rxlt15 == 1 |
           dat_reduced$SGA_rxlt15 == 1, 1, 0)

dat_reduced$antidiabetic_t0_any <-
  ifelse(dat_reduced$Insulin_rxlt15_t0 == 1 |
           dat_reduced$Biguanide_rxlt15_t0 == 1 |
           dat_reduced$GLP1_rxlt15_t0 == 1 |
           dat_reduced$SGLT2_rxlt15_t0 == 1 |
           dat_reduced$OtherDIAB_rxlt15_t0, 1, 0)
dat_reduced$antidiabetic_any <-
  ifelse(dat_reduced$Insulin_rxlt15 == 1 |
           dat_reduced$Biguanide_rxlt15 == 1 |
           dat_reduced$GLP1_rxlt15 == 1 |
           dat_reduced$SGLT2_rxlt15 == 1 |
           dat_reduced$OtherDIAB_rxlt15, 1, 0)

dat_reduced$site_cat <-
  ifelse(dat_reduced$site %in% c(2, 6, 8, 9, 10, 11), 0, dat_reduced$site)
dat_reduced$site_cat <- as.factor(dat_reduced$site_cat)

dat_reduced$race_cat <-
  ifelse(dat_reduced$race_white == 1, 1,
         ifelse(dat_reduced$race_black == 1, 0, 2))
dat_reduced$race_cat <- as.factor(dat_reduced$race_cat)

save.image(file = 'dat_cleaned.RData')


################################################################################
## Step 3: Descriptive analyses
################################################################################
rm(list = ls())

load('dat_cleaned.RData')
library('table1')
library('xtable')


dat_reduced_baseline <- dat_reduced[dat_reduced$time == 0, ]
dat_reduced_baseline$overweight_t0 <- dat_reduced_baseline$bmi_t0 >= 25

tab1 <- table1(~age_t0 + as.factor(female) + race_cat +
                 as.factor(1 - hispanic) +
                 weight_t0 +bmi_t0 +
                 as.factor(overweight_t0) +
                 as.factor(Anxiety_t0_any) +
                 as.factor(Neuropathic_pain_t0_any) +
                 as.factor(Mental_health_dis_t0_any) +
                 as.factor(antipsychotic_t0_any) +
                 as.factor(antidiabetic_t0_any) +
                 as.factor(HTN_rxlt15_t0) +
                 as.factor(AED_rxlt15_t0)
               | factor(Z),
               data = dat_reduced_baseline)
tab1
print(xtable(as.data.frame(tab1)), include.rownames = F)


get_deaths <- function(df){
  return(list(num = sum(df$D),
              perc = round(100 * sum(df$D) / n, 2)))
}

n <- length(unique(dat_reduced$id))
get_deaths(dat_reduced[dat_reduced$time <= 5, ])
get_deaths(dat_reduced[dat_reduced$time <= 11, ])
get_deaths(dat_reduced[dat_reduced$time <= 17, ])
get_deaths(dat_reduced)

round(100 - mean(dat_reduced[dat_reduced$time == 5, ]$R) * 100, 1)
round(100 - mean(dat_reduced[dat_reduced$time == 11, ]$R) * 100, 1)
round(100 - mean(dat_reduced[dat_reduced$time == 17, ]$R) * 100, 1)
round(100 - mean(dat_reduced[dat_reduced$time == 23, ]$R) * 100, 1)

dat_reduced$C_artificial_bool <- ifelse(dat_reduced$C_artificial == 1, TRUE, FALSE)
get_artcens <- function(df){
  return(round(100 * mean(tapply(df$C_artificial_bool, df$id, any)), 1))
}
get_artcens(dat_reduced[dat_reduced$time <= 5, ])
get_artcens(dat_reduced[dat_reduced$time <= 11, ])
get_artcens(dat_reduced[dat_reduced$time <= 17, ])
get_artcens(dat_reduced[dat_reduced$time <= 23, ])


## Plot of death timing
pdf('/apps/biostats_final_project_data1/admin/sean/deaths-hist.pdf')
hist(dat_reduced[dat_reduced$D == 1, ]$time, xlab = 'Month', main = 'Death times')
dev.off()


################################################################################
## Step 4: Applying IPW analyses -- Continuous Outcome
################################################################################

rm(list = ls())
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

library('data.table')
library('progress')
library('splines')


## Simple analysis: Removing individuals who died
source('ipw.R')
source('bootstrap.R')

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
      pooled = FALSE,
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
      pooled = TRUE,
      pooling_method = 'nonstacked',
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
      pooled = TRUE,
      pooling_method = 'stacked',
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
## Step 4: Applying IPW analyses -- Binary Outcome
################################################################################

rm(list = ls())
load('dat_cleaned.RData')


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

library('data.table')
library('progress')
library('splines')
source('ipw.R')
source('bootstrap.R')

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
      pooled = FALSE,
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
      pooled = TRUE,
      pooling_method = 'nonstacked',
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
      pooled = TRUE,
      pooling_method = 'stacked',
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
