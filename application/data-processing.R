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