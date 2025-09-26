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