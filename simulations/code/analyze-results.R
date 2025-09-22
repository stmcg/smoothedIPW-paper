rm(list = ls())

library('xtable')
library('scales')
library('RColorBrewer')
col <- brewer.pal(name = 'Set1', n = 3)[1:2]
col <- alpha(col, alpha = 0.75)
tstar <- c(6, 12, 18, 24)

################################################################################
## Null Scenario
################################################################################
load('../results/res-null.RData')

pdf('../results/simres.pdf', width = 7, height = 5)
par(mar = c(6.5, 4.1, 3, 2.1)) # For legend
boxplot(est_pooled_dif[, 1], est_nonpooled_dif[, 1], 
        est_pooled_dif[, 2], est_nonpooled_dif[, 2], 
        est_pooled_dif[, 3], est_nonpooled_dif[, 3], 
        est_pooled_dif[, 4], est_nonpooled_dif[, 4], 
        at = c(1,2,4,5,7,8,10,11), 
        ylab = 'Average Treatment Effect', 
        xlab = 'Follow-Up Time',
        xaxt = "n", col = col, 
        cex.lab = 1.25, 
        outline = FALSE)
axis(side = 1, at = c(1.5, 4.5, 7.5, 10.5), 
     labels = c("t*=6", "t*=12", "t*=18", "t*=24"), tick = FALSE)
abline(h = 0, col = 'red', lwd = 2, lty = 2)
# For legend
par(xpd = TRUE)  # Allow plotting outside the plotting region
legend("bottom", inset = -0.43, legend = c("Time-Smoothed IPW", "Non-Smoothed IPW"), 
       fill = col[1:2], bty = "n", cex = 1.05, horiz = TRUE)
par(xpd = FALSE)
dev.off()


pdf('../results/simres_z0.pdf', width = 7, height = 5)
par(mar = c(6.5, 4.1, 3, 2.1)) # For legend
boxplot(est_pooled_z0[, 1], est_nonpooled_z0[, 1], 
        est_pooled_z0[, 2], est_nonpooled_z0[, 2], 
        est_pooled_z0[, 3], est_nonpooled_z0[, 3], 
        est_pooled_z0[, 4], est_nonpooled_z0[, 4], 
        at = c(1,2,4,5,7,8,10,11), 
        main = 'Medication Z=0',
        ylab = 'Counterfactual Outcome Mean', 
        xlab = 'Follow-Up Time',
        xaxt = "n", col = col, 
        cex.lab = 1.25, cex.main = 1.25, 
        outline = FALSE)
axis(side = 1, at = c(1.5, 4.5, 7.5, 10.5), 
     labels = c("t*=6", "t*=12", "t*=18", "t*=24"), tick = FALSE)
abline(h = 0, col = 'red', lwd = 2, lty = 2)
# For legend
par(xpd = TRUE)  # Allow plotting outside the plotting region
legend("bottom", inset = -0.43, legend = c("Time-Smoothed IPW", "Non-Smoothed IPW"), 
       fill = col[1:2], bty = "n", cex = 1.05, horiz = TRUE)
par(xpd = FALSE)
dev.off()


pdf('../results/simres_z1.pdf', width = 7, height = 5)
par(mar = c(6.5, 4.1, 3, 2.1)) # For legend
boxplot(est_pooled_z1[, 1], est_nonpooled_z1[, 1], 
        est_pooled_z1[, 2], est_nonpooled_z1[, 2], 
        est_pooled_z1[, 3], est_nonpooled_z1[, 3], 
        est_pooled_z1[, 4], est_nonpooled_z1[, 4], 
        at = c(1,2,4,5,7,8,10,11), 
        main = 'Medication Z=1',
        ylab = 'Counterfactual Outcome Mean', 
        xlab = 'Follow-Up Time',
        xaxt = "n", col = col, 
        cex.lab = 1.25, cex.main = 1.25, 
        outline = FALSE)
axis(side = 1, at = c(1.5, 4.5, 7.5, 10.5), 
     labels = c("t*=6", "t*=12", "t*=18", "t*=24"), tick = FALSE)
abline(h = 0, col = 'red', lwd = 2, lty = 2)
# For legend
par(xpd = TRUE)  # Allow plotting outside the plotting region
legend("bottom", inset = -0.43, legend = c("Time-Smoothed IPW", "Non-Smoothed IPW"), 
       fill = col[1:2], bty = "n", cex = 1.05, horiz = TRUE)
par(xpd = FALSE)
dev.off()


latex_table_null <- function(est_pooled, ci_lb_pooled, ci_ub_pooled,
                              est_nonpooled, ci_lb_nonpooled, ci_ub_nonpooled){
  bias_pooled <- round(100 * apply(est_pooled, 2, mean), 2)
  bias_nonpooled <- round(100 * apply(est_nonpooled, 2, mean), 2)
  
  se_pooled <- round(10 * apply(est_pooled, 2, sd), 2)
  se_nonpooled <- round(10 * apply(est_nonpooled, 2, sd), 2)
  
  cov_pooled <- round(colMeans((ci_lb_pooled < 0) & (ci_ub_pooled > 0)), 2)
  cov_nonpooled <- round(colMeans((ci_lb_nonpooled < 0) & (ci_ub_nonpooled > 0)), 2)
  
  table_mat <- cbind(tstar, 
                     bias_pooled, se_pooled, cov_pooled,
                     bias_nonpooled, se_nonpooled, cov_nonpooled)
  print(xtable(table_mat, digits = c(0, 0, 2, 2, 2, 2, 2, 2)), include.rownames = F)
}

latex_table_null(est_pooled = est_pooled_dif, 
                       ci_lb_pooled = ci_lb_pooled_dif, 
                       ci_ub_pooled = ci_ub_pooled_dif, 
                       est_nonpooled = est_nonpooled_dif, 
                       ci_lb_nonpooled = ci_lb_nonpooled_dif, 
                       ci_ub_nonpooled = ci_ub_nonpooled_dif)
latex_table_null(est_pooled = est_pooled_z0, 
                       ci_lb_pooled = ci_lb_pooled_z0, 
                       ci_ub_pooled = ci_ub_pooled_z0, 
                       est_nonpooled = est_nonpooled_z0, 
                       ci_lb_nonpooled = ci_lb_nonpooled_z0, 
                       ci_ub_nonpooled = ci_ub_nonpooled_z0)
latex_table_null(est_pooled = est_pooled_z1, 
                       ci_lb_pooled = ci_lb_pooled_z1, 
                       ci_ub_pooled = ci_ub_pooled_z1, 
                       est_nonpooled = est_nonpooled_z1, 
                       ci_lb_nonpooled = ci_lb_nonpooled_z1, 
                       ci_ub_nonpooled = ci_ub_nonpooled_z1)

# ATE

round(100 * apply(est_pooled_dif, 2, mean), 2)
round(100 * apply(est_nonpooled_dif, 2, mean), 2)

round(10 * apply(est_pooled_dif, 2, sd), 2)
round(10 * apply(est_nonpooled_dif, 2, sd), 2)

round(colMeans((ci_lb_pooled_dif < 0) & (ci_ub_pooled_dif > 0)), 2)
round(colMeans((ci_lb_nonpooled_dif < 0) & (ci_ub_nonpooled_dif > 0)), 2)

# Z=0

round(100 * (apply(est_pooled_z0, 2, mean)), 2)
round(100 * (apply(est_nonpooled_z0, 2, mean)), 2)

round(10 * apply(est_pooled_z0, 2, sd), 2)
round(10 * apply(est_nonpooled_z0, 2, sd), 2)

round(colMeans((ci_lb_pooled_z0 < 0) & (ci_ub_pooled_z0 > 0)), 2)
round(colMeans((ci_lb_nonpooled_z0 < 0) & (ci_ub_nonpooled_z0 > 0)), 2)

# Z=1

round(100 * (apply(est_pooled_z1, 2, mean)), 2)
round(100 * (apply(est_nonpooled_z1, 2, mean)), 2)

round(10 * apply(est_pooled_z1, 2, sd), 2)
round(10 * apply(est_nonpooled_z1, 2, sd), 2)

round(colMeans((ci_lb_pooled_z1 < 0) & (ci_ub_pooled_z1 > 0)), 2)
round(colMeans((ci_lb_nonpooled_z1 < 0) & (ci_ub_nonpooled_z1 > 0)), 2)

round(apply(est_nonpooled_dif, 2, var) / apply(est_pooled_dif, 2, var), 2)





################################################################################
## Nonnull Scenario
################################################################################
load('../results/res-nonnull.RData')

pdf('../results/simres_nonnull.pdf', width = 7, height = 5)
par(mar = c(6.5, 4.1, 3, 2.1)) # For legend
boxplot(est_pooled_dif[, 1], est_nonpooled_dif[, 1], 
        est_pooled_dif[, 2], est_nonpooled_dif[, 2], 
        est_pooled_dif[, 3], est_nonpooled_dif[, 3], 
        est_pooled_dif[, 4], est_nonpooled_dif[, 4], 
        at = c(1,2,4,5,7,8,10,11), 
        ylab = 'Average Treatment Effect', 
        xlab = 'Follow-Up Time',
        xaxt = "n", col = col, 
        cex.lab = 1.25, 
        outline = FALSE)
points(c(1.5, 4.5, 7.5, 10.5), truth_dif, col = 'red', pch = 4, cex = 1.5)
axis(side = 1, at = c(1.5, 4.5, 7.5, 10.5), 
     labels = c("t*=6", "t*=12", "t*=18", "t*=24"), tick = FALSE)
# For legend
par(xpd = TRUE)  # Allow plotting outside the plotting region
legend("bottom", inset = -0.43, legend = c("Time-Smoothed IPW", "Non-Smoothed IPW"), 
       fill = col[1:2], bty = "n", cex = 1.05, horiz = TRUE)
par(xpd = FALSE)
dev.off()


pdf('../results/simres_z0_nonnull.pdf', width = 7, height = 5)
par(mar = c(6.5, 4.1, 3, 2.1)) # For legend
boxplot(est_pooled_z0[, 1], est_nonpooled_z0[, 1], 
        est_pooled_z0[, 2], est_nonpooled_z0[, 2], 
        est_pooled_z0[, 3], est_nonpooled_z0[, 3], 
        est_pooled_z0[, 4], est_nonpooled_z0[, 4], 
        at = c(1,2,4,5,7,8,10,11), 
        main = 'Medication Z=0',
        ylab = 'Counterfactual Outcome Mean', 
        xlab = 'Follow-Up Time',
        xaxt = "n", col = col, 
        cex.lab = 1.25, cex.main = 1.25, 
        outline = FALSE)
points(c(1.5, 4.5, 7.5, 10.5), truth_z0, col = 'red', pch = 4, cex = 1.5)
axis(side = 1, at = c(1.5, 4.5, 7.5, 10.5), 
     labels = c("t*=6", "t*=12", "t*=18", "t*=24"), tick = FALSE)
# For legend
par(xpd = TRUE)  # Allow plotting outside the plotting region
legend("bottom", inset = -0.43, legend = c("Time-Smoothed IPW", "Non-Smoothed IPW"), 
       fill = col[1:2], bty = "n", cex = 1.05, horiz = TRUE)
par(xpd = FALSE)
dev.off()


pdf('../results/simres_z1_nonnull.pdf', width = 7, height = 5)
par(mar = c(6.5, 4.1, 3, 2.1)) # For legend
boxplot(est_pooled_z1[, 1], est_nonpooled_z1[, 1], 
        est_pooled_z1[, 2], est_nonpooled_z1[, 2], 
        est_pooled_z1[, 3], est_nonpooled_z1[, 3], 
        est_pooled_z1[, 4], est_nonpooled_z1[, 4], 
        at = c(1,2,4,5,7,8,10,11), 
        main = 'Medication Z=1',
        ylab = 'Counterfactual Outcome Mean', 
        xlab = 'Follow-Up Time',
        xaxt = "n", col = col, 
        cex.lab = 1.25, cex.main = 1.25, 
        outline = FALSE)
points(c(1.5, 4.5, 7.5, 10.5), truth_z1, col = 'red', pch = 4, cex = 1.5)
axis(side = 1, at = c(1.5, 4.5, 7.5, 10.5), 
     labels = c("t*=6", "t*=12", "t*=18", "t*=24"), tick = FALSE)
# For legend
par(xpd = TRUE)  # Allow plotting outside the plotting region
legend("bottom", inset = -0.43, legend = c("Time-Smoothed IPW", "Non-Smoothed IPW"), 
       fill = col[1:2], bty = "n", cex = 1.05, horiz = TRUE)
par(xpd = FALSE)
dev.off()

latex_table_nonnull <- function(est_pooled, ci_lb_pooled, ci_ub_pooled,
                                   est_nonpooled, ci_lb_nonpooled, ci_ub_nonpooled, 
                                   truth){
  bias_pooled <- round(10 * (apply(est_pooled, 2, mean) - truth), 2)
  bias_nonpooled <- round(10 * (apply(est_nonpooled, 2, mean) - truth), 2)
  
  se_pooled <- round(10 * apply(est_pooled, 2, sd), 2)
  se_nonpooled <- round(10 * apply(est_nonpooled, 2, sd), 2)
  
  truth_mat <- matrix(rep(truth, each = n_reps), nrow = n_reps)
  cov_pooled <- round(colMeans((ci_lb_pooled < truth_mat) & (ci_ub_pooled > truth_mat)), 2)
  cov_nonpooled <- round(colMeans((ci_lb_nonpooled < truth_mat) & (ci_ub_nonpooled > truth_mat)), 2)
  
  table_mat <- cbind(tstar, 
                     bias_pooled, se_pooled, cov_pooled,
                     bias_nonpooled, se_nonpooled, cov_nonpooled)
  print(xtable(table_mat, digits = c(0, 0, 2, 2, 2, 2, 2, 2)), include.rownames = F)
}

latex_table_nonnull(est_pooled = est_pooled_dif, 
                       ci_lb_pooled = ci_lb_pooled_dif, 
                       ci_ub_pooled = ci_ub_pooled_dif, 
                       est_nonpooled = est_nonpooled_dif, 
                       ci_lb_nonpooled = ci_lb_nonpooled_dif, 
                       ci_ub_nonpooled = ci_ub_nonpooled_dif, 
                       truth = truth_dif)
latex_table_nonnull(est_pooled = est_pooled_z0, 
                          ci_lb_pooled = ci_lb_pooled_z0, 
                          ci_ub_pooled = ci_ub_pooled_z0, 
                          est_nonpooled = est_nonpooled_z0, 
                          ci_lb_nonpooled = ci_lb_nonpooled_z0, 
                          ci_ub_nonpooled = ci_ub_nonpooled_z0, 
                          truth = truth_z0)
latex_table_nonnull(est_pooled = est_pooled_z1, 
                          ci_lb_pooled = ci_lb_pooled_z1, 
                          ci_ub_pooled = ci_ub_pooled_z1, 
                          est_nonpooled = est_nonpooled_z1, 
                          ci_lb_nonpooled = ci_lb_nonpooled_z1, 
                          ci_ub_nonpooled = ci_ub_nonpooled_z1, 
                          truth = truth_z1)


truth_dif_mat <- matrix(rep(truth_dif, each = n_reps), nrow = n_reps)
truth_z0_mat <- matrix(rep(truth_z0, each = n_reps), nrow = n_reps)
truth_z1_mat <- matrix(rep(truth_z1, each = n_reps), nrow = n_reps)

# ATE
round(10 * (apply(est_pooled_dif, 2, mean) - truth_dif), 2)
round(10 * (apply(est_nonpooled_dif, 2, mean) - truth_dif), 2)

round(10 * apply(est_pooled_dif, 2, sd), 2)
round(10 * apply(est_nonpooled_dif, 2, sd), 2)

round(colMeans((ci_lb_pooled_dif < truth_dif_mat) & (ci_ub_pooled_dif > truth_dif_mat)), 2)
round(colMeans((ci_lb_nonpooled_dif < truth_dif_mat) & (ci_ub_nonpooled_dif > truth_dif_mat)), 2)

# Z=0
round(10 * (apply(est_pooled_z0, 2, mean) - truth_z0), 2)
round(10 * (apply(est_nonpooled_z0, 2, mean) - truth_z0), 2)

round(10 * apply(est_pooled_z0, 2, sd), 2)
round(10 * apply(est_nonpooled_z0, 2, sd), 2)

round(colMeans((ci_lb_pooled_z0 < truth_z0_mat) & (ci_ub_pooled_z0 > truth_z0_mat)), 2)
round(colMeans((ci_lb_nonpooled_z0 < truth_z0_mat) & (ci_ub_nonpooled_z0 > truth_z0_mat)), 2)

# Z=1
round(10 * (apply(est_pooled_z1, 2, mean) - truth_z1), 2)
round(10 * (apply(est_nonpooled_z1, 2, mean) - truth_z1), 2)

round(10 * apply(est_pooled_z1, 2, sd), 2)
round(10 * apply(est_nonpooled_z1, 2, sd), 2)

round(colMeans((ci_lb_pooled_z1 < truth_z1_mat) & (ci_ub_pooled_z1 > truth_z1_mat)), 2)
round(colMeans((ci_lb_nonpooled_z1 < truth_z1_mat) & (ci_ub_nonpooled_z1 > truth_z1_mat)), 2)

round(apply(est_nonpooled_dif, 2, var) / apply(est_pooled_dif, 2, var), 2)


# Checking correctness of "true" values
boxplot(est_nonpooled_dif)
points(truth_dif, col = 'red')

boxplot(est_nonpooled_z0)
points(truth_z0, col = 'red')

boxplot(est_nonpooled_z1)
points(truth_z1, col = 'red')




################################################################################
## Null Scenario with deaths
################################################################################
library('scales')
library('RColorBrewer')
col <- brewer.pal(name = 'Set1', n = 4)[c(4,3,2)]
col <- alpha(col, alpha = 0.75)

load('../results/res-null-deaths.RData')

pdf('../results/simres-deaths.pdf', width = 7, height = 5)
par(mar = c(6.5, 4.1, 3, 2.1)) # For legend
boxplot(est_stacked_dif[, 1], est_nonstacked_dif[, 1], est_nonpooled_dif[, 1], 
        est_stacked_dif[, 2], est_nonstacked_dif[, 2], est_nonpooled_dif[, 2], 
        est_stacked_dif[, 3], est_nonstacked_dif[, 3], est_nonpooled_dif[, 3], 
        est_stacked_dif[, 4], est_nonstacked_dif[, 4], est_nonpooled_dif[, 4], 
        at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15), 
        ylab = 'Average Treatment Effect', 
        xlab = 'Follow-Up Time',
        xaxt = "n", col = col, 
        cex.lab = 1.25, 
        outline = FALSE)
axis(side = 1, at = c(2, 6, 10, 14), 
     labels = c("t*=6", "t*=12", "t*=18", "t*=24"), tick = FALSE)
abline(h = 0, col = 'red', lwd = 2, lty = 2)
# For legend
par(xpd = TRUE)  # Allow plotting outside the plotting region
legend("bottom", inset = -0.43, 
       legend = c("Stacked IPW", "Nonstacked IPW", "Non-Smoothed IPW"), 
       fill = col[1:3], bty = "n", cex = 1.05, horiz = TRUE)
par(xpd = FALSE)
dev.off()

pdf('../results/simres_z0-deaths.pdf', width = 7, height = 5)
par(mar = c(6.5, 4.1, 3, 2.1)) # For legend
boxplot(est_stacked_z0[, 1], est_nonstacked_z0[, 1], est_nonpooled_z0[, 1], 
        est_stacked_z0[, 2], est_nonstacked_z0[, 2], est_nonpooled_z0[, 2], 
        est_stacked_z0[, 3], est_nonstacked_z0[, 3], est_nonpooled_z0[, 3], 
        est_stacked_z0[, 4], est_nonstacked_z0[, 4], est_nonpooled_z0[, 4], 
        at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15), 
        main = 'Medication Z=0',
        ylab = 'Counterfactual Outcome Mean', 
        xlab = 'Follow-Up Time',
        xaxt = "n", col = col, 
        cex.lab = 1.25, cex.main = 1.25, 
        outline = FALSE)
axis(side = 1, at = c(2, 6, 10, 14), 
     labels = c("t*=6", "t*=12", "t*=18", "t*=24"), tick = FALSE)
points(c(2, 6, 10, 14), truth_z0, col = 'red', pch = 4, cex = 1.5)
# For legend
par(xpd = TRUE)  # Allow plotting outside the plotting region
legend("bottom", inset = -0.43, 
       legend = c("Stacked IPW", "Nonstacked IPW", "Non-Smoothed IPW"), 
       fill = col[1:3], bty = "n", cex = 1.05, horiz = TRUE)
par(xpd = FALSE)
dev.off()


pdf('../results/simres_z1-deaths.pdf', width = 7, height = 5)
par(mar = c(6.5, 4.1, 3, 2.1)) # For legend
boxplot(est_stacked_z1[, 1], est_nonstacked_z1[, 1], est_nonpooled_z1[, 1], 
        est_stacked_z1[, 2], est_nonstacked_z1[, 2], est_nonpooled_z1[, 2], 
        est_stacked_z1[, 3], est_nonstacked_z1[, 3], est_nonpooled_z1[, 3], 
        est_stacked_z1[, 4], est_nonstacked_z1[, 4], est_nonpooled_z1[, 4], 
        at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15), 
        main = 'Medication Z=1',
        ylab = 'Counterfactual Outcome Mean', 
        xlab = 'Follow-Up Time',
        xaxt = "n", col = col, 
        cex.lab = 1.25, cex.main = 1.25, 
        outline = FALSE)
axis(side = 1, at = c(2, 6, 10, 14), 
     labels = c("t*=6", "t*=12", "t*=18", "t*=24"), tick = FALSE)
points(c(2, 6, 10, 14), truth_z1, col = 'red', pch = 4, cex = 1.5)
# For legend
par(xpd = TRUE)  # Allow plotting outside the plotting region
legend("bottom", inset = -0.43, 
       legend = c("Stacked IPW", "Nonstacked IPW", "Non-Smoothed IPW"), 
       fill = col[1:3], bty = "n", cex = 1.05, horiz = TRUE)
par(xpd = FALSE)
dev.off()

latex_table_null_deaths <- function(est_nonstacked, ci_lb_nonstacked, ci_ub_nonstacked,
                                    est_stacked, ci_lb_stacked, ci_ub_stacked,
                                    est_nonpooled, ci_lb_nonpooled, ci_ub_nonpooled, 
                                    truth){
  
  truth_mat <- matrix(rep(truth, each = n_reps), nrow = n_reps)
  
  bias_nonstacked <- round(10 * (apply(est_nonstacked, 2, mean) - truth), 2)
  bias_stacked <- round(10 * (apply(est_stacked, 2, mean) - truth), 2)
  bias_nonpooled <- round(10 * (apply(est_nonpooled, 2, mean) - truth), 2)
  
  se_nonstacked <- round(10 * apply(est_nonstacked, 2, sd), 2)
  se_stacked <- round(10 * apply(est_stacked, 2, sd), 2)
  se_nonpooled <- round(10 * apply(est_nonpooled, 2, sd), 2)
  
  truth_mat <- matrix(rep(truth, each = n_reps), nrow = n_reps)
  cov_nonstacked <- round(colMeans((ci_lb_nonstacked < truth_mat) & (ci_ub_nonstacked > truth_mat)), 2)
  cov_stacked <- round(colMeans((ci_lb_stacked < truth_mat) & (ci_ub_stacked > truth_mat)), 2)
  cov_nonpooled <- round(colMeans((ci_lb_nonpooled < truth_mat) & (ci_ub_nonpooled > truth_mat)), 2)
  
  table_mat <- cbind(tstar, 
                     bias_nonstacked, se_nonstacked, cov_nonstacked,
                     bias_stacked, se_stacked, cov_stacked,
                     bias_nonpooled, se_nonpooled, cov_nonpooled)
  print(xtable(table_mat, digits = c(0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2)), include.rownames = F)
}

latex_table_null_deaths(est_nonstacked = est_nonstacked_dif, ci_lb_nonstacked = ci_lb_nonstacked_dif, ci_ub_nonstacked = ci_ub_nonstacked_dif, 
                        est_stacked = est_stacked_dif, ci_lb_stacked = ci_lb_stacked_dif, ci_ub_stacked = ci_ub_stacked_dif, 
                        est_nonpooled = est_nonpooled_dif, ci_lb_nonpooled = ci_lb_nonpooled_dif, ci_ub_nonpooled = ci_ub_nonpooled_dif, 
                        truth = rep(0, times = 4))
latex_table_null_deaths(est_nonstacked = est_nonstacked_z0, ci_lb_nonstacked = ci_lb_nonstacked_z0, ci_ub_nonstacked = ci_ub_nonstacked_z0, 
                        est_stacked = est_stacked_z0, ci_lb_stacked = ci_lb_stacked_z0, ci_ub_stacked = ci_ub_stacked_z0, 
                        est_nonpooled = est_nonpooled_z0, ci_lb_nonpooled = ci_lb_nonpooled_z0, ci_ub_nonpooled = ci_ub_nonpooled_z0, 
                        truth = truth_z0)
latex_table_null_deaths(est_nonstacked = est_nonstacked_z1, ci_lb_nonstacked = ci_lb_nonstacked_z1, ci_ub_nonstacked = ci_ub_nonstacked_z1, 
                        est_stacked = est_stacked_z1, ci_lb_stacked = ci_lb_stacked_z1, ci_ub_stacked = ci_ub_stacked_z1, 
                        est_nonpooled = est_nonpooled_z1, ci_lb_nonpooled = ci_lb_nonpooled_z1, ci_ub_nonpooled = ci_ub_nonpooled_z1, 
                        truth = truth_z1)



truth_dif_mat <- matrix(rep(0, each = n_reps), nrow = n_reps)
truth_z0_mat <- matrix(rep(truth_z0, each = n_reps), nrow = n_reps)
truth_z1_mat <- matrix(rep(truth_z1, each = n_reps), nrow = n_reps)

# ATE
round(10 * apply(est_stacked_dif, 2, mean), 2)
round(10 * apply(est_nonstacked_dif, 2, mean), 2)
round(10 * apply(est_nonpooled_dif, 2, mean), 2)

round(10 * apply(est_stacked_dif, 2, sd), 2)
round(10 * apply(est_nonstacked_dif, 2, sd), 2)
round(10 * apply(est_nonpooled_dif, 2, sd), 2)

round(colMeans((ci_lb_stacked_dif < 0) & (ci_ub_stacked_dif > 0)), 2)
round(colMeans((ci_lb_nonstacked_dif < 0) & (ci_ub_nonstacked_dif > 0)), 2)
round(colMeans((ci_lb_nonpooled_dif < 0) & (ci_ub_nonpooled_dif > 0)), 2)

# Z=0
round(10 * (apply(est_stacked_z0, 2, mean) - truth_z0), 2)
round(10 * (apply(est_nonstacked_z0, 2, mean) - truth_z0), 2)
round(10 * (apply(est_nonpooled_z0, 2, mean) - truth_z0), 2)

round(10 * apply(est_stacked_z0, 2, sd), 2)
round(10 * apply(est_nonstacked_z0, 2, sd), 2)
round(10 * apply(est_nonpooled_z0, 2, sd), 2)

round(colMeans((ci_lb_stacked_z0 < truth_z0_mat) & (ci_ub_stacked_z0 > truth_z0_mat)), 2)
round(colMeans((ci_lb_nonstacked_z0 < truth_z0_mat) & (ci_ub_nonstacked_z0 > truth_z0_mat)), 2)
round(colMeans((ci_lb_nonpooled_z0 < truth_z0_mat) & (ci_ub_nonpooled_z0 > truth_z0_mat)), 2)

# Z=1
round(10 * (apply(est_stacked_z1, 2, mean) - truth_z1), 2)
round(10 * (apply(est_nonstacked_z1, 2, mean) - truth_z1), 2)
round(10 * (apply(est_nonpooled_z1, 2, mean) - truth_z1), 2)

round(10 * apply(est_stacked_z1, 2, sd), 2)
round(10 * apply(est_nonstacked_z1, 2, sd), 2)
round(10 * apply(est_nonpooled_z1, 2, sd), 2)

round(colMeans((ci_lb_stacked_z1 < truth_z1_mat) & (ci_ub_stacked_z1 > truth_z1_mat)), 2)
round(colMeans((ci_lb_nonstacked_z1 < truth_z1_mat) & (ci_ub_nonstacked_z1 > truth_z1_mat)), 2)
round(colMeans((ci_lb_nonpooled_z1 < truth_z1_mat) & (ci_ub_nonpooled_z1 > truth_z1_mat)), 2)

round(apply(est_nonpooled_dif, 2, var) / apply(est_stacked_dif, 2, var), 2)
round(apply(est_nonpooled_dif, 2, var) / apply(est_nonstacked_dif, 2, var), 2)

