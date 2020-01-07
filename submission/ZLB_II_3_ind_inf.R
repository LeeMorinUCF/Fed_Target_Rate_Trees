################################################################################
# 
# Decision Trees for the Federal Reserve Target Rate Policy
# 
# Code to Accompany 
# Federal Reserve Policy after the Zero Lower Bound:
# An Indirect Inference Approach
# 
# Lee Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
# 
# Ying Shang, Ph.D.
# Associate Professor
# International School of Economics and Management
# Capital University of Economics and Business
# 
# January 4, 2020
# 
################################################################################
# 
# Estimation of decision tree models for the target interest 
# rate policy of the Federal Reserve. 
# Identification is by indirect inference during zero lower bound episode. 
# 
# This version estimates the missing shadow rate changes by indirect inference. 
# 
# Dependencies: 
#   Assumes ZLB_II_2_policy_trees.R has been run on 'ExclZLB' model. 
# 
################################################################################


################################################################################
# Load Data
################################################################################

# # No longer needed: added to original data series.
# 
# # Need complete federal funds series.
# # Specify parameters for critical values.
# file_tag <- 'MZLB_revised_'
# cv_version <- 1
# folder_name <- 'MZLB_data'
# 
# in_file_name <- sprintf('%s/%s%d.csv', 
#                         folder_name, file_tag, cv_version)
# 
# # Load table of daily interest rates and ranges. 
# mzlb_ffr <- read.csv(file = in_file_name)
# 
# colnames(mzlb_ffr)
# nrow(mzlb_ffr)
# nrow(mzlb)
# 
# mzlb_ffr <- mzlb_ffr[, c('date', 'fed_funds')]
# colnames(mzlb_ffr) <- c('date_ffr', 'fed_funds')
# 
# # Join fed_funds by appending column.
# mzlb <- cbind(mzlb, mzlb_ffr)
# 
# # Verify join.
# head(mzlb[, c('date', 'date_ffr', 'eff_ffr', 'fed_funds')])
# tail(mzlb[, c('date', 'date_ffr', 'eff_ffr', 'fed_funds')])


################################################################################
# Analyze and prepare variables for indirect inference. 
################################################################################

# Extract predictions ExclZLB model. 
sel_case <- 'ExclZLB'
target_var <- 'fed_jump'


# Verify expected jumps and class predictions by model. 
summary(mzlb[, sprintf('pred_jump_%s', sel_case)])
table(mzlb[, sprintf('pred_class_%s', sel_case)], useNA = 'ifany')

# Plot figures to inspect variablity over ZLB period. 
plot(mzlb[, sprintf('pred_jump_%s', sel_case)], 
     type = 'l')
lines(mzlb[, 'zlb_ind']*0.25, col = 'blue')
range_soma <- range(mzlb[, 'soma_hold'], na.rm = TRUE)
lines(0.2*(mzlb[, 'soma_hold'] - range_soma[1]) / 
        (range_soma[2] - range_soma[1]), col = 'red')
lines(mzlb[, 'shadow_rate']*0.1, col = 'green')

# Compare variability of predicted class over ZLB period.
table(mzlb[, sprintf('pred_class_%s', sel_case)], 
      mzlb[, 'zlb_ind'], useNA = 'ifany')
# Note there are no negative predictions (AE or ME) in ZLB 
# (none are observed with censoring at ZLB).


# Compare variability of expected jump size over ZLB period.
table(mzlb[, sprintf('pred_jump_%s', sel_case)], 
      mzlb[, 'zlb_ind'], useNA = 'ifany')


# Obtain predictions of class probabilities. 
prob_class <- sprintf('prob_%s', levels(mzlb[, target_var]))
mzlb[, prob_class] <- NA
mzlb[, prob_class] <- predict(ExclZLB_fed_tree, newdata = mzlb)
summary(mzlb[, prob_class])


# Cross-check with class probabilities. 
table(mzlb[, prob_class[5]], 
      mzlb[, 'zlb_ind'], useNA = 'ifany')


# Need only estimate the rate cut probabilities 
# for the 51 ZLB months with flat, near-zero probabilities.

# Extract estimated probabilities. 
cens_probs <- mzlb[cumsum(mzlb[, 'zlb_ind']) == 1, prob_class]
cens_pred_jump <- mzlb[cumsum(mzlb[, 'zlb_ind']) == 1, 
                       sprintf('pred_jump_%s', sel_case)]
# Verify the expected jump size.
cens_exp_jump <- sum(cens_probs*seq(-0.5,0.5, by = 0.25))
c(cens_pred_jump, cens_exp_jump)
# Check. Matches value from table above. 

# Create an indicator for the censored ZLB class. 
mzlb[, 'cens_class_ind'] <- mzlb[, 'zlb_ind'] & 
  mzlb[, sprintf('pred_jump_%s', sel_case)] == cens_pred_jump

# Trust but Verify.
table(mzlb[, sprintf('pred_jump_%s', sel_case)], 
      mzlb[, 'cens_class_ind'], useNA = 'ifany')
# 51 observations are separated from the other stable periods. 

# Next, estimate probabilities of (shadow) rate cuts for this time period.


################################################################################
# Calculate Auxilliary parameters from Data in ExclZLB period. 
################################################################################

# Simplified version with only the most critical measures of accuracy. 

# Compare with actual target rate series. 

# Should be as close as possible before and after ZLB, when target rate is observed.
#   => distance is calculated from difference from target rate. 
# Should remain negative while ZLB is in effect. 
#   => distance is calculated only from predicted target rate above zero. 

# First need to calculate expected jump sizes and expected target rates. 



# Create index for post-ZLB period.
mzlb[, 'post_zlb_ind'] <- cumsum(mzlb[, 'zlb_ind']) >= 1 &
  !mzlb[, 'zlb_ind']
mzlb[nrow(mzlb), 'post_zlb_ind'] <- FALSE


#--------------------------------------------------------------------------------
# Create an objective function for optimization based on 
# additional (auxilliary) parameters. 
# This is required to identify the second moment of the change in interest rates. 
#--------------------------------------------------------------------------------

# Run a regression of the second moment of the expected rate change 
# on the predictor variables in the model. 

# Calculate a second moment target variable. 
target_var_2nd <- 'fed_jump_st_dev' 

mzlb[, target_var_2nd] <- - mzlb[, 'pred_jump_ExclZLB']^2
prob_class <- sprintf('prob_%s', levels(mzlb[, target_var]))
# jump_values <- 0.25*(as.integer(levels(mzlb[, 'fed_jump'])) + 0)
jump_values <- c(0.25*seq(-2,2) + 0)

for (class_num in 1:length(prob_class)) {
  
  # Calculate the expected jump size. 
  mzlb[, target_var_2nd] <- mzlb[, target_var_2nd] + 
    jump_values[class_num]^2*mzlb[, prob_class[class_num]]
  
}

summary(mzlb[, target_var_2nd])
summary(sqrt(mzlb[, target_var_2nd]))

# Check for negative values.
mzlb[mzlb[, target_var_2nd] < 0, c(prob_class, target_var_2nd)]


# Restate to standard deviations. 
mzlb[, target_var_2nd] <- sqrt(mzlb[, target_var_2nd])

table(mzlb[, target_var_2nd], 
      mzlb[, 'zlb_ind'], useNA = 'ifany')
table(mzlb[, target_var_2nd], 
      mzlb[, 'cens_class_ind'], useNA = 'ifany')
# Twelve observations dropped.
# Still enough to identify second probability. 


# Specify model equation.
fmla_string <- sprintf('%s ~ %s', target_var_2nd,
                       paste(pred_var_list, collapse = " + "))
# fmla_string <- sprintf('%s ~ %s', target_var_2nd,
#                        paste(trim_var_list, collapse = " + "))

fmla <- as.formula(fmla_string)

#--------------------------------------------------------------------------------
# Auxilliary parameters from data: 
# Estimate using observed rate changes only. 
#--------------------------------------------------------------------------------

lm_aux_params_data <- lm(formula = fmla, 
                         data = mzlb[!mzlb[, 'cens_class_ind'], ])

summary(lm_aux_params_data)

# Store prediction for the censored period. 
mzlb[, sprintf('%s_pred', target_var_2nd)] <- 
  predict(lm_aux_params_data, newdata = mzlb)

# Aggregate by leaf node, labelled by jump predictions. 
avg_pred_2nd <- aggregate(mzlb[!is.na(mzlb[, sprintf('%s_pred', target_var_2nd)]), 
                               sprintf('%s_pred', target_var_2nd)], 
                          by = list(mzlb[!is.na(mzlb[, sprintf('%s_pred', target_var_2nd)]), 
                                         'cens_class_ind']), 
                          FUN = 'mean')
colnames(avg_pred_2nd) <- c('cens_class_ind', sprintf('%s_pred', target_var_2nd))

# Store average predicted second moment for the auxiliary parameter. 
avg_pred_2nd_ap <- avg_pred_2nd[avg_pred_2nd[, 'cens_class_ind'], 
                                sprintf('%s_pred', target_var_2nd)]


# Store RSS for a weight in the objective function.  
lm_aux_params_wt <- summary(lm_aux_params_data)$sigma

# Now need a weight for the distance for the predicted target rates.
ffr_wt <- sd(mzlb[!mzlb[, 'zlb_ind'], 'fed_funds'], na.rm = TRUE)





#--------------------------------------------------------------------------------
# Create a second function for optimization based on 
# additional (auxilliary) parameters. 
#--------------------------------------------------------------------------------

cens_zlb_dist <- function(est_cens_probs) {
  
  # For troubleshooting optimization 
  # (NaNs possible if probability bounds are violated). 
  # print(sprintf('x = (%f, %f)', est_cens_probs[1], est_cens_probs[2]))
  
  distance <- 0
  
  # Calculate expected rate cuts over censored ZLB class period. 
  mzlb[, 'est_cens_pred_jump'] <- mzlb[, 'pred_jump_ExclZLB']
  mzlb[mzlb[, 'cens_class_ind'], 'est_cens_pred_jump'] <- NA
  mzlb[mzlb[, 'cens_class_ind'], 'est_cens_pred_jump'] <- sum(est_cens_probs*c(-0.5,-0.25))
  
  # Calculate expected path of target rate. 
  mzlb[, 'est_cens_target_rate'] <- cumsum(mzlb[, 'est_cens_pred_jump']) + 
    mzlb[1, 'eff_ffr'] - 1.5
  # Correction to align at contact point with ZLB. 
  mzlb[cumsum(mzlb[, 'zlb_ind']) >= 1, 'est_cens_target_rate'] <- 
    mzlb[cumsum(mzlb[, 'zlb_ind']) >= 1, 'est_cens_target_rate'] - 
    mzlb[cumsum(mzlb[, 'zlb_ind']) == 1, 'est_cens_target_rate']
  
  # Calculate terms in distance function. 
  
  # Calculate difference from target rate after ZLB liftoff. 
  post_ZLB_dist <- sum((mzlb[mzlb[, 'post_zlb_ind'], 'est_cens_target_rate'] - 
                          mzlb[mzlb[, 'post_zlb_ind'], 'fed_funds'])^2)
  
  # Calculate one-sided difference from predicted target rate above zero. 
  in_ZLB_dist <- sum(mzlb[mzlb[, 'zlb_ind'] & 
                            mzlb[, 'est_cens_target_rate'] > 0, 'est_cens_target_rate']^2)
  
  # Calculate second moment to compare with auxilliary parameter. 
  est_pred_avg <- sum(c(-0.5, -0.25, 0) * 
                        c(est_cens_probs, 1-sum(est_cens_probs)))
  est_pred_2nd_ap <- sqrt(- est_pred_avg^2 + 
                            sum(c(-0.5, -0.25, 0)^2 * 
                                  c(est_cens_probs, 1-sum(est_cens_probs))))
  est_pred_2nd_ap_dist <- (est_pred_2nd_ap - avg_pred_2nd_ap)^2
  
  # Calculate the weighted distance.
  distance <- (in_ZLB_dist + post_ZLB_dist)/ffr_wt^2 + 
    est_pred_2nd_ap_dist/lm_aux_params_wt^2
  
  return(distance)
}



# Parameters are probabilities of -0.5 and -0.25 rate cuts, respectively. 
est_cens_probs <- c(0.10, 0.25) # To test function. 

# Test with function calls.
cens_zlb_dist(est_cens_probs)

#--------------------------------------------------------------------------------
# Preliminary grid search for starting values
#--------------------------------------------------------------------------------


# Initialize grid for calculation of several tests. 
prob_2_seq <- seq(0.04, 0.2, by = 0.001)
prob_1_seq <- seq(0.005, 0.325, by = 0.001)
dist_test <- expand.grid(prob_2 = prob_2_seq, prob_1 = prob_1_seq)
# Remove impossible combinations (negative probabilities). 
dist_test <- dist_test[dist_test[, 'prob_1'] + dist_test[, 'prob_2'] <= 1, ]
dist_test[, 'exp_jump'] <- NA
dist_test[, 'dist'] <- NA

for (row_num in 1:nrow(dist_test)) {
  
  test_cens_probs <- c(dist_test[row_num, 'prob_2'], dist_test[row_num, 'prob_1'])
  
  dist_test[row_num, 'exp_jump'] <- sum(test_cens_probs*c(-0.5, -0.25))
  
  dist_test[row_num, 'dist'] <- cens_zlb_dist(test_cens_probs)
  
}

# Inspect leading parameter values.
dist_test[order(dist_test$dist), ][1:50, ]

# Create a finer simplex to optimize over. 
min(dist_test[order(dist_test$dist), ][1:50, 'prob_2'])
max(dist_test[order(dist_test$dist), ][1:50, 'prob_2'])
min(dist_test[order(dist_test$dist), ][1:50, 'prob_1'])
max(dist_test[order(dist_test$dist), ][1:50, 'prob_1'])

# Use the best estimates so far as starting values for optimization. 
est_cens_probs_start <- dist_test[order(dist_test$dist), ][1, c('prob_2', 'prob_1')]


################################################################################
# Perform estimation by indirect inference. 
################################################################################


#--------------------------------------------------------------------------------
# Optimize over rate decrease probabilities.  
#--------------------------------------------------------------------------------


# optim(par, fn, gr = NULL,
#       method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
#                  "Brent"),
#       lower = -Inf, upper = Inf,
#       control = list(), hessian = FALSE)

est_cens_probs_model <- optim(par = est_cens_probs_start, 
                              fn = cens_zlb_dist, 
                              gr = NULL,
                              method = "L-BFGS-B",
                              lower = c(0, 0), 
                              upper = c(0.5, 0.5),
                              # control = list(), 
                              hessian = TRUE)

est_cens_probs_model


#--------------------------------------------------------------------------------
# Calculate statistics.  
#--------------------------------------------------------------------------------


# Parameter estimates in percentage points. 
prob_hat <- est_cens_probs_model$par*100

# Use Hessian Matrix to calculate standard errors. 
hess <- est_cens_probs_model$hessian

var_mat <- solve(hess)

se_probs <- sqrt(diag(var_mat))*100

t_stats <- prob_hat/se_probs

conf_int_u <- pmin(prob_hat + 1.96*se_probs, 100)
conf_int_l <- pmax(prob_hat - 1.96*se_probs, 0)


################################################################################
# Postestimation and Comparison. 
################################################################################

#--------------------------------------------------------------------------------
# Calculate predictions.  
#--------------------------------------------------------------------------------

# Calculate expected rate cuts over censored ZLB class period. 
mzlb[, 'est_cens_pred_jump'] <- mzlb[, 'pred_jump_ExclZLB']
mzlb[mzlb[, 'cens_class_ind'], 'est_cens_pred_jump'] <- NA
mzlb[mzlb[, 'cens_class_ind'], 'est_cens_pred_jump'] <- sum(est_cens_probs_model$par*c(-0.5,-0.25))

# Calculate expected path of target rate. 
mzlb[, 'est_cens_target_rate'] <- cumsum(mzlb[, 'est_cens_pred_jump']) + 
  mzlb[1, 'eff_ffr'] - 1.5
# Correction to align at contact point with ZLB. 
mzlb[cumsum(mzlb[, 'zlb_ind']) >= 1, 'est_cens_target_rate'] <- 
  mzlb[cumsum(mzlb[, 'zlb_ind']) >= 1, 'est_cens_target_rate'] - 
  mzlb[cumsum(mzlb[, 'zlb_ind']) == 1, 'est_cens_target_rate']



#--------------------------------------------------------------------------------
# Plot predictions.  
#--------------------------------------------------------------------------------

# Plot the components on screen for inspection. 


# Compare with actual federal funds target rate and SOMA holdings.
plot(mzlb[, 'est_cens_target_rate'], type = 'l')
lines(mzlb[, 'fed_funds'], col = 'blue')
lines(mzlb[, 'shadow_rate'], col = 'green')
lines(10*(mzlb[, 'soma_hold'] - range_soma[1]) /
        (range_soma[2] - range_soma[1]), col = 'red')




#--------------------------------------------------------------------------------
# Output figures of predicted (shadow) target rates.
#--------------------------------------------------------------------------------

# Plot the components on screen for inspection. 


# Add dates on axes.
new_year_dates <- (1:nrow(mzlb))[substr(mzlb[, 'date'], 6,7) == '01']
mzlb[new_year_dates, 'date']
new_year_labels <- substr(mzlb[new_year_dates, 'date'], 1,4)


# Compare with actual federal funds target rate. 
plot(mzlb[, 'fed_funds'], type = 'l', 
     main = 'Comparison with Shadow Rates and SOMA Holdings', 
     ylab = c('Interest Rates % (left axis)', 
              'SOMA Holdings (billions, right axis)'),
     xaxt='n', 
     ylim = c(-4, 10), 
     col = 'blue', 
     lwd = 3)
# lines(mzlb[, 'fed_funds'], col = 'blue', lwd = 3)
lines(mzlb[, 'est_cens_target_rate'], col = 'black', lwd = 3, lty = 'dashed')
lines(mzlb[, 'shadow_rate'], col = 'green', lwd = 3, lty = 'dotted')
lines(mzlb[, 'soma_hold']/500, col = 'red', lwd = 3, lty = 'twodash')
lines(rep(0, nrow(mzlb)), lwd = 1, col = 'black')

axis(1, at = new_year_dates[seq(4, 32, by = 5)], 
     labels = new_year_labels[seq(4, 32, by = 5)])
axis(4, at = seq(0, 10, by = 2), labels = seq(0, 10, by = 2)*500)



# Determine date of sharp upturn in shadow rate.
min(mzlb[, 'est_cens_target_rate'])

mzlb[mzlb[, 'est_cens_target_rate'] ==
       min(mzlb[, 'est_cens_target_rate']), 
     c('date', 'est_cens_target_rate')]
# Output:
#       date     est_cens_target_rate
# 322 2013-03             -4.132281



#--------------------------------------------------------------------------------
# Ready to produce the publishable version. 
#--------------------------------------------------------------------------------

# Plot Figure 6 and output to the figs folder. 
# Create two copies of each to ease 
# building the pdf documents on different platforms. 
fig_type_list <- c('pdf', 'eps')
fig_num <- 6

for (fig_type in fig_type_list) {
  
  # Extension depends on figure file format. 
  fig_file_name <- sprintf('%s/Fig%d.%s', fig_path, fig_num, fig_type)
  
  
  # Open the selected file. 
  if (fig_type == 'pdf') {
    # Open pdf file to save figure.
    pdf(fig_file_name)
  } else if (fig_type == 'eps') {
    # Alternatively, open eps file to save figure.
    postscript(fig_file_name)
    # Required for submission of manuscript. 
  }
  
  
  # Compare with actual federal funds target rate. 
  plot(mzlb[, 'fed_funds'], type = 'l', 
       # main = 'Comparison with Shadow Rates and SOMA Holdings', 
       ylab = c('Interest Rates % (left axis)', 
                'SOMA Holdings (billions, right axis)'),
       xaxt='n', 
       ylim = c(-4, 10), 
       col = 'blue', 
       lwd = 3)
  # lines(mzlb[, 'fed_funds'], col = 'blue', lwd = 3)
  lines(mzlb[, 'est_cens_target_rate'], col = 'black', lwd = 3, lty = 'dashed')
  lines(mzlb[, 'shadow_rate'], col = 'green', lwd = 3, lty = 'dotted')
  lines(mzlb[, 'soma_hold']/500, col = 'red', lwd = 3, lty = 'twodash')
  lines(rep(0, nrow(mzlb)), lwd = 1, col = 'black')
  
  axis(1, at = new_year_dates[seq(4, 32, by = 5)], 
       labels = new_year_labels[seq(4, 32, by = 5)])
  axis(4, at = seq(0, 10, by = 2), labels = seq(0, 10, by = 2)*500)
  
  
  # Close pdf file to save figure.
  dev.off()
  
}



################################################################################
# End. 
################################################################################

