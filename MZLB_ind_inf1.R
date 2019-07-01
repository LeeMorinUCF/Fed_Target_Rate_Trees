################################################################################
# 
# Decision Trees for the Federal Reserve Target Rate Policy
# 
# Lee Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
# 
# June 27, 2019
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
#   Assumes MZLB_refit.R has been run on ExclZLB model. 
# 
################################################################################


################################################################################
# Load Data
################################################################################

# # Added to original data series.
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



# Store expected jumps and class predictions by model. 
summary(mzlb[, sprintf('pred_jump_%s', sel_case)])
table(mzlb[, sprintf('pred_class_%s', sel_case)], useNA = 'ifany')

# Plot figures to inspect variablity over ZLB period. 
plot(mzlb[, sprintf('pred_jump_%s', sel_case)], 
     type = 'l')
lines(mzlb[, 'zlb_ind']*0.25, col = 'blue')
lines(0.2*(mzlb[, 'soma_hold'] - range_soma[1]) / 
        (range_soma[2] - range_soma[1]), col = 'red')
lines(mzlb[, 'shadow_rate']*0.1, col = 'green')

# Compare variability if predicted class over ZLB period.
table(mzlb[, sprintf('pred_class_%s', sel_case)], 
      mzlb[, 'zlb_ind'], useNA = 'ifany')
# Note there are no negative predictions in ZLB 
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
#   => penalty from difference from target rate. 
# Should remain negative while ZLB is in effect. 
#   => penalty only from predicted target rate above zero. 

# First need to calculate expected jump sizes and expected target rates. 


# Calculate inputs to distance function. 

# Parameters are probabilities of -0.5 and -0.25 rate cuts, respectively. 
est_cens_probs <- c(0.10, 0.25) 

# Calculate expected rate cuts over censored ZLB class period. 
mzlb[, 'est_cens_pred_jump'] <- mzlb[, sprintf('pred_jump_%s', sel_case)]
mzlb[mzlb[, 'cens_class_ind'], 'est_cens_pred_jump'] <- NA
mzlb[mzlb[, 'cens_class_ind'], 'est_cens_pred_jump'] <- sum(est_cens_probs*c(-0.5,-0.25))

# Sense check. 
summary(mzlb[, 'est_cens_pred_jump'])
table(mzlb[, 'est_cens_pred_jump'], 
      mzlb[, 'cens_class_ind'], useNA = 'ifany')

# Calculate expected path of target rate. 
mzlb[, 'est_cens_target_rate'] <- cumsum(mzlb[, 'est_cens_pred_jump']) + 
  mzlb[1, 'eff_ffr'] - 1.5
# Correction to align at contact point with ZLB. 
mzlb[cumsum(mzlb[, 'zlb_ind']) >= 1, 'est_cens_target_rate'] <- 
  mzlb[cumsum(mzlb[, 'zlb_ind']) >= 1, 'est_cens_target_rate'] - 
  mzlb[cumsum(mzlb[, 'zlb_ind']) == 1, 'est_cens_target_rate']

# Compare with actual federal funds target rate. 
plot(mzlb[, 'est_cens_target_rate'], type = 'l')
lines(mzlb[, 'fed_funds'], col = 'blue')
lines(mzlb[, 'shadow_rate'], col = 'green')
lines(10*(mzlb[, 'soma_hold'] - range_soma[1]) / 
        (range_soma[2] - range_soma[1]), col = 'red')
# That was for a pre-specified value of the censored probabilities. 



#--------------------------------------------------------------------------------
# Calculate distance function. 
#--------------------------------------------------------------------------------

# Calculate difference from target rate after ZLB liftoff. 

summary(mzlb[cumsum(mzlb[, 'zlb_ind']) >= 1 & 
               !mzlb[, 'zlb_ind'], 'est_cens_target_rate'])

# Create index for post-ZLB period. 
mzlb[, 'post_zlb_ind'] <- cumsum(mzlb[, 'zlb_ind']) >= 1 & 
  !mzlb[, 'zlb_ind']
mzlb[nrow(mzlb), 'post_zlb_ind'] <- FALSE

post_ZLB_dist <- sum((mzlb[mzlb[, 'post_zlb_ind'], 'est_cens_target_rate'] - 
                        mzlb[mzlb[, 'post_zlb_ind'], 'fed_funds'])^2)



# Calculate one-sided difference from predicted target rate above zero. 
in_ZLB_dist <- sum(mzlb[mzlb[, 'zlb_ind'] & 
                          mzlb[, 'est_cens_target_rate'] > 0, 'est_cens_target_rate']^2)

distance <- in_ZLB_dist + post_ZLB_dist



#--------------------------------------------------------------------------------
# Create a function for optimization based on interest rate statistics only. 
# This is enough to identify the first moment of the change in interest rates. 
#--------------------------------------------------------------------------------

cens_zlb_dist_v1 <- function(est_cens_probs) {
  
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
  
  distance <- in_ZLB_dist + post_ZLB_dist
  
  return(distance)
}

# Test with function calls.
cens_zlb_dist_v1(est_cens_probs)

# Parameters are probabilities of -0.5 and -0.25 rate cuts, respectively. 
est_cens_probs <- c(0.10, 0.25) 

# Initialize grid for calculation of several tests. 
prob_2_seq <- seq(0, 0.2, by = 0.005)
prob_1_seq <- seq(0, 0.4, by = 0.005)
dist_test <- expand.grid(prob_2 = prob_2_seq, prob_1 = prob_1_seq)
# Remove impossible combinations (negative probabilities). 
dist_test <- dist_test[dist_test[, 'prob_1'] + dist_test[, 'prob_2'] <= 1, ]
dist_test[, 'exp_jump'] <- NA
dist_test[, 'dist'] <- NA

for (row_num in 1:nrow(dist_test)) {
  
  test_cens_probs <- c(dist_test[row_num, 'prob_2'], dist_test[row_num, 'prob_1'])
  
  dist_test[row_num, 'exp_jump'] <- sum(test_cens_probs*c(-0.5, -0.25))
  
  dist_test[row_num, 'dist'] <- cens_zlb_dist_v1(test_cens_probs)
  
}

dist_test[order(dist_test$dist), ][1:50, ]

# Confirms tha this is enough to identify the first moment 
# of the change in interest rates. 
# Need more auxiliary parameters. 

#--------------------------------------------------------------------------------
# Create a second function for optimization based on 
# additional (auxilliary) parameters. 
# This is required to identify the second moment of the change in interest rates. 
#--------------------------------------------------------------------------------

# Run a regression of the second moment of the expected rate change 
# on the predictor variables in the model. 

# Calculate a second moment target variable. 
target_var_2nd <- 'fed_jump_st_dev' 

mzlb[, target_var_2nd] <- - mzlb[, 'pred_jump_ExclZLB']^2
prob_class <- sprintf('prob_%s', levels(mzlb[, target_var]))
jump_values <- 0.25*(as.integer(levels(mzlb[, 'fed_jump'])) + 0)

for (class_num in 1:length(prob_class)) {
  
  # Calculate the expected jump size. 
  mzlb[, target_var_2nd] <- mzlb[, target_var_2nd] + 
    jump_values[class_num]^2*mzlb[, prob_class[class_num]]
  
}

summary(mzlb[, target_var_2nd])
summary(sqrt(mzlb[, target_var_2nd]))

# Restate to standard deviations. 
mzlb[, target_var_2nd] <- sqrt(mzlb[, target_var_2nd])

table(mzlb[, target_var_2nd], 
      mzlb[, 'zlb_ind'], useNA = 'ifany')
table(mzlb[, target_var_2nd], 
      mzlb[, 'cens_class_ind'], useNA = 'ifany')



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

# Test with function calls.
cens_zlb_dist(est_cens_probs)

# Parameters are probabilities of -0.5 and -0.25 rate cuts, respectively. 
est_cens_probs <- c(0.10, 0.25) 

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

dist_test[order(dist_test$dist), ][1:50, ]

# Create a finer simplex to manually optimize over. 
min(dist_test[order(dist_test$dist), ][1:50, 'prob_2'])
max(dist_test[order(dist_test$dist), ][1:50, 'prob_2'])
min(dist_test[order(dist_test$dist), ][1:50, 'prob_1'])
max(dist_test[order(dist_test$dist), ][1:50, 'prob_1'])

# Use the best estimates so far as starting values for optimization. 
est_cens_probs_start <- dist_test[order(dist_test$dist), ][1, c('prob_2', 'prob_1')]


################################################################################
# Perform estimation by indirect inference. 
################################################################################

# Optimize on rate decrease probabilities.  


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

# Parameter estimates in percentage points. 
prob_hat <- est_cens_probs_model$par*100

# Use Hessian Matrix to calculate standard errors. 
hess <- est_cens_probs_model$hessian

var_mat <- solve(hess)

se_probs <- sqrt(diag(var_mat))*100

t_stats <- prob_hat/se_probs

conf_int_u <- pmin(prob_hat + 1.96*se_probs, 100)
conf_int_l <- pmax(prob_hat - 1.96*se_probs, 0)


# Plot predictions.
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


# Compare with actual federal funds target rate. 
plot(mzlb[, 'est_cens_target_rate'], type = 'l')
lines(mzlb[, 'fed_funds'], col = 'blue')
lines(mzlb[, 'shadow_rate'], col = 'green')
lines(10*(mzlb[, 'soma_hold'] - range_soma[1]) / 
        (range_soma[2] - range_soma[1]), col = 'red')


# Compare components of the optimal distance. 
est_pred_2nd_ap_hat <- sum(c(-0.5, -0.25, 0)^2 * 
                         c(est_cens_probs_model$par, 1-sum(est_cens_probs_model$par)))
avg_pred_2nd_ap




################################################################################
# Postestimation and Comparison. 
################################################################################


#--------------------------------------------------------------------------------
# Output figures of predicted (shadow) target rates.
#--------------------------------------------------------------------------------


# Add dates on axes.
new_year_dates <- (1:nrow(mzlb))[substr(mzlb[, 'date'], 6,7) == '01']
mzlb[new_year_dates, 'date']
new_year_labels <- substr(mzlb[new_year_dates, 'date'], 1,4)


#--------------------------------------------------------------------------------
# Compare with shadow rate model.
#--------------------------------------------------------------------------------

# Compare with actual federal funds target rate. 
plot(mzlb[, 'est_cens_target_rate'], type = 'l', 
     main = 'Estimates of the Federal Funds Rate', 
     ylab = 'Estimates of the Federal Funds Rate', 
     xaxt='n')
lines(mzlb[, 'fed_funds'], col = 'blue')
lines(mzlb[, 'shadow_rate'], col = 'green')
lines(rep(0, nrow(mzlb)), lwd = 2, col = 'black')

axis(1, at = new_year_dates[seq(4, 32, by = 5)], 
     labels = new_year_labels[seq(4, 32, by = 5)])


#--------------------------------------------------------------------------------
# Compare with SOMA asset holdings.
#--------------------------------------------------------------------------------

plot(mzlb[, 'soma_hold'], type = 'l')


mzlb[, 'd_soma_hold'] <- c(NA, diff(mzlb[, 'soma_hold']))

plot(mzlb[mzlb[, 'zlb_ind'], 'est_cens_pred_jump'], 
     mzlb[mzlb[, 'zlb_ind'], 'd_soma_hold'])


# Compare with actual federal funds target rate. 
plot(mzlb[!is.na(mzlb[, 'soma_hold']), 'est_cens_target_rate'], 
     type = 'l', ylim = c(-5,12), 
     # main = 'Predicted Federal Funds Rate and SOMA Asset Holdings', 
     main = '', 
     xlab = 'Date', 
     ylab = 'FFR and SOMA Asset Holdings')
# lines(mzlb[, 'fed_funds'], col = 'blue')
# lines(mzlb[, 'shadow_rate'], col = 'green')
lines(10*(mzlb[!is.na(mzlb[, 'soma_hold']), 'soma_hold'] - range_soma[1]) / 
        (range_soma[2] - range_soma[1]) + 1, col = 'red')
lines(rep(0, nrow(mzlb)), lwd = 2, col = 'black')

# axis(1, at = new_year_dates[seq(4, 32, by = 5)], 
#      labels = new_year_labels[seq(4, 32, by = 5)])



#--------------------------------------------------------------------------------
# Plot both together for a single figure.
#--------------------------------------------------------------------------------


# Compare with actual federal funds target rate. 
plot(mzlb[, 'est_cens_target_rate'], type = 'l', 
     main = 'Comparison with Shadow Rates and SOMA Holdings', 
     ylab = c('Interest Rates % (left axis)', 
              'SOMA Holdings (billions, right axis)'),
     xaxt='n', 
     ylim = c(-4, 10), 
     lwd = 2)
lines(mzlb[, 'fed_funds'], col = 'blue', lwd = 2)
lines(mzlb[, 'shadow_rate'], col = 'green', lwd = 2)
lines(mzlb[, 'soma_hold']/500, col = 'red', lwd = 2)
lines(rep(0, nrow(mzlb)), lwd = 1, col = 'black')

axis(1, at = new_year_dates[seq(4, 32, by = 5)], 
     labels = new_year_labels[seq(4, 32, by = 5)])
axis(4, at = seq(0, 10, by = 2), labels = seq(0, 10, by = 2)*500)



################################################################################
# End. 
################################################################################

# Correct the calculation of auxiliary parameters. 

test_cens_probs <- c(0.2, 0.0)
est_pred_2nd_ap <- sum(c(-0.5, -0.25, 0)^2 *
                         c(test_cens_probs, 1-sum(test_cens_probs)))

est_pred_2nd_ap <- - est_pred_avg^2 + 
  sum(c(-0.5, -0.25, 0)^2 * 
        c(test_cens_probs, 1-sum(test_cens_probs)))

est_pred_2nd_ap <- sqrt(- est_pred_avg^2 + 
                          sum(c(-0.5, -0.25, 0)^2 * 
                                c(test_cens_probs, 1-sum(test_cens_probs))))
