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

# Calculate distance function. 

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


table(mzlb[, 'est_cens_pred_jump'] == mzlb[, 'pred_jump'])


################################################################################
# Perform estimation by indirect inference. 
################################################################################






################################################################################
# Postestimation and Comparison. 
################################################################################


#--------------------------------------------------------------------------------
# Output figures of predicted (shadow) target rates.
#--------------------------------------------------------------------------------



#--------------------------------------------------------------------------------
# Compare with shadow rate model.
#--------------------------------------------------------------------------------


#--------------------------------------------------------------------------------
# Compare with SOMA asset holdings.
#--------------------------------------------------------------------------------



################################################################################
# End. 
################################################################################
