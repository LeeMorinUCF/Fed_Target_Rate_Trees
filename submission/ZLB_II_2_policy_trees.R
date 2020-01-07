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
# This second script is used to estimate a classification tree 
# for Federal Reserve policy as a function of the variables
# prepared in ZLB_II_1_data_prep.R. 
# 
# Dependencies: 
#   Packages rpart and rpart.plot to estimate classification trees.
# 
################################################################################



################################################################################
# Estimation of Classification Models 
################################################################################

#--------------------------------------------------------------------------------
# Repeat Estimation for Three Cases:
# 1. Ignoring ZLB (as if ZLB dates are truly all zero changes).
#   Include all observations, without ZLB indicator. 
# 2. Excluding ZLB (dropping observations from dataset).
#   Exclude ZLB observations (and ZLB indicator, now constant). 
# 3. Acknowledging ZLB (adding additional category for ZLB dates).
#   Include all observations and include ZLB indicator. 
#--------------------------------------------------------------------------------

# Chose a Target Variable. 
table(mzlb[, 'fed_jump'], mzlb[, 'fed_jump_zlb'], useNA = 'ifany')


# Loop through target and observations 
# depending on whether ZLB is ignored or excluded.

sel_case_list <- c('ExclZLB', # Excluding ZLB,
                   # used with indirect inference in second stage.
                   'IgnZLB',  # Ignoring ZLB, treating ZLB as observed zeros,
                   # Estimated for comparison. 
                   'AcknZLB'  # Acknowledging ZLB, treating ZLB as separate category, 
                   # Likewise, estimated for comparison.
                   )
# Set corresponding figure numbers in manuscript. 
fig_num_list <- seq(2, 4)

for (sel_case in sel_case_list) {
  
  
  #--------------------------------------------------------------------------------
  # Set target, sample and explanatory variables. 
  #--------------------------------------------------------------------------------
  
  if (sel_case == 'ExclZLB') {
    
    target_var <- 'fed_jump'
    sel_obsns <- incl_obsns & mzlb[, 'zlb_ind'] == FALSE
    estn_var_list <- pred_var_list
    
  } else if (sel_case == 'IgnZLB') {
    
    target_var <- 'fed_jump'
    sel_obsns <- incl_obsns
    estn_var_list <- pred_var_list
    
  } else if (sel_case == 'AcknZLB') {
    
    target_var <- 'fed_jump_zlb'
    sel_obsns <- incl_obsns
    # Add a ZLB indicator to identify censored observations at ZLB.
    # estn_var_list <- c(pred_var_list, 'zlb_ind')
    estn_var_list <- c(pred_var_list)
    
  }
  
  
  #--------------------------------------------------------------------------------
  # Estimation of Chosen Case of Classification Tree Model (specified by sel_case)
  #--------------------------------------------------------------------------------
  
  # Specify model equation.
  fmla_string <- sprintf('%s ~ %s', target_var, 
                         paste(estn_var_list, collapse = " + "))
  
  fmla <- as.formula(fmla_string)
  
  
  # Grow initial tree.
  rand_seed <- 914 # Month and date of the first version of the paper (time flies!)
  set.seed(rand_seed)
  # Default function call:
  # fed_tree <- rpart(fmla,
  #              method = "class", data = mzlb[sel_obsns, ])
  fed_tree <- rpart(fmla,
                    method = "class", 
                    data = mzlb[sel_obsns, ], 
                    control = rpart.control(minsplit = 12, # 1 year
                                            minbucket = 3, # 1 quarter
                                            cp = 0.01,
                                            maxcompete = 4, 
                                            maxsurrogate = 5, 
                                            usesurrogate = 2, 
                                            xval = 10,
                                            surrogatestyle = 0, 
                                            maxdepth = 30))
  
  # Display the summary statistics for splits.
  printcp(fed_tree)  
  
  
  # Display variable importance statistics. 
  fed_tree$variable.importance
  
  
  
  #--------------------------------------------------------------------------------
  # Postestimation: Pruning
  #--------------------------------------------------------------------------------
  
  # Prune tree by minimum cross-validation error.
  which.min(fed_tree$cptable[,"xerror"])
  fed_tree$cptable[which.min(fed_tree$cptable[,"xerror"]),"CP"]
  
  # Selected complexity parameter by case (there are ties). 
  if (sel_case == 'ExclZLB') {
    sel_cp_min <- fed_tree$cptable[2,"CP"]
  } else if (sel_case == 'AcknZLB') {
    # sel_cp_min <- fed_tree$cptable[3,"CP"] # With ZLB flag (same as ExclZLB)
    sel_cp_min <- fed_tree$cptable[4,"CP"] # Without (two options)
    sel_cp_min <- fed_tree$cptable[5,"CP"]
  }  else if (sel_case == 'IgnZLB') {
    # sel_cp_min <- fed_tree$cptable[3,"CP"]
    sel_cp_min <- 0.023
  } 
  
  # Prune tree according to specified complexity parameter. 
  pr_fed_tree <- prune(fed_tree, cp = sel_cp_min)
  
  # Output results of pruned tree. 
  printcp(pr_fed_tree)  
  
  # Display variable importance statistics. 
  pr_fed_tree$variable.importance
  
  
  
  #--------------------------------------------------------------------------------
  # Postestimation: Plot trees
  #--------------------------------------------------------------------------------
  
  # Create a separate plot for each case of model. 
  fig_num <- fig_num_list[sel_case_list == sel_case]
  
  
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
    
    
    # Plot the classification tree.
    rpart.plot(pr_fed_tree, type = 2, 
               cex = 0.65, tweak = 0.95, 
               yesno = 2, leaf.round = 0, 
               extra = 1, 
               legend.x = 20)
    # Ignore the warning messages. Just check the figure. 
    
    
    # Close pdf file to save figure.
    dev.off()
    
  }
  
  
  #--------------------------------------------------------------------------------
  # Postestimation: Save for indirect inference
  #--------------------------------------------------------------------------------
  
  
  # For ExclZLB, save this tree for indirect inference.
  if (sel_case == 'ExclZLB') {
    ExclZLB_fed_tree <- pr_fed_tree
  } 
  
  
  #--------------------------------------------------------------------------------
  # Postestimation: Calculate Predictions
  #--------------------------------------------------------------------------------
  
  # Obtain predictions of class probabilities. 
  prob_class <- sprintf('prob_%s', levels(mzlb[, target_var]))
  mzlb[, prob_class] <- NA
  mzlb[, prob_class] <- predict(fed_tree, newdata = mzlb)
  
  # Initialize variables for expected jumps, predicted classes and their probabilities. 
  mzlb[, 'pred_jump'] <- 0
  mzlb[, 'pred_class'] <- factor(levels = levels(mzlb[, target_var]))
  
  
  # Current maximum probability of class, 
  # to be replaced with true max probability of class. 
  mzlb[, 'pred_max_prob'] <- 0
  
  # Translate class number into jump size. 
  if (target_var == 'fed_jump_zlb') {
    jump_values <- c(0, 0.25*seq(-2,2) + 0)
  } else {
    jump_values <- c(0.25*seq(-2,2) + 0)
  }
  
  for (class_num in 1:length(prob_class)) {
    
    # Calculate the expected jump size. 
    mzlb[, 'pred_jump'] <- mzlb[, 'pred_jump'] + 
      jump_values[class_num]*mzlb[, prob_class[class_num]]
    
    # Determine which rows have a new high probability for this class.
    pred_class_tag <- levels(mzlb[, target_var])[class_num]
    new_high_prob_class <- mzlb[, prob_class[class_num]] >  mzlb[, 'pred_max_prob']
    
    # Label the predicted class, by maximum probability. 
    mzlb[new_high_prob_class, 'pred_class'] <- pred_class_tag
    
    # Update maximum probability.
    mzlb[new_high_prob_class, 'pred_max_prob'] <- 
      mzlb[new_high_prob_class, prob_class[class_num]]
    
  }
  
  # Verify calculations. 
  summary(mzlb[, prob_class]) # Probabilities
  summary(mzlb[, 'pred_jump']) # Within [-0.5, 0.5]
  
  # Verify correct maximum class probability. 
  head(mzlb[, c(prob_class, 'pred_max_prob')])
  tail(mzlb[, c(prob_class, 'pred_max_prob')])
  
  # Compare most probable class with expected jump size.
  plot(mzlb[, 'pred_jump'], 
       mzlb[, 'pred_class'])
  # Almost collinear, aside from ZLB. 
  
  
  # Plot time series of expected jump size. 
  plot(mzlb[, 'pred_jump'], type = 'l')
  # Variability as expected. 
  
  # Store expected jumps and class predictions by model. 
  mzlb[, sprintf('pred_jump_%s', sel_case)] <- mzlb[, 'pred_jump']
  mzlb[, sprintf('pred_class_%s', sel_case)] <- mzlb[, 'pred_class']
  
  
}


# 
# #--------------------------------------------------------------------------------
# # Calculate Measures of Accuracy
# #--------------------------------------------------------------------------------
# 
# 
# # First, visualize the predictions and actuals.
# mzlb[, 'fed_jump_num'] <- 0.25*(as.integer(mzlb[, 'fed_jump']) - 3)
# plot(mzlb[, 'pred_jump'], 
#      mzlb[, 'fed_jump_num'])
# # A positive slope is good.
# 
# # Fit regression model. 
# rpart_lm <- lm(formula = fed_jump_num ~ pred_jump, data = mzlb)
# summary(rpart_lm)
# # Store the r-squared for comparison. 
# fit_rsq <- summary(rpart_lm)$r.squared
# 
# 
# # A confusion matrix gives more numbers to compare.
# conf_mat <- table(mzlb[sel_obsns, target_var], 
#                   mzlb[sel_obsns, 'pred_class'], useNA = 'ifany')
# 
# # Correction for incorrect class names in AcknZLB case 
# # rownames(conf_mat) <- c('-9', '-2', '-1', '0', '1', '2')
# 
# pct_correct <- sum(mzlb[sel_obsns, target_var] == 
#                      mzlb[sel_obsns, 'pred_class'], na.rm = TRUE) / 
#   sum(!is.na(mzlb[sel_obsns, target_var]))
# 
# pct_correct_non_zlb <- sum(mzlb[!mzlb[, 'zlb_ind'], target_var] == 
#                              mzlb[!mzlb[, 'zlb_ind'], 'pred_class'], na.rm = TRUE) / 
#   sum(!is.na(mzlb[!mzlb[, 'zlb_ind'], target_var]))
# 
# 
# 
# # Compare with the null model: Always zero (most frequent class).
# pct_zero <- sum(mzlb[sel_obsns, target_var] == 'UN', na.rm = TRUE) / 
#   sum(!is.na(mzlb[sel_obsns, target_var]))
# 
# 
# # Store this confusion matrix for this model case.
# conf_mat_name <- sprintf('conf_mat_%s', sel_case)
# # Reorder columns.
# conf_mat_cols <- levels(mzlb[, 'pred_class'])[levels(mzlb[, 'pred_class']) %in% 
#                                                 colnames(conf_mat)]
# assign(conf_mat_name, conf_mat[, conf_mat_cols])
# 
# 
# # Fix pct_correct for AcknZLB case.
# # pct_correct <- sum(diag(conf_mat_AcknZLB))/sum(sum(conf_mat_AcknZLB))
# 
# # Store correct prediction as well. 
# pct_correct_name <- sprintf('pct_correct_%s', sel_case)
# assign(pct_correct_name, pct_correct)
# pct_correct_name <- sprintf('pct_correct_non_zlb_%s', sel_case)
# assign(pct_correct_name, pct_correct_non_zlb)
# 
# 
# # Store the r-squared of the prediction.
# fit_rsq_name <- sprintf('fit_rsq_%s', sel_case)
# assign(fit_rsq_name, fit_rsq)
# 
# 
# 
# # Test:
# # conf_mat_ExclZLB
# # pct_correct_ExclZLB
# # pct_correct_non_zlb_ExclZLB
# # fit_rsq_ExclZLB
# # conf_mat_AcknZLB
# # conf_mat_AcknZLB_orig
# # pct_correct_AcknZLB
# # pct_correct_non_zlb_AcknZLB
# # fit_rsq_AcknZLB
# # conf_mat_IgnZLB
# # pct_correct_IgnZLB
# # pct_correct_non_zlb_IgnZLB
# # fit_rsq_IgnZLB
# # pct_zero
# 


################################################################################
# Plot Figures and Save for Manuscript
################################################################################


#--------------------------------------------------------------------------------
# Plot the predictions from the three competing models. 
# Must plot Results after all 3 model cases are estimated
#--------------------------------------------------------------------------------


# Plot the components on screen for inspection. 

plot(mzlb[, 'fed_funds'], 
     main = 'Effective Federal Funds Rate and Predictions', 
     xlab = 'Date', 
     ylab = 'Interest Rates', 
     cex.main = 1.5, 
     cex.lab = 1.5, 
     type = 'l', ylim = c(0,12), col = 'blue', lwd = 4, 
     xaxt='n')
axis(1, at = five_year_dates, 
     labels = five_year_labels)

# Plot the model predictions for each case. 
adj_eff_ffr <- 2.0 # Initial condition.
fig_sel_case <- 'ExclZLB'
lines(cumsum(mzlb[, sprintf('pred_jump_%s', fig_sel_case)]) + 
        mzlb[1, 'eff_ffr'] - adj_eff_ffr, 
      col = 'black', lwd = 3, lty = 'dashed')

fig_sel_case <- 'IgnZLB'
lines(cumsum(mzlb[, sprintf('pred_jump_%s', fig_sel_case)]) + 
        mzlb[1, 'eff_ffr'] - adj_eff_ffr, 
      col = 'black', lwd = 3, lty = 'twodash')

fig_sel_case <- 'AcknZLB'
lines(cumsum(mzlb[, sprintf('pred_jump_%s', fig_sel_case)]) + 
        mzlb[1, 'eff_ffr'] - adj_eff_ffr, 
      col = 'black', lwd = 3, lty = 'dotted')


# As expected, the ExclZLB model does not predict well on the ZLB period. 
# Likewise, the AcknZLB model predicts zeros on the ZLB period, 
# which is a placeholder waiting for the predictions from indirect inference. 


#--------------------------------------------------------------------------------
# Ready to produce the publishable version. 
#--------------------------------------------------------------------------------


# Plot Figure 5 and output to the figs folder. 
# Create two copies of each to ease 
# building the pdf documents on different platforms. 
fig_type_list <- c('pdf', 'eps')
fig_num <- 5

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
  
  
  plot(mzlb[, 'fed_funds'], 
       # main = 'Effective Federal Funds Rate and Predictions', 
       xlab = 'Date', 
       ylab = 'Interest Rates', 
       cex.main = 1.5, 
       cex.lab = 1.5, 
       type = 'l', ylim = c(0,12), col = 'blue', lwd = 4, 
       xaxt='n')
  axis(1, at = five_year_dates, 
       labels = five_year_labels)
  
  # Plot the model predictions for each case. 
  adj_eff_ffr <- 2.0 # Initial condition.
  fig_sel_case <- 'ExclZLB'
  lines(cumsum(mzlb[, sprintf('pred_jump_%s', fig_sel_case)]) + 
          mzlb[1, 'eff_ffr'] - adj_eff_ffr, 
        col = 'black', lwd = 3, lty = 'dashed')
  
  fig_sel_case <- 'IgnZLB'
  lines(cumsum(mzlb[, sprintf('pred_jump_%s', fig_sel_case)]) + 
          mzlb[1, 'eff_ffr'] - adj_eff_ffr, 
        col = 'black', lwd = 3, lty = 'twodash')
  
  fig_sel_case <- 'AcknZLB'
  lines(cumsum(mzlb[, sprintf('pred_jump_%s', fig_sel_case)]) + 
          mzlb[1, 'eff_ffr'] - adj_eff_ffr, 
        col = 'black', lwd = 3, lty = 'dotted')
  
  
  # Close pdf file to save figure.
  dev.off()
}

#--------------------------------------------------------------------------------


################################################################################
# End
################################################################################
