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
# This version compares the predictions with those from shadow rate models. 
# 
# Dependencies: 
#   None.
# 
################################################################################


################################################################################
# Setup Workspace and Load Libraries
################################################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory.
# wd_path <- '/home/ec2-user/MZLB-II_revisions' # On AWS
wd_path <- 'C:/Users/le279259/Documents/Research/MZLB-II/MZLB-II_revisions' # On Windows
# wd_path <- '~/MZLB-II_revisions' # On business.ucf.edu

setwd(wd_path)

# Load library to estimate classification tree with rpart.
library(rpart)


################################################################################
# Load Daily Federal Funds Rate series.
################################################################################

# Specify parameters for critical values.
file_tag <- 'ZLBFedYields'
cv_version <- 2
folder_name <- 'MZLB_data'

in_file_name <- sprintf('%s/%s%d.csv', 
                        folder_name, file_tag, cv_version)

# Load table of daily interest rates and ranges. 
fed_rates <- read.csv(file = in_file_name)


#--------------------------------------------------------------------------------
# Include indicator for zero lower bound
#--------------------------------------------------------------------------------

fed_rates[, 'zlb_ind'] <- fed_rates[, 'DFEDTARL'] == 0
fed_rates[is.na(fed_rates[, 'DFEDTARL']), 'zlb_ind'] <- FALSE

#--------------------------------------------------------------------------------
# Calculate target rate changes
#--------------------------------------------------------------------------------

# Generate changes in variables. 
fed_rates[, 'd_DFF'] <- c(NA, diff(fed_rates[, 'DFF']))
fed_rates[, 'd_DFEDTAR'] <- c(NA, diff(fed_rates[, 'DFEDTAR']))
fed_rates[, 'd_DFEDTARL'] <- c(NA, diff(fed_rates[, 'DFEDTARL']))
fed_rates[, 'd_DFEDTARU'] <- c(NA, diff(fed_rates[, 'DFEDTARU']))

#--------------------------------------------------------------------------------
# Generate additional variables for target rate changes
#--------------------------------------------------------------------------------

# Join them into a single variable of rate changes. 
fed_rates[, 'fed_chg'] <- NA
fed_rates[!is.na(fed_rates[, 'd_DFEDTAR']), 'fed_chg'] <- 
  fed_rates[!is.na(fed_rates[, 'd_DFEDTAR']), 'd_DFEDTAR']
fed_rates[!is.na(fed_rates[, 'd_DFEDTARL']), 'fed_chg'] <- 
  fed_rates[!is.na(fed_rates[, 'd_DFEDTARL']), 'd_DFEDTARL']
fed_rates[fed_rates[, 'DATE'] == '2008-12-16', 'fed_chg'] <- -0.75

# Aggregate by calendar month. 
fed_rates[, 'month'] <- substr(fed_rates[, 'DATE'], 1, 7)
fed_monthly <- aggregate(fed_rates[, c('fed_chg', 'zlb_ind')], 
                         by = list(fed_rates[, 'month']), 
                         FUN = 'sum')
colnames(fed_monthly) <- c('month', 'fed_chg', 'zlb_ind')

# Aggregate ZLB indicator to indicate complete months at the ZLB. 
fed_monthly[, 'zlb_ind'] <- fed_monthly[, 'zlb_ind'] >= 28



#--------------------------------------------------------------------------------
# Categorize target rate changes
#--------------------------------------------------------------------------------


# Cut the fed target rate changes into categories. 
# Label them by the modal change. 
fed_rates[, 'fed_jump'] <- cut(fed_rates[, 'fed_chg'], 
                               breaks = c(-Inf,-0.26,-0.01,0,0.25,Inf), 
                               labels = seq(-2,2))
fed_monthly[, 'fed_jump'] <- cut(fed_monthly[, 'fed_chg'], 
                                 breaks = c(-Inf,-0.26,-0.01,0,0.25,Inf), 
                                 labels = seq(-2,2))


#--------------------------------------------------------------------------------
# Mark ZLB dates as additional rate change category
#--------------------------------------------------------------------------------

# Initialize new factor. 
fed_monthly[, 'fed_jump_zlb'] <- NA
levels(fed_monthly[, 'fed_jump_zlb']) <- c('-9', 
                                           levels(fed_monthly[, 'fed_jump']))
class(fed_monthly[, 'fed_jump_zlb'])
levels(fed_monthly[, 'fed_jump_zlb'])

# Assign values according to ZLB indicator.
fed_monthly[fed_monthly[, 'zlb_ind'], 'fed_jump_zlb'] <- '-9'
fed_monthly[!fed_monthly[, 'zlb_ind'], 'fed_jump_zlb'] <- 
  fed_monthly[!fed_monthly[, 'zlb_ind'], 'fed_jump']


################################################################################
# Load series of predictor variables.
################################################################################

# Specify parameters for critical values.
file_tag <- 'MZLB_revised_'
cv_version <- 1
folder_name <- 'MZLB_data'

in_file_name <- sprintf('%s/%s%d.csv', 
                        folder_name, file_tag, cv_version)

# Load table of daily interest rates and ranges. 
mzlb <- read.csv(file = in_file_name)


#--------------------------------------------------------------------------------
# Join Fed jump series.
#--------------------------------------------------------------------------------

# Simple join by appending columns as is. 
mzlb <- cbind(mzlb, fed_monthly)
colnames(mzlb)

# Verify that dates are aligned.
table(mzlb[, 'date'] == mzlb[, 'month'], useNA = 'ifany')
# All good.



################################################################################
# Transformations and Data Preparation
################################################################################


#--------------------------------------------------------------------------------
# Unemployment Gap
#--------------------------------------------------------------------------------

# To be replaced by the unemployment gap (still highly persistent).
raw_unemp_var_list <- c('nrou', 'unemp', 'unemp_sa', 'unemp_ns')

mzlb[, 'unemp_gap_1'] <- mzlb[, 'unemp'] - mzlb[, 'nrou']
mzlb[, 'unemp_gap_2'] <- mzlb[, 'unemp_sa'] - mzlb[, 'nrou']
mzlb[, 'unemp_gap_3'] <- mzlb[, 'unemp_ns'] - mzlb[, 'nrou']

#--------------------------------------------------------------------------------
# Yield Curve
#--------------------------------------------------------------------------------

# Both 20 and 30 year issues have missing data: 
# summary(mzlb[, c('tb20yr_cm', 'tb30yr_cm')])
# The zeros are actually missing values.
mzlb[is.na(mzlb[, 'tb20yr_cm']) | 
       mzlb[, 'tb20yr_cm'] == 0, 'tb20yr_cm'] <- NA
mzlb[is.na(mzlb[, 'tb30yr_cm']) | 
       mzlb[, 'tb30yr_cm'] == 0, 'tb30yr_cm'] <- NA

# So similar it doesn't make much difference. 
# Easiest to impute missing values from each series. 
mzlb[is.na(mzlb[, 'tb20yr_cm']), 'tb20yr_cm'] <- mzlb[is.na(mzlb[, 'tb20yr_cm']), 'tb30yr_cm']
mzlb[is.na(mzlb[, 'tb30yr_cm']), 'tb30yr_cm'] <- mzlb[is.na(mzlb[, 'tb30yr_cm']), 'tb20yr_cm']

# Estimate principal components to summarize data. 
yield_curve_vars <- c("tb3mo_cm", "tb6mo_cm", "tb1yr_cm", "tb2yr_cm",     
                      "tb3yr_cm", "tb5yr_cm", "tb7yr_cm",    
                      "tb10yr_cm", "tb20yr_cm", "tb30yr_cm")

yield_pca <- prcomp(mzlb[1:(nrow(mzlb)-1), yield_curve_vars], 
                    center = TRUE,scale. = TRUE)

summary(yield_pca)
# The first two carry most variation. 

# Extract the predicted values of the 
# first two principal components.
mzlb[, c('yield_pc1', 'yield_pc2')] <- NA
mzlb[1:(nrow(mzlb)-1), c('yield_pc1', 'yield_pc2')] <- 
  predict(yield_pca)[, 1:2]

#--------------------------------------------------------------------------------
# Differencing for Stationarity
#--------------------------------------------------------------------------------

# Omit some variables but most borderline series are acceptable. 

diff_var_list <- c('cpi_urb_all_ns', 'cpi_urb_all_sa', 
                   'pcons_exp', 'wti_oil',
                   'house_tot', 'house_1un', 'house_tot_ns', 'house_1un_ns')

for (var_name in diff_var_list) {
  
  diff_var_name <- sprintf('d_%s', var_name)
  mzlb[, diff_var_name] <- c(NA, diff(mzlb[, var_name]))
  
}


#--------------------------------------------------------------------------------
# Unconventional Monetary Policy
#--------------------------------------------------------------------------------

var_name_1 <- 'shadow_rate'
plot(mzlb[, var_name_1], type = 'l', 
     main = sprintf('Plot of %s', var_name_1))

var_name_1 <- 'soma_hold'
plot(mzlb[, var_name_1], type = 'l', 
     main = sprintf('Plot of %s', var_name_1))



var_name_1 <- 'shadow_rate'
var_name_2 <- 'eff_ffr'
plot(mzlb[, var_name_1], type = 'l', 
     main = sprintf('Plot of %s and %s', var_name_1, var_name_2), 
     ylim = c(-3, 10))
lines(mzlb[, var_name_2], col = 'blue')
lines(mzlb[, var_name_2]*0, col = 'black')

# Generate changes in shadow rate. 
var_name <- 'shadow_rate'
diff_var_name <- sprintf('d_%s', var_name)
mzlb[, diff_var_name] <- c(NA, diff(mzlb[, var_name]))

summary(mzlb[, c('shadow_rate', 'd_shadow_rate')])

hist(mzlb[, c('d_shadow_rate')])

# Categorize as with actual target rate changes.
mzlb[, 'shadow_jump'] <- cut(mzlb[, 'd_shadow_rate'], 
                               # breaks = c(-Inf,-0.26,-0.01,0,0.25,Inf),
                               breaks = c(-Inf,-0.26,-0.1,0.1,0.25,Inf),
                               labels = seq(-2,2))


table(mzlb[, 'shadow_jump'], useNA = 'ifany')

table(mzlb[, 'shadow_jump'])/sum(!is.na(mzlb[, 'shadow_jump']))

# Compare with actual rate jumps outside ZLB period. 
table(mzlb[, 'fed_jump'])/sum(!is.na(mzlb[, 'fed_jump']))

# Similar fractions of -2, 1 and 2, shift of 15% from zero to -1. 
# Sounds plausible. 

# Convert categories back to numbers. 
table(mzlb[, 'shadow_jump'], 
      as.integer(mzlb[, 'shadow_jump']) - 3, useNA = 'ifany')

# Compare in figure. 
mzlb[, 'sum_shadow_jump'] <- NA
mzlb[!is.na(mzlb[, 'shadow_jump']), 'sum_shadow_jump'] <- 
  cumsum(0.25*(as.integer(mzlb[!is.na(mzlb[, 'shadow_jump']), 'shadow_jump']) - 3))


var_name_1 <- 'shadow_rate'
var_name_2 <- 'eff_ffr'
var_name_3 <- 'sum_shadow_jump'
plot(mzlb[, var_name_1], type = 'l', 
     main = sprintf('Plot of %s and %s', var_name_1, var_name_2), 
     ylim = c(-4, 10))
lines(mzlb[, var_name_2], col = 'blue')
lines(mzlb[, var_name_3], col = 'green')
lines(mzlb[, var_name_3]*0, col = 'black')


#--------------------------------------------------------------------------------
# Create a New Target Variable that Includes Shadow Rate
#--------------------------------------------------------------------------------

mzlb[, 'fed_jump_w_shadow'] <- mzlb[, 'fed_jump']

mzlb[!is.na(mzlb[, 'shadow_jump']), 'fed_jump_w_shadow'] <-
  mzlb[!is.na(mzlb[, 'shadow_jump']), 'shadow_jump']

table(mzlb[, 'fed_jump'], 
      mzlb[, 'fed_jump_w_shadow'], useNA = 'ifany')


#--------------------------------------------------------------------------------
# List of Candidate Predictor Variables
#--------------------------------------------------------------------------------

target_var <- 'fed_jump_w_shadow'


# Remove variables to exclude from estimation. 
excl_var_list <- c('date', 'vix', 'lab_mkt_cond', colnames(mzlb)[c(33:54, 68:ncol(mzlb))])
excl_var_list <- c(excl_var_list, diff_var_list, raw_unemp_var_list)

pred_var_list <- colnames(mzlb)[!(colnames(mzlb) %in% excl_var_list)]


# Remove observations with many missing variables.
# incl_obsns <- 2:(nrow(mzlb) - 3)
incl_obsns <- 1:nrow(mzlb) %in% 2:(nrow(mzlb) - 3)
# Logical version is more flexible. 


# Data are ready for modelling.
summary(mzlb[incl_obsns, c(target_var, pred_var_list)])



#--------------------------------------------------------------------------------
# Repeat Estimation on a sequence of Candidate Predictor Variables
#--------------------------------------------------------------------------------

# Start with entire list. 
drop_var_list <- c(excl_var_list)


# Make quick exclusions on variables with close surrogates. 

# Remove vxo and keep vol for volatility. 
drop_var_list <- c(drop_var_list, 'vxo')

# Remove duplicate unemployment gap.
drop_var_list <- c(drop_var_list, 'unemp_gap_2', 'unemp_gap_3')

# Remove alternate inflation variables.
drop_var_list <- c(drop_var_list, 'infl_sa_ann_1', 'infl_sa_ann_2')

# Compare leading indicators.
drop_var_list <- c(drop_var_list, 'lead_ind_norm', 'lead_ind_gdp')


# Compare changes in inflation. 
drop_var_list <- c(drop_var_list, 'd_cpi_urb_all_ns', 'd_cpi_urb_all_sa')

# Compare changes in housing start variables. 
# All are very noisy and rank low in variable importance. 
drop_var_list <- c(drop_var_list, 'd_house_tot', 'd_house_tot_ns', 'd_house_1un', 'd_house_1un_ns')

# Some variables are consistently ranked with low importance. 
drop_var_list <- c(drop_var_list, 'vol', 'd_wti_oil', 'spx_ret', 'd_pcons_exp', 's_o_and_i')

# Compare different measures of inflation. 
# Surveys contain complimentary information. 
# Drop 'infl_exp', which is the lowest performer of the two surveys. 
drop_var_list <- c(drop_var_list, 'infl_exp')



################################################################################
# Model building
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
target_var <- 'fed_jump_w_shadow'
# target_var <- 'fed_jump'
# target_var <- 'fed_jump_zlb'


# Select observations depending on whether ZLB is ignored or excluded.
# incl_obsns <- 1:nrow(mzlb) %in% 2:(nrow(mzlb) - 3)
incl_obsns <- 1:nrow(mzlb) %in% 50:(nrow(mzlb) - 3)
# incl_obsns <- 1:nrow(mzlb) %in% 2:(nrow(mzlb) - 9)
sel_obsns <- incl_obsns
# sel_obsns <- incl_obsns & mzlb[, 'zlb_ind'] == FALSE
# sel_obsns <- incl_obsns & cumsum(mzlb[, 'zlb_ind']) == 0

# head(mzlb[sel_obsns, 'date'])
# tail(mzlb[sel_obsns, 'date'])
# 
# mzlb[mzlb[, 'zlb_ind'] == TRUE, 'date'][1]
# mzlb[mzlb[, 'zlb_ind'] == TRUE, 'date'][sum(mzlb[, 'zlb_ind'])]


#--------------------------------------------------------------------------------
# Estimation on Current List of Candidate Predictor Variables
#--------------------------------------------------------------------------------



# Impose current exclusions and fit the classification tree. 
trim_var_list <- colnames(mzlb)[!(colnames(mzlb) %in% drop_var_list)]

# Impose current exclusions but add a ZLB indicator. 
# trim_var_list <- c(trim_var_list, 'zlb_ind')



# Specify model equation.
fmla_string <- sprintf('%s ~ %s', target_var, 
                       paste(trim_var_list, collapse = " + "))

fmla <- as.formula(fmla_string)


# Grow initial tree 

fed_tree <- rpart(fmla,
                  method = "class", 
                  data = mzlb[sel_obsns, ], 
                  control = rpart.control(minsplit = 12, 
                                          minbucket = 3, 
                                          # minsplit = 20, 
                                          # minbucket = 7, 
                                          # cp = 0.02, 
                                          cp = 0.01,
                                          maxcompete = 4, 
                                          maxsurrogate = 5, 
                                          usesurrogate = 2, 
                                          xval = 10,
                                          # xval = 10,
                                          surrogatestyle = 0, 
                                          maxdepth = 30))

# Visualize cross-validation results.
plotcp(fed_tree)

# Display the summary statistics for splits.
printcp(fed_tree)  

# Detailed summary of splits. 
# summary(fed_tree) 

# Display variable importance statistics. 
fed_tree$variable.importance


# Plot the node value. 
plot(fed_tree$where)
table(fed_tree$where, useNA = 'ifany')


# Plot the expected rate change. 
pred_class <- sprintf('class_%s', levels(mzlb[, 'fed_jump']))
mzlb[, pred_class] <- NA
mzlb[, pred_class] <- predict(fed_tree, newdata = mzlb)
jump_values <- 0.25*(as.integer(levels(mzlb[, 'fed_jump'])) + 0)
mzlb[, 'pred_jump'] <- 0
mzlb[, 'pred_class'] <- '-8'
levels(mzlb[, 'pred_class']) <- c('-9', '-8', levels(mzlb[, 'fed_jump']))
mzlb[, 'pred_max_prob'] <- 0
for (class_num in 1:length(pred_class)) {
  
  # Calculate the expected jump size. 
  mzlb[, 'pred_jump'] <- mzlb[, 'pred_jump'] + 
    jump_values[class_num]*mzlb[, pred_class[class_num]]
  
  # Determine which rows have a new high probability for this class.
  pred_class_tag <- levels(mzlb[, 'fed_jump'])[class_num]
  new_high_prob_class <- mzlb[, pred_class[class_num]] >  mzlb[, 'pred_max_prob']
  
  # Label the predicted class, by maximum probability. 
  mzlb[new_high_prob_class, 'pred_class'] <- pred_class_tag
  
  # Update maximum probability.
  mzlb[new_high_prob_class, 'pred_max_prob'] <- 
    mzlb[new_high_prob_class, pred_class[class_num]]
  
}
summary(mzlb[, pred_class])
summary(mzlb[, 'pred_jump'])
plot(mzlb[, 'pred_jump'])

plot(mzlb[, 'pred_jump'], 
     0.25*(as.integer(mzlb[, 'fed_jump']) - 3))

# Verify correct maximum class probability. 
head(mzlb[, c(pred_class, 'pred_max_prob')])
tail(mzlb[, c(pred_class, 'pred_max_prob')])

# Confusion matrix.
table(mzlb[, target_var], 
      mzlb[, 'pred_class'], useNA = 'ifany')

pct_correct <- sum(mzlb[, target_var] == 
                     mzlb[, 'pred_class'], na.rm = TRUE) / 
  sum(!is.na(mzlb[, target_var] == mzlb[, 'pred_class']))



pct_zero <- sum(mzlb[, target_var] == 0, na.rm = TRUE)






# Prune this tree to lump together similar leaf nodes. 
pr_fed_tree<- prune(fed_tree, cp = fed_tree$cptable[5,"CP"])


printcp(pr_fed_tree)  
pr_fed_tree$variable.importance

plot(pr_fed_tree$where)
table(pr_fed_tree$where, useNA = 'ifany')

