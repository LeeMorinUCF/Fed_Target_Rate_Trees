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
# This first script is used to conduct an initial data inspection 
# and preliminary modeling with new variables and more observations. 
# The output is a dataset to estimate a classification tree 
# for Federal Reserve policy in ZLB_II_2_policy_trees.R. 
# 
# Dependencies: 
#   Packages urca and tseries for unit root tests. 
# 
################################################################################


################################################################################
# Setup Workspace and Load Libraries
################################################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory to the folder containing the repo Fed_Target_Rate_Trees.
wd_path <- '~/Research/MZLB-II/MZLB-II_revisions/Fed_Target_Rate_Trees'

setwd(wd_path)

# Set path for data.
data_path <- 'submission/data'

# Set path for saved figures.
fig_path <- 'submission/figs'

# Load libraries designed for nonstationary time series.
# install.packages('urca')
library(urca)
# install.packages('tseries')
library(tseries)

# Load libraries to estimate classification tree with rpart.
# install.packages('rpart')
library(rpart)
# install.packages('rpart.plot')
library(rpart.plot)

################################################################################
# Load Daily Federal Funds Rate series.
################################################################################

# Specify parameters for critical values.
file_tag <- 'ZLBFedYields'
cv_version <- 2

in_file_name <- sprintf('%s/%s%d.csv', 
                        data_path, file_tag, cv_version)

# Load table of daily interest rates and ranges. 
fed_rates <- read.csv(file = in_file_name)


# Analyze and calculate discrete rate changes. 
summary(fed_rates)
head(fed_rates)
tail(fed_rates)

plot(fed_rates[, 'DFF'], type = 'l', 
     main = 'Federal Funds Rate and Targets', 
     xlab = 'Date', 
     ylab = 'Interest Rate', 
     ylim = c(0,10), 
     xaxt='n')
lines(fed_rates[, 'DFEDTAR'], col = 'red')
lines(fed_rates[, 'DFEDTARU'], col = 'blue')
lines(fed_rates[, 'DFEDTARL'], col = 'blue')


# Add dates on axes.
new_year_dates <- (1:nrow(fed_rates))[substr(fed_rates[, 'DATE'], 6,7) == '01' & 
                                        substr(fed_rates[, 'DATE'], 9,10) == '01']
fed_rates[new_year_dates, 'DATE']
new_year_labels <- substr(fed_rates[new_year_dates, 'DATE'], 1,4)

axis(1, at = new_year_dates[seq(4, 32, by = 5)], 
     labels = new_year_labels[seq(4, 32, by = 5)])

#--------------------------------------------------------------------------------
# Include indicator for zero lower bound
#--------------------------------------------------------------------------------

summary(fed_rates[, 'DFEDTARL'])
table(fed_rates[, 'DFEDTARL'] == 0, useNA = 'ifany')
table(fed_rates[, 'DFEDTARL'] == 0, 
      as.character(fed_rates[, 'DATE']) >= '2008-12-16', 
      useNA = 'ifany')

fed_rates[, 'zlb_ind'] <- fed_rates[, 'DFEDTARL'] == 0
fed_rates[is.na(fed_rates[, 'DFEDTARL']), 'zlb_ind'] <- FALSE

# Trust but Verify.
table(fed_rates[, 'DFEDTARL'] == 0, 
      fed_rates[, 'zlb_ind'], useNA = 'ifany')


#--------------------------------------------------------------------------------
# Analyze target rate changes
#--------------------------------------------------------------------------------

# Generate changes in variables. 
fed_rates[, 'd_DFF'] <- c(NA, diff(fed_rates[, 'DFF']))
fed_rates[, 'd_DFEDTAR'] <- c(NA, diff(fed_rates[, 'DFEDTAR']))
fed_rates[, 'd_DFEDTARL'] <- c(NA, diff(fed_rates[, 'DFEDTARL']))
fed_rates[, 'd_DFEDTARU'] <- c(NA, diff(fed_rates[, 'DFEDTARU']))

# Catalogue discrete jumps in discretionary target. 
table(fed_rates[, 'd_DFEDTAR'])
# Note that the only observed changes are in 16ths:
# table(fed_rates[, 'd_DFEDTAR']*16)
# But they are concentrated in 5 categories: 
# (<-0.25), [-0.25, 0), (0), (0,0.25],(>0.25)]

# The target range has only shown nonzero changes of +0.25. 
table(fed_rates[, 'd_DFEDTARU'])
table(fed_rates[, 'd_DFEDTARL'])
table(fed_rates[, 'd_DFEDTARU'], 
      fed_rates[, 'd_DFEDTARL'])
# Changes in the range bounds have coincided. 


# Analyze the history of rate changes. 
table(is.na(fed_rates[, 'd_DFEDTAR']), 
      is.na(fed_rates[, 'd_DFEDTARL']))
# Either one or the other is present, except for two dates. 
fed_rates[is.na(fed_rates[, 'd_DFEDTAR']) & 
                  is.na(fed_rates[, 'd_DFEDTARL']), 'DATE']
# The first is the first observation (dropped by differencing).
# The second is 2008-12-16, the date that the policy changed 
# from a target rate to a target range, 
# which is also the date of first differencing for the ranges. 
fed_rates[is.na(fed_rates[, 'd_DFEDTAR']) & 
            is.na(fed_rates[, 'd_DFEDTARL']), ]


# Call it a jump of -0.75 to -1.00, depending on bound.
# Same jump category either way. 
fed_rates[fed_rates[, 'DATE'] %in% 
            c('2008-12-15', '2008-12-16', '2008-12-17'), ]


#--------------------------------------------------------------------------------
# Generate variables for target rate changes
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
class(fed_rates[, 'fed_chg'])
fed_monthly <- aggregate(fed_rates[, c('fed_chg', 'zlb_ind')], 
                         by = list(fed_rates[, 'month']), 
                         FUN = 'sum')
colnames(fed_monthly) <- c('month', 'fed_chg', 'zlb_ind')
summary(fed_monthly)
table(fed_monthly[, 'fed_chg'], useNA = 'ifany')
nrow(fed_monthly)

# Aggregate ZLB indicator. 
table(fed_monthly[, 'zlb_ind'], useNA = 'ifany')
# Omit both partial months as included in the ZLB indicator.
# For at least part of those months there was athe possibility of a rate change,
# And, in fact, that must have occurred to make a partial month of ZLB. 
fed_monthly[, 'zlb_ind'] <- fed_monthly[, 'zlb_ind'] >= 28
table(fed_monthly[, 'zlb_ind'], useNA = 'ifany')


# Compare the tables of counts by discrete rate change 
# before and after aggregating by month. 
# There is not much of a difference, since a single rate change occurs
# in a particular month. 
# When there are more than one, it does not usually bump into a different category. 
table(fed_rates[, 'fed_chg'])
table(fed_monthly[, 'fed_chg'])


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

# Verify the assignments.
table(fed_rates[, 'fed_chg'], fed_rates[, 'fed_jump'], useNA = 'ifany')
table(fed_monthly[, 'fed_chg'], fed_monthly[, 'fed_jump'], useNA = 'ifany')


# Due to the discrete nature of the typical rate change, 
# and the typical sequence of FOMC meetings, 
# not much variation is lost by categorizing in this way. 
table(fed_rates[, 'fed_jump'])
table(fed_monthly[, 'fed_jump'])
# Although some months include several small changes. 

# Compare with ZLB indicators.
table(fed_rates[, 'fed_jump'], fed_rates[, 'zlb_ind'], useNA = 'ifany')
table(fed_monthly[, 'fed_jump'], fed_monthly[, 'zlb_ind'], useNA = 'ifany')


summary(fed_rates)

# Monthly rate jumps are ready to join to predictor variables. 
head(fed_monthly)
tail(fed_monthly)

plot(cumsum(as.numeric(fed_monthly[-1, 'fed_jump']) - 3), # type = 'l', 
     main = 'Discrete Jumps in Federal Funds Rate and Targets', 
     xlab = 'Date', 
     ylab = 'Interest Rate Jumps')
# Looks similar to the original series, with some deviation
# from the continuous rate changes in between monthly changes. 


#--------------------------------------------------------------------------------
# Mark ZLB dates as additional rate change category
#--------------------------------------------------------------------------------

# Initialize new factor. 
# fed_monthly[, 'fed_jump_zlb'] <- NA
fed_monthly[, 'fed_jump_zlb'] <- factor(levels = c('-9', 
                                                   levels(fed_monthly[, 'fed_jump'])))
# levels(fed_monthly[, 'fed_jump_zlb']) <- c('-9', 
#                                            levels(fed_monthly[, 'fed_jump']))
class(fed_monthly[, 'fed_jump_zlb'])
levels(fed_monthly[, 'fed_jump_zlb'])

# Assign values according to ZLB indicator.
fed_monthly[fed_monthly[, 'zlb_ind'], 'fed_jump_zlb'] <- '-9'
fed_monthly[!fed_monthly[, 'zlb_ind'], 'fed_jump_zlb'] <- 
  fed_monthly[!fed_monthly[, 'zlb_ind'], 'fed_jump']

# Trust but Verify.
table(fed_monthly[, 'fed_jump_zlb'], useNA = 'ifany')
table(fed_monthly[, 'zlb_ind'], 
      fed_monthly[, 'fed_jump_zlb'], useNA = 'ifany')
# Only zero (censored) rate changes are mapped to '-9'.


#--------------------------------------------------------------------------------
# Change category labels to correespond to policy actions. 
#--------------------------------------------------------------------------------


# New caegorical variables have labels for policy actions.
fed_jump_labels <- c('AE', 'ME', 'UN', 'MC', 'AC')

fed_monthly[, 'fed_jump'] <- factor(fed_monthly[, 'fed_jump'], 
                                        levels = levels(fed_monthly[, 'fed_jump']), 
                                        labels = fed_jump_labels)
fed_monthly[, 'fed_jump_zlb'] <- factor(fed_monthly[, 'fed_jump_zlb'], 
                                        levels = levels(fed_monthly[, 'fed_jump_zlb']), 
                                        labels = c('ZLB', fed_jump_labels))


table(fed_monthly[, 'fed_jump'], useNA = 'ifany')
table(fed_monthly[, 'fed_jump_zlb'], useNA = 'ifany')

# Overwrites values but integers map to same order. 
table(as.integer(fed_monthly[, 'fed_jump']), useNA = 'ifany')
table(as.integer(fed_monthly[, 'fed_jump_zlb']), useNA = 'ifany')
# ZLB makes classes off by one. 


################################################################################
# Load series of predictor variables.
################################################################################

# Specify parameters for critical values.
file_tag <- 'MZLB_revised_'
cv_version <- 1

in_file_name <- sprintf('%s/%s%d.csv', 
                        data_path, file_tag, cv_version)

# Load table of daily interest rates and ranges. 
mzlb <- read.csv(file = in_file_name)

# Inspect data for accuracy. 

summary(mzlb)
# head(mzlb)
# tail(mzlb)

ncol(mzlb)
nrow(mzlb)


#--------------------------------------------------------------------------------
# Join Fed jump series.
#--------------------------------------------------------------------------------

# Compare for compatibility.
head(mzlb[, 'date'])
head(fed_monthly)
tail(mzlb[, 'date'])
tail(fed_monthly)


nrow(mzlb)
nrow(fed_monthly)

# Simple join by appending columns as is. 
mzlb <- cbind(mzlb, fed_monthly)
colnames(mzlb)

# Verify that dates are aligned.
table(mzlb[, 'date'] == mzlb[, 'month'], useNA = 'ifany')
# Check.

# Verify that new variables were added. 
colnames(mzlb)
# Check.

# Verify that variables have expected characteristics. 
summary(mzlb)
# Check.


#--------------------------------------------------------------------------------
# Create alternative Fed jump series.
#--------------------------------------------------------------------------------

# Labels added to same factors above.

# # New categorical variables have labels designed for figures and output. 
# # (originals are labeled by number, to simplify the translation to numbers.)
# 
# # Original categorical variables have "numeric" labels.
# table(mzlb[, 'fed_jump'], useNA = 'ifany')
# table(mzlb[, 'fed_jump_zlb'], useNA = 'ifany')
# 
# # New caegorical variables have labels for policy actions. 
# fed_jump_labels <- c('AE', 'ME', 'UN', 'MC', 'AC')
# mzlb[, 'fed_jump_label'] <- factor(levels = fed_jump_labels)
# mzlb[, 'fed_jump_zlb_label'] <- factor(levels = c('ZLB', fed_jump_labels))
# 
# # Map numeric labels to policy action labels.
# mzlb[, 'fed_jump_label'] <- mzlb[, 'fed_jump']...
# mzlb[, 'fed_jump_zlb_label'] <- mzlb[, 'fed_jump_zlb']...
# 
# # Trust but verify. 
# table(mzlb[, 'fed_jump'], 
#       mzlb[, 'fed_jump_label'], useNA = 'ifany')
# table(mzlb[, 'fed_jump_zlb'], 
#       mzlb[, 'fed_jump_zlb_label'], useNA = 'ifany')


################################################################################
# Data Transformations
################################################################################

# Specify a list of variables. 
var_list <- colnames(mzlb)[c(2:length(colnames(mzlb)))]
n_var <- length(var_list)


#--------------------------------------------------------------------------------
# Initial Inspections
#--------------------------------------------------------------------------------

# After each definition of var_name, 
# run this block of code to plot the variable: 
# plot(mzlb[, var_name], type = 'l', 
#      main = sprintf('Plot of %s', var_name))
# print(var_name)

# To be replaced by the unemployment gap (still highly persistent).
var_name <- 'nrou'
var_name <- 'unemp'


# Obvious linear trend.
var_name <- 'cpi_urb_all_ns' # Must be differenced.
var_name <- 'cpi_urb_all_sa' 
var_name <- 'pcons_exp'

# Should probably be differenced.
# Tough call because of crisis. 
var_name <- 'house_tot'
# Looks like linear trend with huge break.
# Take differences.
var_name <- 'wti_oil' # Similarly.


var_name <- ''

var_num <- 0

# Run this block repeatedly to plot each variable in turn:
var_num <- var_num +1
var_name <- var_list[var_num]
plot(mzlb[, var_name], type = 'l', 
     main = sprintf('Plot of %s', var_name))
print(var_name)
# Some variable appear nonstationary. 

#--------------------------------------------------------------------------------
# Unit root tests
#--------------------------------------------------------------------------------

# Initialize data frame for storage of unit root test results. 
ur_results <- data.frame(var_name = var_list, 
                         n_obs = integer(n_var), 
                         n_0_p = numeric(n_var), 
                         d_5_p = numeric(n_var), 
                         adf_p = numeric(n_var))

# Loop on variable names and conduct sequence of unit root tests. 
for (var_num in 1:n_var) {
  
  var_name <- var_list[var_num]
  
  # Remove missing values (only if it leaves consecutive observations).
  not_na <- !is.na(mzlb[, var_name])
  
  # Store count of observations. 
  ur_results[var_num, 'n_obs'] <- length(mzlb[not_na, var_name])
  
  # Start with a simple test from the urca package.
  urca_df_none_0 <- ur.df(mzlb[not_na, var_name], type = "none", lags = 0)
  
  # Store the smallest p-value beyond the statistic. 
  ur_results[var_num, 'n_0_p'] <- cut(urca_df_none_0@teststat, 
                                      breaks = c(-Inf, urca_df_none_0@cval, Inf), 
                                      labels = c(1, 5, 10, 100))
  
  # Alternate test with a linear trend and lags.
  urca_df_drift_5 <- ur.df(mzlb[not_na, var_name], type = "drift", lags = 5)
  
  # Store the smallest p-value beyond the statistic. 
  ur_results[var_num, 'd_5_p'] <- cut(urca_df_drift_5@teststat[, 'tau2'], 
                                      breaks = c(-Inf, urca_df_drift_5@cval['tau2', ], Inf), 
                                      labels = c(1, 5, 10, 100))
  
  # Augmented Dickey-Fuller Unit root tests of levels: tseries package
  adf_test <- adf.test(mzlb[not_na, var_name])
  # Default lag length is nlag = floor(4*(length(x)/100)^(2/9)) = 5
  # with 175 observations, 4*1.75^(2/9) = 4.52
  # with 410 observations, 4*4.1^(2/9) = 5.47311
  # So the default will be 5 for all observations in this dataset.
  
  ur_results[var_num, 'adf_p'] <- adf_test$p.value
  
  
}



# List the variables with p-values less than 5% in first test.
ur_results[ur_results[, 'n_0_p'] <= 2, 'var_name']
ur_results[ur_results[, 'd_5_p'] <= 2, 'var_name']
ur_results[ur_results[, 'adf_p'] <= 0.05, 'var_name']

# List the variables with p-values greater than 5% in first test.
ur_results[ur_results[, 'n_0_p'] >= 2, 'var_name']
ur_results[ur_results[, 'd_5_p'] >= 2, 'var_name']
ur_results[ur_results[, 'adf_p'] >= 0.05, 'var_name']

# Consider any that pass one test for stationarity. 
ur_results[ur_results[, 'n_0_p'] <= 2 |
             ur_results[, 'd_5_p'] <= 2 |
             ur_results[, 'adf_p'] <= 0.05, 'var_name']

# Consider any that fail one test for stationarity. 
ur_results[ur_results[, 'n_0_p'] >= 2 |
             ur_results[, 'd_5_p'] >= 2 |
             ur_results[, 'adf_p'] >= 0.05, 'var_name']


# Visiual inspection of stationary series. 
stat_list <- as.character(ur_results[ur_results[, 'n_0_p'] <= 2 |
                                     ur_results[, 'd_5_p'] <= 2 |
                                     ur_results[, 'adf_p'] <= 0.05, 'var_name'])

# Visiual inspection of potentially nonstationary series. 
ur_list <- as.character(ur_results[ur_results[, 'n_0_p'] <= 2 |
                                       ur_results[, 'd_5_p'] <= 2 |
                                       ur_results[, 'adf_p'] <= 0.05, 'var_name'])
# To avoid false positives (negatives?) select based on the ADF test with lags. 
ur_list <- ur_results[ur_results[, 'adf_p'] >= 0.05, 'var_name']


# Choose a list to inspect.
plot_list <- as.character(ur_list)
# plot_list <- as.character(stat_list)

var_num <- 0

# Run this block repeatedly to plot each variable in turn:
var_num <- var_num +1
var_name <- plot_list[var_num]
plot(mzlb[, var_name], type = 'l', 
     main = sprintf('Plot of %s', var_name))

# Comments:
# SPX returns and volatility are ok as is. 
# NROU has pronounced trend.
# lab_mkt_cond looks stationary with high persistance. 
# Infl is tolerable.
# Rec probs is highly persistant but bounded. 
# plo_unc, fut_inv_surv, infl_exp, s_o_and_i, 
# lead_ind_adj, lead_ind_norm, lead_ind_gdp
# all look stationary with high persistance. 
# house_tot_ns has high seasonality
# FFR is the target, tbills should be omitted or replaced accordingly. 


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



# Add dates on axes.
new_year_dates <- (1:nrow(mzlb))[substr(mzlb[, 'date'], 6,7) == '01']
mzlb[new_year_dates, 'date']
new_year_labels <- substr(mzlb[new_year_dates, 'date'], 1,4)

five_year_dates <- new_year_dates[seq(4, 32, by = 5)]
five_year_labels <- new_year_labels[seq(4, 32, by = 5)]


# Plot the new unemployment variable. 
var_name <- 'unemp_gap_1'
plot(mzlb[, var_name], type = 'l', 
     main = sprintf('Plot of %s', var_name), 
     xaxt='n')

axis(1, at = five_year_dates, 
     labels = five_year_labels)

print(var_name)


# Plot with the ZLB indicator.
lines(mzlb[, 'zlb_ind']*1, col = 'blue')

# Add the cutoff at 2 to indicate (most of) ZLB.
lines(rep(2, nrow(mzlb)), col = 'black', lty = 'dashed')

# The unemployment gap gives a good indication of the recession in the ZLB period. 


# A few other variables wil complete the picture, once the model is estimated. 


#--------------------------------------------------------------------------------
# Yield Curve
#--------------------------------------------------------------------------------


# Note gaps in interest rate series.
# Use constant maturity series instead of seasoned issues. 
# (both for consistency with yield curve and to avoid gaps).
# Both 20 and 30 year issues have missing data: 
# Impute from one to the other when missing (both are very similar). 
# Replace interest rate series with principal components of the yield curve. 


# Both 20 and 30 year issues have missing data: 
summary(mzlb[, c('tb20yr_cm', 'tb30yr_cm')])
# The zeros are actually missing values.
mzlb[is.na(mzlb[, 'tb20yr_cm']) | 
       mzlb[, 'tb20yr_cm'] == 0, 'tb20yr_cm'] <- NA
mzlb[is.na(mzlb[, 'tb30yr_cm']) | 
       mzlb[, 'tb30yr_cm'] == 0, 'tb30yr_cm'] <- NA

# Plot the 20- and 30-year interest rates 
# to verify similarity. 
var_name_1 <- 'tb20yr_cm'
var_name_2 <- 'tb30yr_cm'
plot(mzlb[, var_name_1], type = 'l', 
     main = sprintf('Plot of %s and %s', var_name_1, var_name_2))
lines(mzlb[, var_name_2], col = 'blue')

# They are so similar that it doesn't make much difference. 
# Easiest to impute missing values from each series. 
table(is.na(mzlb[, 'tb20yr_cm']), is.na(mzlb[, 'tb30yr_cm']))
# Only the last observation is missing in common. 
mzlb[is.na(mzlb[, 'tb20yr_cm']), 'tb20yr_cm'] <- mzlb[is.na(mzlb[, 'tb20yr_cm']), 'tb30yr_cm']
mzlb[is.na(mzlb[, 'tb30yr_cm']), 'tb30yr_cm'] <- mzlb[is.na(mzlb[, 'tb30yr_cm']), 'tb20yr_cm']


# Estimate principal components to summarize data. 
yield_curve_vars <- c("tb3mo_cm", "tb6mo_cm", "tb1yr_cm", "tb2yr_cm",     
                 "tb3yr_cm", "tb5yr_cm", "tb7yr_cm",    
                 "tb10yr_cm", "tb20yr_cm", "tb30yr_cm")
summary(mzlb[1:(nrow(mzlb)-1), yield_curve_vars])


yield_pca <- prcomp(mzlb[1:(nrow(mzlb)-1), yield_curve_vars], 
                    center = TRUE,scale. = TRUE)

summary(yield_pca)
# The first two carry most variation. 

# Extract the predicted values of the 
# first two principal components.
summary(predict(yield_pca)[, 1:2])

mzlb[, c('yield_pc1', 'yield_pc2')] <- NA
mzlb[1:(nrow(mzlb)-1), c('yield_pc1', 'yield_pc2')] <- 
  predict(yield_pca)[, 1:2]


# Plot the first two principal components of the yield curve. 
var_name_1 <- 'yield_pc1'
var_name_2 <- 'yield_pc2'
plot(mzlb[, var_name_1], type = 'l', 
     main = sprintf('Plot of %s and %s', var_name_1, var_name_2))
lines(mzlb[, var_name_2], col = 'blue')




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

# Inspect transformed variables. 
var_num <- 0

# Run repeatedly to plot each differenced variable in turn. 
var_num <- var_num +1
var_name <- sprintf('d_%s', diff_var_list[var_num])
plot(mzlb[, var_name], type = 'l', 
     main = sprintf('Plot of %s', var_name))
# All better. 


#--------------------------------------------------------------------------------
# List of Candidate Predictor Variables
#--------------------------------------------------------------------------------

target_var <- 'fed_jump'


# Remove variables to exclude from estimation. 

# Some variables are ineligible.
excl_var_list <- c('date')

# Some variables have missing data and have close substitutes.
excl_var_list <- c(excl_var_list, 'vix', 'lab_mkt_cond')

# Interest rate data are excluded from predictor variables. 
# They are either dependent variables, used for evaluation post-estimation, 
# or are replaced with the yield curve. 
excl_var_list <- c(excl_var_list, 
                   colnames(mzlb)[which(colnames(mzlb) == 'soma_hold') : 
                                    which(colnames(mzlb) == 'fed_jump_zlb')])


# Remove excluded variables or those that had to be differenced. 
excl_var_list <- c(excl_var_list, diff_var_list, raw_unemp_var_list)

# Initial list of predictor variables. 
pred_var_list <- colnames(mzlb)[!(colnames(mzlb) %in% excl_var_list)]


# Remove head and tail observations with many missing variables.
# incl_obsns <- 2:(nrow(mzlb) - 3)
incl_obsns <- 1:nrow(mzlb) %in% 2:(nrow(mzlb) - 3)
# Logical version is more flexible. 


# Data are ready for modelling.
summary(mzlb[incl_obsns, c(target_var, pred_var_list)])


################################################################################
# Preliminary Model building and Investigation
################################################################################


#--------------------------------------------------------------------------------
# Repeated Estimation of rpart() function (in the next script)
# on a Sequence of Candidate Predictor Variables
# Then dropped several that were close surrogates.
#--------------------------------------------------------------------------------


# Start with entire list. 
drop_var_list <- c(excl_var_list)


# Make quick exclusions on variables with close surrogates. 

# Remove vxo and keep vol for volatility. 
drop_var_list <- c(drop_var_list, 'vxo')

# Remove duplicate unemployment gap.
drop_var_list <- c(drop_var_list, 'unemp_gap_2', 'unemp_gap_3')

# Remove alternate inflation variables.
plot(mzlb[incl_obsns, 'infl_sa_6m'], type = 'l')
lines(mzlb[incl_obsns, 'infl_sa_ann_1'], col = 'blue') # Similar
lines(mzlb[incl_obsns, 'infl_sa_ann_2'], col = 'red') # Noisier
lines(mzlb[incl_obsns, 'fut_infl_surv'], col = 'green') # Different. 
drop_var_list <- c(drop_var_list, 'infl_sa_ann_1', 'infl_sa_ann_2')

# Compare leading indicators.
plot(mzlb[incl_obsns, 'lead_ind_adj'], type = 'l')
lines(mzlb[incl_obsns, 'lead_ind_norm'], col = 'blue') # Almost identical
lines(mzlb[incl_obsns, 'lead_ind_gdp'], col = 'red') # Smoothed and lagged
drop_var_list <- c(drop_var_list, 'lead_ind_norm', 'lead_ind_gdp')


# Compare changes in inflation. 
plot(mzlb[incl_obsns, 'd_cpi_urb_all_ns'], 
     mzlb[incl_obsns, 'd_cpi_urb_all_sa']) # Highly correlated. 
drop_var_list <- c(drop_var_list, 'd_cpi_urb_all_ns', 'd_cpi_urb_all_sa')

# Compare changes in housing start variables. 
# Compare leading indicators.
plot(mzlb[incl_obsns, 'd_house_tot_ns'], type = 'l')
lines(mzlb[incl_obsns, 'd_house_1un'], col = 'blue')
lines(mzlb[incl_obsns, 'd_house_1un_ns'], col = 'red')
# All are very noisy and rank low in variable importance. 
drop_var_list <- c(drop_var_list, 'd_house_tot', 'd_house_tot_ns', 'd_house_1un', 'd_house_1un_ns')

# Some variables are consistently ranked with low importance. 
drop_var_list <- c(drop_var_list, 'vol', 'd_wti_oil', 'spx_ret', 'd_pcons_exp', 's_o_and_i')

# Compare different measures of inflation. 
plot(mzlb[incl_obsns, 'infl_sa_6m'], type = 'l')
lines(mzlb[incl_obsns, 'fut_infl_surv'], col = 'blue')
lines(mzlb[incl_obsns, 'infl_exp'], col = 'red')

plot(mzlb[incl_obsns, 'fut_infl_surv'], 
     mzlb[incl_obsns, 'infl_exp'])
# Very similar. 
plot(mzlb[incl_obsns, 'infl_sa_6m'], 
     mzlb[incl_obsns, 'infl_exp'])
plot(mzlb[incl_obsns, 'infl_sa_6m'], 
     mzlb[incl_obsns, 'fut_infl_surv'])
# Surveys contain complimentary information. 
# Drop 'infl_exp', which is the lowest performer of the two surveys. 
drop_var_list <- c(drop_var_list, 'infl_exp')

# Compare indices of consumner sentiment. 
plot(mzlb[incl_obsns, 'cons_conf_surv'], 
     mzlb[incl_obsns, 'cons_sent'])
# They're almost collinear.  
drop_var_list <- c(drop_var_list, 'cons_conf_surv')


# Of the yield curve, the second principal component is more important. 
# A twisting of the yield curve is more important than the level. 
drop_var_list <- c(drop_var_list, 'yield_pc1')

# Smoothed recession probabilities use forward-looking information. 
# This is in contrast to real-time recession probabilities, 
# which are based on only currently available information. 
drop_var_list <- c(drop_var_list, 'rec_prob')

# Policy Uncertainty index is the next least predictive variable. 
drop_var_list <- c(drop_var_list, 'pol_unc')


#--------------------------------------------------------------------------------
# Generating Lags of Remaining Candidate Predictor Variables
#--------------------------------------------------------------------------------

# Impose current exclusions and fit the classification tree. 
trim_var_list <- colnames(mzlb)[!(colnames(mzlb) %in% drop_var_list)]

for (var_name in trim_var_list) {
  
  # First lag.
  lag_var_name <- sprintf('l1_%s', var_name)
  mzlb[, lag_var_name] <- c(NA, mzlb[-nrow(mzlb), var_name])
  
  # Second lag.
  lag_var_name <- sprintf('l2_%s', var_name)
  mzlb[, lag_var_name] <- c(NA, NA, mzlb[-c(nrow(mzlb)-1, nrow(mzlb)), var_name])
  
}
# Verify lags.
head(mzlb[, c(trim_var_list[1], sprintf('l1_%s', trim_var_list[1]), 
              sprintf('l2_%s', trim_var_list[1]))])
# Check.

#--------------------------------------------------------------------------------
# Specification of Final List of Candidate Predictor Variables
#--------------------------------------------------------------------------------


# Include the chosen number of lags of variables. 
# One lag:
# pred_var_list <- c(trim_var_list, 
#                    sprintf('l1_%s', trim_var_list))
# Two lags:
pred_var_list <- c(trim_var_list,
                   sprintf('l1_%s', trim_var_list),
                   sprintf('l2_%s', trim_var_list))

# Variable list is ready for the classification models 
# fit in ZLB_II_2_policy_trees.R. 


################################################################################
# Plot Figures and Save for Manuscript
################################################################################


#--------------------------------------------------------------------------------
# Plot macroeconomic variables during ZLB period. 
#--------------------------------------------------------------------------------



# Plot the components one at a time for inspection. 
plot(mzlb[, 'fed_funds'], type = 'l', 
     main = c('Key Macroeconomic Variables', 
              'during the Zero Lower Bound Episode'),
     xlab = 'Date', 
     ylab = 'Percent (Annual)', 
     cex.main = 1.5, 
     cex.lab = 1.0,  
     xaxt='n', 
     lwd = 3, col = 'blue', 
     ylim = c(-4, 10))
lines(rep(0, nrow(mzlb)), col = 'black')
axis(1, at = five_year_dates, 
     labels = five_year_labels)

# Olverlay the Federal Funds Rate.
# lines(mzlb[, 'fed_funds'], col = 'blue', lwd = 2)
# Olverlay the  Unemployment gap. 
lines(mzlb[, 'unemp_gap_1'], col = 'green', lwd = 3, lty = 'dashed')

# Add the cutoff at 2 to indicate (most of) ZLB.
lines(rep(2, nrow(mzlb)), col = 'black', lty = 'dashed')
# The unemployment gap tells quite a lot 
# about the recession in the ZLB period. 


# Plot with the ZLB indicator.
# lines(mzlb[, 'zlb_ind']*1, col = 'blue')
# Redundant with Federal Funds Rate.
# Use vertical lines instead. 
abline(v = which(cumsum(mzlb[, 'zlb_ind']) == 1), 
       col = 'black', lty = 'dashed')
abline(v = which(cumsum(mzlb[, 'zlb_ind']) == max(cumsum(mzlb[, 'zlb_ind'])))[1], 
       col = 'black', lty = 'dashed')


# A few other variables complete the picture. 
# summary(mzlb[, 'lead_ind_adj'])
lines(mzlb[, 'lead_ind_adj'] - 100, col = 'red', lwd = 3, lty = 'dotted')
mtext('Leading Indicator', side = 4, line = -1, cex.lab = 1,las = 0)
axis(4, at = seq(-2.5, 10, by = 2.5), labels = seq(-2.5, 10, by = 2.5) + 100)


# Add lines to separate a recovery portion of the ZLB. 
abline(v = 330, 
       col = 'black', lty = 'dashed')
# Unemployment gap still above 0.67.
lines(rep(0.67, nrow(mzlb)), col = 'black', lty = 'dashed')



#--------------------------------------------------------------------------------
# Ready to produce the publishable version. 
#--------------------------------------------------------------------------------


# Plot Figure 1 and output to the figs folder. 
# Create two copies of each to ease 
# building the pdf documents on different platforms. 
fig_type_list <- c('pdf', 'eps')
fig_num <- 1

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
  
  # Plot the components one at a time for inspection. 
  plot(mzlb[, 'fed_funds'], type = 'l', 
       # main = c('Key Macroeconomic Variables', 
       #          'during the Zero Lower Bound Episode'),
       xlab = 'Date', 
       ylab = 'Percent (Annual)', 
       # cex.main = 1.5, 
       cex.lab = 1.0,
       # cex.lab = 1.5,  
       xaxt ='n', 
       lwd = 3, col = 'blue', 
       ylim = c(-4, 10))
  lines(rep(0, nrow(mzlb)), col = 'black')
  axis(1, at = five_year_dates, 
       labels = five_year_labels)
  # Olverlay the  Unemployment gap. 
  lines(mzlb[, 'unemp_gap_1'], col = 'green', lwd = 3, lty = 'dashed')
  # Add the cutoff at 2 to indicate (most of) ZLB.
  lines(rep(2, nrow(mzlb)), col = 'black', lty = 'dashed')
  # The unemployment gap tells quite a lot 
  # about the recession in the ZLB period. 
  # Use vertical lines to denote the ZLB period.  
  abline(v = which(cumsum(mzlb[, 'zlb_ind']) == 1), 
         col = 'black', lty = 'dashed')
  abline(v = which(cumsum(mzlb[, 'zlb_ind']) == max(cumsum(mzlb[, 'zlb_ind'])))[1], 
         col = 'black', lty = 'dashed')
  # A few other variables complete the picture. 
  lines(mzlb[, 'lead_ind_adj'] - 100, col = 'red', lwd = 3, lty = 'dotted')
  mtext('Leading Indicator', side = 4, line = -1, cex.lab = 1.0,las = 0)
  axis(4, at = seq(-2.5, 10, by = 2.5), labels = seq(-2.5, 10, by = 2.5) + 100)
  # Add lines to separate a recovery portion of the ZLB. 
  abline(v = 330, 
         col = 'black', lty = 'dashed')
  # Unemployment gap still above 0.67.
  lines(rep(0.67, nrow(mzlb)), col = 'black', lty = 'dashed')
  
  
  # Close pdf file to save figure.
  dev.off()
  
  
}

#--------------------------------------------------------------------------------




################################################################################
# End
################################################################################

