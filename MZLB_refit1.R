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
# This version is designed for an initial data inspection 
# and preliminary modeling with new variables and more observations. 
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

# Load libraries designed for nonstationary time series.
# install.packages('urca')
library(urca)
# install.packages('tseries')
library(tseries)


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

# Analyze and calculate discrete rate changes. 

summary(fed_rates)
head(fed_rates)
tail(fed_rates)

plot(fed_rates[, 'DFF'], type = 'l', 
     main = 'Federal Funds Rate and Targets', 
     xlab = 'Date', 
     ylab = 'Interest Rate')
lines(fed_rates[, 'DFEDTAR'], col = 'red')
lines(fed_rates[, 'DFEDTARU'], col = 'blue')
lines(fed_rates[, 'DFEDTARL'], col = 'blue')

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
fed_monthly <- aggregate(fed_rates[, 'fed_chg'], 
                         by = list(fed_rates[, 'month']), 
                         FUN = 'sum')
colnames(fed_monthly) <- c('month', 'fed_chg')
summary(fed_monthly)
table(fed_monthly[, 'fed_chg'])
nrow(fed_monthly)

# Compare the tables of counts by discrete rate change 
# before and after aggregating by month. 
# There is not much of a difference, since a single rate change occurs
# in a particular month. 
# When there are more than one, it does not usually bump into a different category. 
table(fed_rates[, 'fed_chg'])
table(fed_monthly[, 'fed_chg'])


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
# not much variation is lost by categorizing in this way. 
table(fed_rates[, 'fed_jump'])
table(fed_monthly[, 'fed_jump'])


summary(fed_rates)

# Monthly rate jumps are ready to join to predictor variables. 
head(fed_monthly)
tail(fed_monthly)

plot(cumsum(as.numeric(fed_monthly[-1, 'fed_jump']) - 3), # type = 'l', 
     main = 'Discrete Jumps in Federal Funds Rate and Targets', 
     xlab = 'Date', 
     ylab = 'Interest Rate Jumps')




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

# Simple join by adding columns as is. 
mzlb <- cbind(mzlb, fed_monthly)
colnames(mzlb)

# Verify that dates are aligned.
table(mzlb[, 'date'] == mzlb[, 'month'], useNA = 'ifany')
# All good.


################################################################################
# Data Transformations
################################################################################

# Specify a list of variables. 
var_list <- colnames(mzlb)[c(2:49)]
n_var <- length(var_list)


#--------------------------------------------------------------------------------
# Initial Inspections
#--------------------------------------------------------------------------------

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

# Note gaps in interest rate series.
# Use constant maturity series instead of seasoned issues. 
# (both for consistency with yield curve and to avoid gaps).
# Both 20 and 30 year issues have missing data: 
# Take average for long rate. 
# Replace with principal components of the yield curve. 


var_name <- ''

var_num <- 0

var_num <- var_num +1
var_name <- var_list[var_num]
plot(mzlb[, var_name], type = 'l', 
     main = sprintf('Plot of %s', var_name))
print(var_name)


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



# List the variables with p-values less that 5% in first test.
ur_results[ur_results[, 'n_0_p'] <= 2, 'var_name']
ur_results[ur_results[, 'd_5_p'] <= 2, 'var_name']
ur_results[ur_results[, 'adf_p'] <= 0.05, 'var_name']


ur_results[ur_results[, 'n_0_p'] <= 2 |
             ur_results[, 'd_5_p'] <= 2 |
             ur_results[, 'adf_p'] <= 0.05, 'var_name']

ur_list <- as.character(ur_results[ur_results[, 'n_0_p'] <= 2 |
                                     ur_results[, 'd_5_p'] <= 2 |
                                     ur_results[, 'adf_p'] <= 0.05, 'var_name'])

var_num <- 0

var_num <- var_num +1
var_name <- ur_list[var_num]
plot(mzlb[, var_name], type = 'l', 
     main = sprintf('Plot of %s', var_name))

# SPX and volatility are ok as is. 
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
var_name <- 'nrou'
var_name <- 'unemp'
var_name <- 'unemp_sa'
var_name <- 'unemp_ns'

mzlb[, 'unemp_gap_1'] <- mzlb[, 'unemp'] - mzlb[, 'nrou']
# mzlb[, 'unemp_gap_2'] <- mzlb[, 'unemp_sa'] - mzlb[, 'nrou']
# mzlb[, 'unemp_gap_3'] <- mzlb[, 'unemp_ns'] - mzlb[, 'nrou']

var_name <- 'unemp_gap_1'
plot(mzlb[, var_name], type = 'l', 
     main = sprintf('Plot of %s', var_name))
print(var_name)


#--------------------------------------------------------------------------------
# Yield Curve
#--------------------------------------------------------------------------------

# Both 20 and 30 year issues have missing data: 
# Take average for long rate. 

# Estimate principal components to summarize data. 


#--------------------------------------------------------------------------------
# Differencing for Stationarity
#--------------------------------------------------------------------------------

# Omit some variables but most borderline series are acceptable. 

diff_list <- c('cpi_urb_all_ns', 'cpi_urb_all_sa', 
               'pcons_exp', 'wti_oil',
               'house_tot', 'house_1un', 'house_tot_ns', 'house_1un_ns')

for (var_name in diff_list) {
  
  diff_var_name <- sprintf('d_%s', var_name)
  mzlb[, diff_var_name] <- c(NA, diff(mzlb[, var_name]))
  
}

# Inspect transformed variables. 

var_num <- 0

var_num <- var_num +1
var_name <- sprintf('d_%s', diff_list[var_num])
plot(mzlb[, var_name], type = 'l', 
     main = sprintf('Plot of %s', var_name))
# All better. 


#--------------------------------------------------------------------------------
# List of Predictor Variables
#--------------------------------------------------------------------------------

pred_var_list <- colnames(mzlb)[c(2:31)]


################################################################################
# Model building
################################################################################




################################################################################
# End
################################################################################

