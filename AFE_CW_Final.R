###############################################################
# AFE COURSEWORK: 1864173
###############################################################
#set working directory
setwd("~/Library/CloudStorage/OneDrive-UniversityofBristol/TB2/AFE TB2/Summative Coursework")
library ("vars")
library("xts")
library("zoo")
library("urca")
library("ggplot2")
library("forecast")

###############################################################
#### DATA PROCESSING
###############################################################

#Load csv files
sp500 <- read.csv("S&P500.csv")
cpi <- read.csv("International_Financial_Statistics_MonthlyEconomicData_Edited.csv")
tbill <- read.csv("US_3monthTBill_Edited.csv")

#Check storage structure and date formats
str(sp500)
str(cpi)
str(tbill)

#change CPI column name for ease
names(cpi)[2]<-"CPI_Index_Value" 

#convert strings to date (Y for full year, y for short year)
sp500$Date<-as.Date(sp500$Date,"%Y-%m-%d")
cpi$Date<-as.Date(cpi$Date,format = "%d/%m/%Y")
tbill$Date <- as.Date(tbill$Date, format = "%d/%m/%Y")

#Only need "close" for both sp500 and tbill
sp500 <- subset(sp500, select = -c(High, Low, Adj.Close, Open, Volume))
tbill <- subset(tbill, select = -c(High, Low, Adj.Close, Open, Volume))

#Convert to xts for monthly aggregation
sp500_xts<-xts(sp500$Close, sp500$Date)
names(sp500_xts)[1]<-"sp_close"
cpi_xts<-xts(cpi$CPI_Index_Value, cpi$Date)
names(cpi_xts)[1]<-"CPI_index" #Assign column name
tbill_xts<-xts(tbill$Close, tbill$Date)
names(tbill_xts)[1]<-"tbill_rate"

#Convert daily data to monthly for SP500 and TBill
monthly_sp500 <- apply.monthly(sp500_xts, last) #Use end of month as better reflects actual trading level, avoids distortion
monthly_tbill <- apply.monthly(tbill_xts, mean) #Use mean as better captures the overall interest rate environment

#Convert to monthly index "%Y-%m"
index(monthly_sp500) <- as.yearmon(index(monthly_sp500), "%Y-%m")
index(monthly_tbill) <- as.yearmon(index(monthly_tbill), "%Y-%m")
index(cpi_xts) <- as.yearmon(index(cpi_xts), "%Y-%m")

#Merge the three datasets
data <- na.omit(merge(monthly_sp500, monthly_tbill, cpi_xts, all.y = TRUE))
str(data)
data <- data[, -4] #Drop the all.y column from the merge

#Log value to reduce the difference between variables and .... (for variance?)
data <- log(data)

#Change column names (add initials)
names(data)[1]<-"sp_close_TG"
names(data)[2]<-"TBill_rate_TG"
names(data)[3]<-"cpi_index_TG"
str(data)

#Create subsample: 2009–2019
data <- data["2009-01-01/2019-01-01"]

#Save df
saveRDS(data, file = "CW_Final.rds")

###########################################################################
##########TIME SERIES PROPERTIES CHECKS FOR THE THREE VARIABLES############
###########################################################################

# Visual Check to determine ADF test requirements (ie with or without trend/intercept)
# and to identify possible structural breaks
plot(index(data),data$TBill_rate_TG, type="l")
plot(index(data),data$cpi_index_TG, type="l")
plot(index(data),data$sp_close_TG, type="l")

#     VISUAL NOTES:
#     Both CPI and SP500 close appear to have an upward time trend. Therefore start exploring ADF with trend specification
#     Three month treasury bill has a pronounced downwards time trend and then a strong upwards trend from roughly 2016 onward. 
#     Appears to be structural break around 2016. Failure to account for structural breaks typically make it harder for standard 
#     unit-root tests (like ADF or PP) to reject the null of “unit root,” even if the process is actually stationary around a changed mean or trend.

############################# STRUCTURAL BREAKS #############################
library("urca")
# We start by using Zivot–Andrews to detect a structural break in TBill_rate_TG 
z_trend <- ur.za(data$TBill_rate_TG, model="both", lag=1)
summary(z_trend)

# Teststatistic: -4.8072 
# Critical values: 0.01= -5.34 ; 0.05= -4.8 ; 0.1= -4.58 
# Potential breakpoint at 82

ADF_pre_drift <- ur.df(data$TBill_rate_TG[1:80], type="drift",selectlags="AIC")
summary(ADF_pre_drift)
# Test-statistic is: -2.7579; critical value (5% level): -2.89

ADF_pre_trend <- ur.df(data$TBill_rate_TG[1:80], type="trend",selectlags="AIC")
summary(ADF_pre_trend)
# Test-statistic is: -3.0879; critical value (5% level): -3.45

ADF_post_drift <- ur.df(data$TBill_rate_TG[84:121], type="drift",selectlags="AIC")
summary(ADF_post_drift)
# Test-statistic is: -0.7266; critical value (5% level): -3.58

ADF_post_trend <- ur.df(data$TBill_rate_TG[84:121], type="trend",selectlags="AIC")
summary(ADF_post_trend)
# Test-statistic is: -1.8283; critical value (5% level): -3.5

#I proceed with pre-break subsample to help ensure that the estimated parameters are stable, as the Johansen test assumes constant relationships in the sample
data <- data[1:80, ]

######################################################
#TIME SERIES PROPERTIES
######################################################
#plots to decide with or without trend for each variable's unit root test specification
plot(index(data),data$TBill_rate_TG, type="l")
plot(index(data),data$cpi_index_TG, type="l")
plot(index(data),data$sp_close_TG, type="l")

# Unit root tests with trend
#step1 of TS versus DS
ADF_tbill_trend <- ur.df(data$TBill_rate_TG, type="trend", selectlags="AIC")
summary(ADF_tbill_trend )
# Test-statistic is: -3.0879; critical value (5% level): -3.45

ADF_cpi_trend  <- ur.df(data$cpi_index_TG, type="trend", selectlags="AIC")
summary(ADF_cpi_trend)
# Test-statistic is: -2.6337; critical value (5% level): -3.45

ADF_sp_trend  <- ur.df(data$sp_close_TG, type="trend", selectlags="AIC")
summary(ADF_sp_trend)
# Test-statistic is: -3.9484 ; critical value (1% level): -4.04 ; critical value (5% level): -3.45
# Both CPI and T Bill rate are non-stationary. 
# ADF with trend + intercept suggests that S&P close series is stationary around a deterministic trend.
# Regardless, I will run a manual ADF on all to test whether the trend term is significant

#Step 2 of TS versus DS: regression on trend only
Dtbill <- na.omit(diff(data$TBill_rate_TG, differences=1))
Dcpi <- na.omit(diff(data$cpi_index_TG, differences=1))
Dsp <- na.omit(diff(data$sp_close_TG, differences=1))

Ltbill <- na.omit(lag(data$TBill_rate_TG, k=1))
Lcpi <- na.omit(lag(data$cpi_index_TG, k=1))
Lsp <- na.omit(lag(data$sp_close_TG, k=1))

LDtbill <- na.omit(lag(Dtbill, k=1))
LDcpi <- na.omit(lag(Dcpi, k=1))
LDsp <- na.omit(lag(Dsp, k=1))

tt <- cbind(Dtbill, Dcpi, Dsp, Ltbill, Lcpi, Lsp, LDtbill, LDcpi, LDsp)
tt <- na.omit(tt)
colnames(tt) <- c("Dtbill", "Dcpi", "Dsp", "Ltbill", "Lcpi", "Lsp", "LDtbill", "LDcpi", "LDsp")

ADFtbill <- lm(tt$Dtbill ~ c(1:length(tt$Dtbill))+tt$LDtbill)
summary(ADFtbill)
#Trend coefficient is not statistically significant. t value = 0.833, Pr(>|t|) = 0.408
ADFcpi <- lm(tt$Dcpi ~ c(1:length(tt$Dcpi))+tt$LDcpi)
summary(ADFcpi)
#Trend coefficient is not statistically significant. t value = -0.795, Pr(>|t|) = 0.429
ADFsp <- lm(tt$Dsp ~ c(1:length(tt$Dsp))+tt$LDsp)
summary(ADFsp)
#Trend coefficient is not statistically significant at 5% significant level. t value = -1.785, Pr(>|t|) = 0.07831
# trend not statistically significant, proceed to ADF without trend


#Step 3 of TS versus DS: ADF without trend
ADF_tbill_drift <- ur.df(data$TBill_rate_TG, type="drift", selectlags="AIC")
summary(ADF_tbill_drift)
# Test-statistic is: -2.7579; critical value (5% level): -2.89

ADF_cpi_drift  <- ur.df(data$cpi_index_TG, type="drift", selectlags="AIC")
summary(ADF_cpi_drift)
# Test-statistic is: -1.3515; critical value (5% level): -2.89

ADF_sp_drift  <- ur.df(data$sp_close_TG, type="drift", selectlags="AIC")
summary(ADF_sp_drift)
# Test-statistic is: -2.5661; critical value (5% level): -2.89
#non-stationary


#Step 4 of TS versus DS: DF without constant
ADF_tbill <- ur.df(data$TBill_rate_TG, type="none")
summary(ADF_tbill)
# Test-statistic is: 0.0443; critical value (5% level): -1.95

ADF_cpi  <- ur.df(data$cpi_index_TG, type="none")
summary(ADF_cpi)
# Test-statistic is: 2.1316; critical value (5% level): -1.95

ADF_sp  <- ur.df(data$sp_close_TG, type="none")
summary(ADF_sp)
# Test-statistic is: 2.9803; critical value (5% level): -1.95

##################################################################################
######################## ALTERNATIVE STATIONARITY TESTS: #########################

#PHILLIPS-PERRON TESTS:
library(tseries)
# Phillips-Perron With intercept and trend:
pp_test_sp_trend <- ur.pp(data$sp_close_TG, type = "Z-tau", model = "trend", lags = "short")
summary(pp_test_sp_trend)
# Phillips-Perron With intercept:
pp_test_cpi_drift <- ur.pp(data$cpi_index_TG, type = "Z-tau", model = "constant", lags = "short")
summary(pp_test_cpi_drift)
pp_test_tbill_drift <- ur.pp(data$TBill_rate_TG, type = "Z-tau", model = "constant", lags = "short")
summary(pp_test_tbill_drift)
#S&P Close: Test statistic (-2.981) > critical value at 10% (-3.16), fail to reject null hypothesis of a unit root
#CPI Index: Test statistic (-1.6462 ) > critical value at 10% (-2.58602), fail to reject null hypothesis of a unit root
#T-Bill Rate: Test statistic (-2.3246) > critical value at 10% (-2.58602), fail to reject null hypothesis of a unit root

#KPSS TESTS:
#KPSS With intercept and trend:
k_test_sp_trend<-ur.kpss(data$sp_close_TG, type="tau", lags="short")
summary(k_test_sp_trend)
#KPSS With intercept 
k_test_cpi_drift<-ur.kpss(data$cpi_index_TG, type="mu", lags="short")
summary(k_test_cpi_drift)
k_test_tbill_drift<-ur.kpss(data$TBill_rate_TG, type="mu", lags="short")
summary(k_test_tbill_drift)
#S&P Close: Test statistic (0.0983) < critical value at 5% (0.146). Fail to reject null of stationarity
#CPI Index: Test statistic (2.0445) > critical value at 1% (0.739)
#T-Bill Rate: Test statistic (1.0885) > critical value at 1% (0.739)

############################################################################################

#####################################
##### DE-TRENDING sp_close_TG #######
#####################################
#To further investigate whether sp_close_TG is trend or difference stationary
# I run a linear regression to estimate the trend in sp_close_TG and remove it from the series.

#Estimated regression equation looks like: y_t_hat = α + beta t
sp_trend_reg <- lm(data$sp_close_TG ~ c(1:length(data$sp_close_TG)))

#residuals(trend_reg): extracts the residuals from the linear model, which are the differences between the actual values and the fitted trend line
# ie. detrended = y_t - y_t_hat = et
detrended_sp <- residuals(sp_trend_reg)

#Plot of detrended series
plot(index(detrended_sp), detrended_sp, type = "line")

#ADF without trend on detrended series
ADF_detrended_SP <- ur.df(detrended_sp, type="drift",selectlags="AIC")
summary(ADF_detrended_SP)
# test-statistic is: -3.9471, more negative than the 1% critical value (-3.51). Reject the null that the series series contains a unit root
# detrended SP_close_TG is stationary and therefore I(0). Doesn't require differencing to make it stationary. 

# ACF plot of Detrended Series
Acf(detrended, main = "ACF of Detrended SP Close")
Pacf(detrended, main = "PACF of Detrended SP Close")

############################################################################################

##################################################
#Determining order of integration
##################################################

ADF_Dtbill_drift <- ur.df(tt$Dtbill, type="drift", selectlags="AIC")
summary(ADF_Dtbill_drift)
# Test-statistic is: -6.0424; critical value (1% level): -3.51

ADF_Dcpi_drift  <- ur.df(tt$Dcpi, type="drift", selectlags="AIC")
summary(ADF_Dcpi_drift)
# Test-statistic is: -5.3144; critical value (1% level): -3.51

ADF_Dsp_drift  <- ur.df(tt$Dsp, type="drift", selectlags="AIC")
summary(ADF_Dsp_drift)
# Test-statistic is: -6.5596; critical value (1% level): -3.51
# ALL I(1), first difference stationary

### PLOTS OF FIRST DIFFERENCED DATA
plot(index(tt),tt$Dtbill, type="l")
plot(index(tt),tt$Dcpi, type="l")
plot(index(tt),tt$Dsp, type="l")


############################################################################################
# DETERMINING WHETHER SP_CLOSE_TG IS DIFFERENCE OR TREND STATIONARY:

# ACF and PACF of level series
Acf(data$sp_close_TG, main = "ACF of sp_close_TG")
Pacf(data$sp_close_TG, main = "PACF of sp_close_TG")

# ACF and PACF of Detrended Series
Acf(detrended, main = "ACF of Detrended SP Close")
Pacf(detrended, main = "PACF of Detrended SP Close")

# ACF and PACF of first-differenced Series
Acf(tt$sp_close_TG, main = "ACF of Dsp_close_TG")
Pacf(tt$sp_close_TG, main = "PACF of Dsp_close_TG")


######################################################
#Determining the number of cointegrating vectors
######################################################
#Graph comparing the series and determining possible cointegration specifications:

# Create scale multiplier that maps TBill range (-10 to 0) to CPI/SP range (0 to 10)
tbill_to_primary <- function(x) (x + 10) * (10/10)
primary_to_tbill <- function(x) (x * (10/10)) - 10
ggplot(data, aes(x = index(data))) +
  # Primary axes variables
  geom_line(aes(y = cpi_index_TG), color = "blue") +
  geom_line(aes(y = sp_close_TG), color = "green") +
  # Transform TBill data to match primary scale
  geom_line(aes(y = tbill_to_primary(TBill_rate_TG)), color = "orange") +
  scale_y_continuous(
    name = "log CPI Index/ log SP Close Price",
    limits = c(0, 10),
    sec.axis = sec_axis(~primary_to_tbill(.), name = "log 3 Month Treasury Bill Rate", breaks = seq(-10, 0, 2))
  ) +
  labs(x = "Date", color = "Legend"
  ) +
  scale_color_manual(values = c("S&P 500 Close Price" = "green", "CPI Index" = "blue", "3-Month T-Bill Rate" = "orange"))
# S&P500 has a clear upward trend, indicative of a deterministic trend. CPI appears relatively stable but with a slight upward slope
# 3Month Treasury Bill rate shows significant fluctuations but doesn't appear to have a consistent upward or downward trend.
# Given clear deterministic trend in S&P, using ecdet = "trend" would be appropriate to account for this trend in the cointegration relationship. 

#Determine lag order for test. Include both trend and a constant.
VARselect(data, lag.max=12, type = "both")
#Lag K = 2 has the lowest AIC

#Trace Test: Restricted trend (Includes a linear trend in the cointegration relationship)
Jtest_trend_lag2 <- ca.jo(data, type = "trace", ecdet = "trend", K = 2, spec = "transitory")
summary (Jtest_trend_lag2)
Jtest_trend_lag12 <- ca.jo(data, type = "trace", ecdet = "trend", K = 12, spec = "transitory")
summary (Jtest_trend_lag12)

#Trace Test: Restricted constant (Includes a constant in the cointegrating equation but no trend.)
Jtest_constant_lag2  <- ca.jo(data, type = "trace", ecdet = "const", K = 2, spec = "transitory")
summary (Jtest_constant_lag2 )
Jtest_constant_lag12 <- ca.jo(data, type = "trace", ecdet = "const", K = 12, spec = "transitory")
summary (Jtest_constant_lag12)

#Trace Test: Unrestricted constant (No deterministic terms (no intercept or trend in the cointegrating equation))
Jtest_none_lag2  <- ca.jo(data, type = "trace", ecdet = "none", K = 2, spec = "transitory")
summary (Jtest_constant_lag2 )
Jtest_none_lag12 <- ca.jo(data, type = "trace", ecdet = "none", K = 12, spec = "transitory")
summary (Jtest_none_lag12)

#Maximal Eigenvalue Test: Restricted trend
JtestE_trend_lag2 <- ca.jo(data, type = "eigen", ecdet = "trend", K = 2, spec = "transitory")
summary (JtestE_trend_lag2)
JtestE_trend_lag12 <- ca.jo(data, type = "eigen", ecdet = "trend", K = 12, spec = "transitory")
summary (JtestE_trend_lag12)

##########################################################################################
# We assume one cointegrating relationship and proceed with r=1 for both lag structures:

#VECM Trend
vecm_trend_2lags <- cajorls(Jtest_trend_lag2, r = 1)
vecm_trend_2lags
vecm_trend_12lags <- cajorls(Jtest_trend_lag12, r = 1)
vecm_trend_12lags

#VECM Constant
vecm_constant_2 <- cajorls(Jtest_constant_lag2, r = 1)
vecm_constant_2
vecm_constant_12 <- cajorls(Jtest_constant_lag12, r = 1)
vecm_constant_12

#VECM None
vecm_none_2 <- cajorls(Jtest_none_lag12, r = 1)
vecm_none_2
vecm_none_12 <- cajorls(Jtest_none_lag12, r = 1)
vecm_none_12

#PLOT OF VECM RESIDUALS:
#K=2 Residual Plot
res2 <- ts(vecm_trend_2$rlm$residuals)
plot(res2,type="l")
# Residuals appear stationary and free of autocorrelation
# This indicates that the VECM effectively captures the underlying dynamics of the data, leaving no systematic patterns in the errors.

#K=12 Residual Plot
res12 <- ts(vecm_trend$rlm$residuals)
plot(res12,type="l")

######################################################################################################################
# ca.jorls function doesn’t provide the estimated standard errors of the coefficients. 
# Use VECM function with tsDyn for this
install.packages("tsDyn")
library("tsDyn")

# VECM Trend (Estimated by ML)
vecm_trend_ML <- VECM(data, lag=1, r=1, include= c("both"), estim=c("ML"))
summary(vecm_trend_ML)
vecm_trend_ML_12 <- VECM(data, lag=11, r=1, include= c("both"), estim=c("ML"))
summary(vecm_trend_ML_12)

# VECM Constant (Estimated by ML)
vecm_constant_ML <- VECM(data, lag=1, r=1, include= c("const"), estim=c("ML"))
summary(vecm_constant_ML)
vecm_constant_ML_12 <- VECM(data, lag=11, r=1, include= c("const"), estim=c("ML"))
summary(vecm_constant_ML_12)

# VECM None (Estimated by ML)
vecm_none_ML <- VECM(data, lag=1, r=1, include= c("none"), estim=c("ML"))
summary(vecm_none_ML)
vecm_none_ML_12 <- VECM(data, lag=11, r=1, include= c("none"), estim=c("ML"))
summary(vecm_none_ML_12)

######################################################################################################################

######################################################
# Results Interpretation 
######################################################

#Use the vec2var function to convert the VECM to a VAR format to obtain the irfs and variance decomposition.
VAR <- vec2var(Jtest_trend_lag2, r=1) #converts VECM into unrestricted VAR model

#Prediction (12 Month Forecast)
tt <- predict(VAR, n.ahead = 12, ci = 0.95, dumvar = NULL)
#Forecast visualization
fanchart(tt)

#SP Impulse response functions (IRF)
irf_sp_tbill <- irf(VAR, impulse="TBill_rate_TG", response=c("sp_close_TG"))
plot(irf_sp_tbill)
irf_sp_cpi <- irf(VAR, impulse="cpi_index_TG", response=c("sp_close_TG"))
plot(irf_sp_cpi)
irf_sp_sp <- irf(VAR, impulse="sp_close_TG", response=c("sp_close_TG"))
plot(irf_sp_sp)

#CPI Impulse response functions (IRF)
irf_cpi_tbill <- irf(VAR, impulse="TBill_rate_TG", response=c("cpi_index_TG"))
plot(irf_sp_tbill)
irf_cpi_cpi <- irf(VAR, impulse="cpi_index_TG", response=c("cpi_index_TG"))
plot(irf_sp_cpi)
irf_cpi_sp <- irf(VAR, impulse="sp_close_TG", response=c("cpi_index_TG"))
plot(irf_cpi_sp)

#T Bill Impulse response functions (IRF)
irf_tbill_tbill <- irf(VAR, impulse="TBill_rate_TG", response=c("TBill_rate_TG"))
plot(irf_tbill_tbill)
irf_tbill_cpi <- irf(VAR, impulse="cpi_index_TG", response=c("TBill_rate_TG"))
plot(irf_tbill_cpi)
irf_tbill_sp <- irf(VAR, impulse="sp_close_TG", response=c("TBill_rate_TG"))
plot(irf_tbill_sp)

#Variance Decomposition
fevd(VAR, n.ahead=12)

######################################################################################################################

######################################################
# DIAGNOSTIC CHECKS
######################################################

#SERIAL CORRELATION CHECK:

#Portmanteau test for autocorrelation:
serial.test(VAR, lags.pt = 2, type = "PT.adjusted") #Use PT.adjusted for small sample (n = 80)
# p-value is high (0.1162 > 0.05), so fail to reject the null hypothesis of no serial correlation in the residuals.
#Suggests that VAR model (VARA) has adequately captured the time dependence in the data

#Breusch-Godfrey LM test for autocorrelation:
serial.test(VAR, lags.pt = 2, type = "BG")
#p-value is high (0.3919 > 0.05), so fail to reject the null hypothesis of no serial correlation in the residuals.


# NORMALLY DISTRIBUTED RESIDUALS CHECK:
normality.test(VAR, multivariate.only = TRUE)

# 1. JB-Test (multivariate): Jarque-Bera test for overall normality.
#     the null hypothesis (H0) is that the residuals are multivariate normal.
#     With a chi-squared statistic of 31.668 and a p-value = 1.889e-05 (essentially 0), strongly reject the null hypothesis at any reasonable significance level

# 2. Skewness only (multivariate): Tests whether the residuals have zero skewness (symmetry).
#     The chi-squared statistic of 19.923 with a p-value = 0.0001761 leads to rejection of the null hypothesis that the residuals have zero skewness (are symmetric)

# 3. Kurtosis only (multivariate): Tests whether the residuals have kurtosis consistent with a normal distribution (typically kurtosis = 3 for a normal distribution).
#     The chi-squared statistic of 11.745 with a p-value = 0.00831 leads to rejection of the null hypothesis that the residuals have kurtosis consistent with normality.

# THEREFORE residuals of VECM model (VAR) are not multivariate normal (p-values < 0.01 for JB, skewness, and kurtosis), showing significant skewness and excess kurtosis.
# Non-normal Residuals violate violating the assumption of multivariate normality often assumed for VAR/ VECM inference 
# can lead to unreliable p-values, confidence intervals, and hypothesis tests, even though parameter estimates remain consistent.

# PARAMETER STABILITY TEST:

# Extract residuals from the VECM
residuals <- residuals(vecm_trend_ML)
residuals_ts <- ts(residuals, start=c(2010, 2), frequency=12) # Start after differencing, monthly data

# Apply OLS-CUSUM to the residuals of the sp_close_TG equation
# Use a formula: regress residuals on a constant (no predictors)
cusum_test <- efp(residuals_ts[, 1] ~ 1, type="OLS-CUSUM")

# Plot the CUSUM test
plot(cusum_test)

# For TBill_rate_TG
cusum_test_TBill <- efp(residuals_ts[, 2] ~ 1, type="OLS-CUSUM")
plot(cusum_test_TBill)

# For cpi_index_TG
cusum_test_cpi <- efp(residuals_ts[, 3] ~ 1, type="OLS-CUSUM")
plot(cusum_test_cpi)

#CUSUM path stays within 95% confidence interval bounds.
#No evidence of parameter instability/ structural breaks at the 5% significance level.
#Paths do fluctuate which is normal due to sampling variability, but they remain bounded, suggesting these fluctuations are not statistically significant


#ARCH EFFECT
arch.test(VAR,lags.multi=2, multivariate.only=TRUE)

#Null Hypothesis: There are no ARCH effects in the residuals—i.e., the conditional variance of the residuals is constant (homoskedasticity).
#p-value = 0.03725, less than the common significance level of 0.05, so we reject the null hypothesis. 

# Presence of ARCH effects indicates that the residuals’ variance is not constant but depends on past squared residuals, showing volatility clustering.
# This violates the homoskedasticity assumption of the standard VAR/ VECM, potentially leading to inefficient estimates, unreliable standard errors, and poor forecasting performance

######################################################################################################################

### RESPONSE TO NON-NORMALITY AND ARCH EFFECT:

#Apply Robust SEs: Use vcovHAC() to adjust inference and re-evaluate coefficient significance.
#Fit VECM-GARCH: Implement a univariate GARCH(1,1) on each residual series using rugarch, then consider a multivariate approach if needed.
#Check Outliers: Inspect residuals for outliers (e.g., mid-2011 dip in the black line) and add dummies if significant.
#Re-run Diagnostics: Confirm that normality and ARCH issues are mitigated.
#Compare Models: Compare the original VECM with the adjusted model (e.g., via AIC/BIC or forecast accuracy) to justify changes.



