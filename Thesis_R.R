#install.packages('rpart', repos='http://cran.rstudio.com', type='source')
#install.packages('rpart.plot', repos='http://cran.rstudio.com', type='source')
#install.packages('Metrics', repos='http://cran.rstudio.com', type='source')

#### Packages ####

library(rpart)
library(rpart.plot) #for plots
library(Metrics) #for ML metrics of performance e.g. RMSE/MAE
#### Для прямого импорта - сложности с новой версией R
# install.packages('QuantTools', repos='http://cran.rstudio.com', type='source')
# library( QuantTools )
# Rcpp::evalCpp( code = '2 + 2' ) devtools::install_bitbucket( 'quanttools/QuantTools' )
library(ggplot2)
library("xlsx")
library(stringr)
library(tidyverse)
library(dplyr)
library(xts)
library(zoo)
library(tseries)
library(lubridate)
library(ggfortify) #for autoplot and ts objects to match each other
library(forecast)
library(urca)
library(rusquant)
install.packages('rugarch', repos='http://cran.rstudio.com', type='source')
library(rugarch)
library(gridExtra)
library(grid)
library(fpp)
library(vars)
library(lmtest)
library("readxl")
library(utils)
require(xlsx)
install.packages('PerformanceAnalytics', repos='https://CRAN.R-project.org/package=PerformanceAnalytics', type='source')
library("PerformanceAnalytics")
library("colorspace") #library for colours

#### Part 1 
#### Upload Data #### 
rtsi_15_21 <- data.frame(read.csv("/Users/dariakarpova/Desktop/rtsi15_21.csv", header = TRUE, sep = ";"))
str(rtsi_15_21)
colnames(rtsi_15_21) <- cbind("Ticker", "Period", "Date", "Time", "Open", "High", "Low", "Close", "Volume")
head(rtsi_15_21)
#rtsi_df <- subset(rtsi_15_21, c(1, 3, 8))

#### Leave only relevant info : "Date", "Open", "High", "Low", "Close" 
rtsi_df <- rtsi_15_21[,c(3,5,6,7,8)]
#head(rtsi_df)
#### Convert Date to dates format #### 
#df <- transform(df, x = as.Date(as.character(x), "%Y%m%d"))
dates <- (rtsi_df[,1])
dates_as_date <- ymd(dates)
class(dates_as_date)
head(dates_as_date)

#### Convert dates to a time series indices and "Close" to time series format
rtsi_close <- rtsi_df[,5]
close_ts <- xts(rtsi_close, order.by = dates_as_date)
names(close_ts) <- ("Close")
class(close_ts)
head(close_ts)
#### Display Time Series as a fancy graph
plot(close_ts["2015-01-05/2020-12-30"], main = "RTSI Close Price",  col = "navy")
axis(at = pretty(close_ts))
#### Display to show TS features
tsdisplay(close_ts)

#### Progress to log-returns
rtsi_log_ret <- diff(log(close_ts))[-1] #-1 to exclude first NA
head(rtsi_log_ret)
tsdisplay(rtsi_log_ret)
plot(rtsi_log_ret, main = "RTSI log-returns")

#### Samples ####
rtsi_1518 <- rtsi_log_ret["2015-01-06/2018-12-29"] # n = 1009
rtsi_1519 <- rtsi_log_ret["2015-01-06/2019-12-30"] 
rtsi_1520 <- rtsi_log_ret["2015-01-06/2020-12-30"] #2020: 2020-01-03/2020-12-30 n=250 #total n_15-20 = 1511
rtsi_1919 <- rtsi_log_ret["2019-01-03/2019-12-30"] #n = 252 
rtsi_2020 <- rtsi_log_ret["2020-01-03/2020-12-30"]

plot(rtsi_1520, main = "RTSI returns",  col = "Midnight Blue")
tsdisplay(rtsi_1520)
#### Nice Graph
period = c("2019-01::2020-12")
chart.TimeSeries(rtsi_1520, period.areas = period, period.color = "red") 
                 #event.lines = "2020-01-03", event.color = "blue")

#### Find what happened in 2018 (and when)
fallcheck <- rtsi_log_ret["2018-03-15/2018-05-01"]
plot(fallcheck)
min(fallcheck)
#### Find the dates and min for covid crises
fallcheck_2 <- rtsi_log_ret["2020-01-01/2020-06-20"]
plot(fallcheck_2)
min(fallcheck_2)

#### Proxy Data
proxy_df <- data.frame(read.csv("/Users/dariakarpova/Desktop/proxy_data.csv", header = TRUE, sep = ";"))
str(proxy_df)
#View(proxy_df)
colnames(proxy_df) <- cbind("Ticker", "Period", "Date", "Time", "Open", "High", "Low", "Close", "Volume")
head(proxy_df)
proxy_data <- proxy_df[,c(3,4,5,6,7,8)]
dates_proxy <- (proxy_data[,1:2])
head(dates_proxy)
proxy_days <- ymd(dates_proxy$Date)
class(proxy_days)
#### Convert Close values for proxies to xts 
proxy_close <- proxy_data[,6]
proxy_ts <- xts(proxy_close, order.by = proxy_days) 
names(proxy_ts) <- ("Proxy Close")
class(proxy_ts)
head(proxy_ts)
tsdisplay(proxy_ts)

#### Progress proxy to log-returns
proxy_log_ret <- diff(log(proxy_ts))[-1] #-1 to exclude first NA
head(proxy_log_ret)
tsdisplay(proxy_log_ret)
plot(proxy_log_ret, main = "Proxy log-returns")
proxy_sq <- proxy_log_ret^2
proxy_daily <- apply.daily(proxy_sq, sum)
head(proxy_daily)
#check
sq_2020 <- rtsi_2020^2
plot(proxy_daily, col = "red")
lines(sq_2020, col = "blue")

#proxy_hours <- (dates_proxy$Time)/10000
#binded <- cbind.data.frame(proxy_days, proxy_hours)
#make_datetime() 
#proxy_dates <- ymd(dates_proxy)

#### Structural Brakes Test (CUSUM)
library(CPAT)
CUSUM.test(rtsi_1520, use_kernel_var = FALSE, stat_plot = FALSE,
           kernel = "ba", bandwidth = "and")
#returns do not have structural brakes
CUSUM.test(close_ts["2015-01-06/2020-12-30"], use_kernel_var = FALSE, stat_plot = FALSE,
           kernel = "ba", bandwidth = "and")
#prices do have structural brakes
mean(rtsi_1520)

#### Normality 
# Load the package PerformanceAnalytics and make the histogram
chart.Histogram(rtsi_1520, main = "Histogram and Q-Q plot of log-returns",
                methods = c("add.normal","add.density", "add.qqplot"),
                colorset = c("lightgray","navy","red2"))

chart.QQPlot(rtsi_1520, distribution = "norm")
library(sn)
n = length(rtsi_1520)
fit.tSN = st.mple(as.matrix(rep(1,n)),rtsi_1520,symmetr = TRUE)
names(fit.tSN$dp) = c("location","scale","dof")
round(fit.tSN$dp,3)
par(mar = c(3,3,3,3))
chart.QQPlot(rtsi_1520, main = "t-distribution Q-Q plot of log-returns", 
             xlab = "quantilesSymmetricTdistEst",line = c("quartiles"), envelope = .95, 
             distribution = 't', distributionParameter = 'df=fit.tSN$dp[3]',pch = 20)
#### Student hist?
library(EnvStats)
qqPlot(as.numeric(rtsi_1520), distribution = "t",
       param.list = list(df = 4),
       add.line = TRUE)
#std_d <- rt(length(rtsi_1520), df = 4)
#qqnorm(std_d) qqline(std_d , col = "red")
#hist(rtsi_1520, nclass = 20, probability = TRUE)
#lines(density(rtsi_1520), col = "blue") #plot(density(std_d))

#### Q-Q plot: Normal or Student?
qqnorm(rtsi_1520)
qqline(rtsi_1520, col = "red")

# Generate n standard normal variables, make a Q-Q plot, add a red line
x1 <- rnorm(length(rtsi_1520))
qqnorm(x1)
qqline(x1, col = "red")

# Generate n Student t variables, make a Q-Q plot, add a red line
std_d <- rt(n, df = 4)
qqnorm(std_d)
qqline(std_d, col = "red")

#### Still cheking - more formally
install.packages('moments', repos='http://cran.rstudio.com', type='source')
library(moments)
skewness(as.numeric(rtsi_1520))
kurtosis(as.numeric(rtsi_1520))
jarque.bera.test(rtsi_1520) #reject the H0 of normality
## если повторить тесты на нормальность для rtsi_1519, получим схожие результаты 
## (заметно лучше только в skewness/kurtosis - все равно далеко от нормального)
#### ЗДЕСЬ И ДАЛЕЕ rtsi_1520 ЕСЛИ НЕ СКАЗАНО ИНАЧЕ

jarque.bera.test(rtsi_1520)
jarque.bera.test(close_ts)

#### Stationarity #### 
#### Are returns stationary?
adf_none = ur.df(close_ts, selectlags = 'BIC', type = 'none') 
summary(adf_none)
adf_const = ur.df(close_ts, selectlags = 'BIC', type = 'drift') 
summary(adf_const)
adf_trend = ur.df(close_ts, selectlags = 'BIC', type = 'trend') 
summary(adf_trend)

kpss_ur <- ur.kpss(rtsi_1520, type = c("mu"), lags = c("long")) #mu = with a drift, tau - linear trend
summary(kpss_ur)
kpss_ur_tau <- ur.kpss(rtsi_1520, type = c("tau"), lags = c("long")) #mu = with a drift, tau - linear trend
summary(kpss_ur_tau)

kpss_ur <- ur.kpss(close_ts["2015-01-06/2020-12-30"], type = c("mu"), lags = c("long")) #mu = with a drift, tau - linear trend
summary(kpss_ur)
kpss_ur_tau <- ur.kpss(close_ts["2015-01-06/2020-12-30"], type = c("tau"), lags = c("long")) #mu = with a drift, tau - linear trend
summary(kpss_ur_tau)

kpss.test(rtsi_1520, null = c("Level"))
kpss.test(rtsi_1519, null = c("Level"))
kpss.test(rtsi_1520, null = c("Trend"))
kpss.test(close_ts["2015-01-06/2020-12-30"], null = c("Level"))
kpss.test(close_ts["2015-01-06/2020-12-30"], null = c("Trend"))
# according to all the test the returns are stationary, the same result is achieved with rtsi_1519

#autocorrelation - Ljung-Box test
Box.test(rtsi_1520,lag = 10, fitdf = 0, type="Lj")
Box.test(sq_ret_1520,lag = 10, fitdf = 0, type="Lj") 

# correlogram 
acf_1520 <- ggAcf(rtsi_1520, 40)
pacf_1520 <- ggPacf(rtsi_1520, 40)
acf_1520
pacf_1520
grid.arrange(acf_1520, pacf_1520, nrow = 2)
#1519 
acf_1519 <- ggAcf(rtsi_1519, 40)
pacf_1519 <- ggPacf(rtsi_1519, 40)
grid.arrange(acf_1519, pacf_1519, nrow = 2)

#### squared and absolute returns
sq_ret_1520 <- rtsi_1520^2
plot(sq_ret_1520)
mean(sq_ret_1520)
acf_sq1520 <- ggAcf(sq_ret_1520, 40)
pacf_sq1520 <- ggPacf(sq_ret_1520, 40)
grid.arrange(acf_sq1520, pacf_sq1520, nrow = 2)

absacf_1520 <- ggAcf(abs(rtsi_1520), 60)
abspacf_1520 <- ggPacf(abs(rtsi_1520), 40)
absacf_1520
abspacf_1520
grid.arrange(absacf_1520, abspacf_1520, nrow = 2)
Box.test(abs(rtsi_1520),lag = 10, fitdf = 0, type="Lj")
#plot(sq_ret_1520)
#(1+mean(sq_ret_1520))^250


#### Part 2.1
#### ARIMA for returns
arima_auto = auto.arima(rtsi_1519, ic = c("bic"))
arima_00 <- Arima(rtsi_1519, order = c(0,0,0))
arima_10 <- Arima(rtsi_1519, order = c(1,0,0))
arima_01 <- Arima(rtsi_1519, order = c(0,0,1))
arima_11 <- Arima(rtsi_1519, order = c(1,0,1))
arima_12 <- Arima(rtsi_1519, order = c(1,0,2))
arima_21 <- Arima(rtsi_1519, order = c(2,0,1))
arima_22 <- Arima(rtsi_1519, order = c(2,0,2))
summary(arima_auto)
#checkresiduals(arima_auto)
#autoplot(arima_auto)

akaike_inf <- matrix(c(arima_00$bic, arima_10$bic, arima_01$bic, arima_11$bic, 
                       arima_12$bic, arima_21$bic, arima_22$bic, arima_auto$bic), ncol=8)
rownames(akaike_inf) <- c("BIC")
colnames(akaike_inf) <- c("00", "10", "01", "11", "12", "21", "22" , "auto")
akaike_inf <- as.table(akaike_inf)
akaike_inf
checkresiduals(arima_auto)
checkresiduals(arima_00)
mean(rtsi_1519)



#### Part 2.2
#### GARCH FAMILY ####
######################
#####   GARCH  #######
######################
# 1. GARCH(1.1) with normal dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH"), distribution.model = "norm")
# Estimate the GARCH(1.1) model with normal dist.
garchfit_11_n <- ugarchfit(data = rtsi_1519, spec = garchspec_11_n)
# GARCH(1.2) with normal dist. 
garchspec_12_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(1, 2)), distribution.model = "norm")
garchfit_12_n <- ugarchfit(data = rtsi_1519, spec = garchspec_12_n)
# Estimate the GARCH(2.1) model with normal dist.
garchspec_21_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(2, 1)), distribution.model = "norm")
garchfit_21_n <- ugarchfit(data = rtsi_1519, spec = garchspec_21_n)
# Estimate the GARCH(2.2) model with normal dist
garchspec_22_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(2, 2)), distribution.model = "norm")
garchfit_22_n <- ugarchfit(data = rtsi_1519, spec = garchspec_22_n)
# IC comparison
infocriteria(garchfit_11_n)
infocriteria(garchfit_12_n)
infocriteria(garchfit_21_n)
infocriteria(garchfit_22_n)
garchfit_11_n
#_________________________________________________________________________
# 2. GARCH(1.1) with Student dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), distribution.model = "std")
# Estimate the GARCH(1.1) model with normal dist.
garchfit_11_std <- ugarchfit(data = rtsi_1519, spec = garchspec_11_std)
# GARCH(1.2) with Student dist. 
garchspec_12_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(1, 2)), distribution.model = "std")
garchfit_12_std <- ugarchfit(data = rtsi_1519, spec = garchspec_12_std)
# Estimate the GARCH(2.1) model with Student dist.
garchspec_21_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(2, 1)), distribution.model = "std")
garchfit_21_std <- ugarchfit(data = rtsi_1519, spec = garchspec_21_std)
# Estimate the GARCH(2.2) model with Student dist
garchspec_22_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(2, 2)), distribution.model = "std")
garchfit_22_std <- ugarchfit(data = rtsi_1519, spec = garchspec_22_std)
# IC comparison
infocriteria(garchfit_11_std)
infocriteria(garchfit_12_std)
infocriteria(garchfit_21_std)
infocriteria(garchfit_22_std)
garchfit_11_std
plot(garchfit_11_std@fit$residuals)
plot(garchfit_11_std, which = 3)
#_________________________________________________________________________
# 3. GARCH(1.1) with skewed Student dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), distribution.model = "sstd")
# Estimate the GARCH(1.1) model with normal dist.
garchfit_11_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_11_sstd)
# GARCH(1.2) with Student dist. 
garchspec_12_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "sGARCH", garchOrder = c(1, 2)), distribution.model = "sstd")
garchfit_12_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_12_sstd)
# Estimate the GARCH(2.1) model with Student dist.
garchspec_21_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "sGARCH", garchOrder = c(2, 1)), distribution.model = "sstd")
garchfit_21_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_21_sstd)
# Estimate the GARCH(2.2) model with Student dist
garchspec_22_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "sGARCH", garchOrder = c(2, 2)), distribution.model = "sstd")
garchfit_22_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_22_sstd)
# IC comparison
infocriteria(garchfit_11_sstd)
infocriteria(garchfit_12_sstd)
infocriteria(garchfit_21_sstd)
infocriteria(garchfit_22_sstd)
#_________________________________________________________________________
######################
#####   IGARCH  ######
######################
# 4. IGARCH(1.1) with normal dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_i_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "iGARCH"), distribution.model = "norm")
# Estimate the IGARCH(1.1) model with normal dist.
garchfit_11_i_n <- ugarchfit(data = rtsi_1519, spec = garchspec_11_i_n)
# GARCH(1.2) with normal dist. 
garchspec_12_i_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "iGARCH", garchOrder = c(1, 2)), distribution.model = "norm")
garchfit_12_i_n <- ugarchfit(data = rtsi_1519, spec = garchspec_12_i_n)
# Estimate the IGARCH(2.1) model with normal dist.
garchspec_21_i_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "iGARCH", garchOrder = c(2, 1)), distribution.model = "norm")
garchfit_21_i_n <- ugarchfit(data = rtsi_1519, spec = garchspec_21_i_n)
# Estimate the IGARCH(2.2) model with normal dist
garchspec_22_i_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "iGARCH", garchOrder = c(2, 2)), distribution.model = "norm")
garchfit_22_i_n <- ugarchfit(data = rtsi_1519, spec = garchspec_22_i_n)
# IC comparison
infocriteria(garchfit_11_i_n)
infocriteria(garchfit_12_i_n)
infocriteria(garchfit_21_i_n)
infocriteria(garchfit_22_i_n)
garchfit_11_i_n
#_________________________________________________________________________
# 5. IGARCH(1.1) with Student dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_i_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), distribution.model = "std")
# Estimate the IGARCH(1.1) model with normal dist.
garchfit_11_i_std <- ugarchfit(data = rtsi_1519, spec = garchspec_11_i_std)
# GARCH(1.2) with Student dist. 
garchspec_12_i_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "iGARCH", garchOrder = c(1, 2)), distribution.model = "std")
garchfit_12_i_std <- ugarchfit(data = rtsi_1519, spec = garchspec_12_i_std)
# Estimate the IGARCH(2.1) model with Student dist.
garchspec_21_i_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "iGARCH", garchOrder = c(2, 1)), distribution.model = "std")
garchfit_21_i_std <- ugarchfit(data = rtsi_1519, spec = garchspec_21_i_std)
# Estimate the IGARCH(2.2) model with Student dist
garchspec_22_i_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "iGARCH", garchOrder = c(2, 2)), distribution.model = "std")
garchfit_22_i_std <- ugarchfit(data = rtsi_1519, spec = garchspec_22_i_std)
# IC comparison
infocriteria(garchfit_11_i_std)
infocriteria(garchfit_12_i_std)
infocriteria(garchfit_21_i_std)
infocriteria(garchfit_22_i_std)
garchfit_11_i_std
plot(garchfit_11_i_std, which = "all")
#_________________________________________________________________________
# 6. IGARCH(1.1) with skewed Student dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_i_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), distribution.model = "sstd")
# Estimate the IGARCH(1.1) model with normal dist.
garchfit_11_i_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_11_i_sstd)
# GARCH(1.2) with Student dist. 
garchspec_12_i_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                variance.model = list(model = "iGARCH", garchOrder = c(1, 2)), distribution.model = "sstd")
garchfit_12_i_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_12_i_sstd)
# Estimate the IGARCH(2.1) model with Student dist.
garchspec_21_i_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                variance.model = list(model = "iGARCH", garchOrder = c(2, 1)), distribution.model = "sstd")
garchfit_21_i_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_21_i_sstd)
# Estimate the IGARCH(2.2) model with Student dist
garchspec_22_i_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                variance.model = list(model = "iGARCH", garchOrder = c(2, 2)), distribution.model = "sstd")
garchfit_22_i_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_22_i_sstd)
# IC comparison
infocriteria(garchfit_11_i_sstd)
infocriteria(garchfit_12_i_sstd)
infocriteria(garchfit_21_i_sstd)
infocriteria(garchfit_22_i_sstd)
#_________________________________________________________________________
######################
#####   FIGARCH  #####
######################
# 7. FIGARCH(1.1) with normal dist. 
# Specify a standard fiGARCH model with constant mean
garchspec_11_fi_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "fiGARCH"), distribution.model = "norm")
# Estimate the fiGARCH(1.1) model with normal dist.
garchfit_11_fi_n <- ugarchfit(data = rtsi_1519, spec = garchspec_11_fi_n)
# GARCH(1.2) with normal dist. 
garchspec_12_fi_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "fiGARCH", garchOrder = c(1, 2)), distribution.model = "norm")
garchfit_12_fi_n <- ugarchfit(data = rtsi_1519, spec = garchspec_12_fi_n)
# Estimate the fiGARCH(2.1) model with normal dist.
garchspec_21_fi_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "fiGARCH", garchOrder = c(2, 1)), distribution.model = "norm")
garchfit_21_fi_n <- ugarchfit(data = rtsi_1519, spec = garchspec_21_fi_n)
# Estimate the fiGARCH(2.2) model with normal dist
garchspec_22_fi_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "fiGARCH", garchOrder = c(2, 2)), distribution.model = "norm")
garchfit_22_fi_n <- ugarchfit(data = rtsi_1519, spec = garchspec_22_fi_n)
# IC comparison
infocriteria(garchfit_11_fi_n)
infocriteria(garchfit_12_fi_n)
infocriteria(garchfit_21_fi_n)
infocriteria(garchfit_22_fi_n)

#_________________________________________________________________________
# 8. FIGARCH(1.1) with Student dist. 
# Specify a standard fiGARCH model with constant mean
garchspec_11_fi_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "fiGARCH", garchOrder = c(1, 1)), distribution.model = "std")
# Estimate the fiGARCH(1.1) model with normal dist.
garchfit_11_fi_std <- ugarchfit(data = rtsi_1519, spec = garchspec_11_fi_std)
# GARCH(1.2) with Student dist. 
garchspec_12_fi_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "fiGARCH", garchOrder = c(1, 2)), distribution.model = "std")
garchfit_12_fi_std <- ugarchfit(data = rtsi_1519, spec = garchspec_12_fi_std)
# Estimate the fiGARCH(2.1) model with Student dist.
garchspec_21_fi_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "fiGARCH", garchOrder = c(2, 1)), distribution.model = "std")
garchfit_21_fi_std <- ugarchfit(data = rtsi_1519, spec = garchspec_21_fi_std)
# Estimate the fiGARCH(2.2) model with Student dist
garchspec_22_fi_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "fiGARCH", garchOrder = c(2, 2)), distribution.model = "std")
garchfit_22_fi_std <- ugarchfit(data = rtsi_1519, spec = garchspec_22_fi_std)
# IC comparison
infocriteria(garchfit_11_fi_std)
infocriteria(garchfit_12_fi_std)
infocriteria(garchfit_21_fi_std)
infocriteria(garchfit_22_fi_std)
garchfit_11_fi_std
#plot(garchfit_11_fi_std, which = "all")
#_________________________________________________________________________
# 9. FIGARCH(1.1) with skewed Student dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_fi_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "fiGARCH", garchOrder = c(1, 1)), distribution.model = "sstd")
# Estimate the fiGARCH(1.1) model with normal dist.
garchfit_11_fi_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_11_fi_sstd)
# GARCH(1.2) with Student dist. 
garchspec_12_fi_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "fiGARCH", garchOrder = c(1, 2)), distribution.model = "sstd")
garchfit_12_fi_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_12_fi_sstd)
# Estimate the fiGARCH(2.1) model with Student dist.
garchspec_21_fi_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "fiGARCH", garchOrder = c(2, 1)), distribution.model = "sstd")
garchfit_21_fi_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_21_fi_sstd)
# Estimate the fiGARCH(2.2) model with Student dist
garchspec_22_fi_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "fiGARCH", garchOrder = c(2, 2)), distribution.model = "sstd")
garchfit_22_fi_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_22_fi_sstd)
# IC comparison
infocriteria(garchfit_11_fi_sstd)
infocriteria(garchfit_12_fi_sstd)
infocriteria(garchfit_21_fi_sstd)
infocriteria(garchfit_22_fi_sstd)
#_________________________________________________________________________
######################
#####   EGARCH  ######
######################
# 10. EGARCH(1.1) with normal dist. 
# Specify a standard eGARCH model with constant mean
garchspec_11_e_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), distribution.model = "norm")
# Estimate the eGARCH(1.1) model with normal dist.
garchfit_11_e_n <- ugarchfit(data = rtsi_1519, spec = garchspec_11_e_n)
# eGARCH(1.2) with normal dist. 
garchspec_12_e_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "eGARCH", garchOrder = c(1, 2)), distribution.model = "norm")
garchfit_12_e_n <- ugarchfit(data = rtsi_1519, spec = garchspec_12_e_n)
# Estimate the eGARCH(2.1) model with normal dist.
garchspec_21_e_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "eGARCH", garchOrder = c(2, 1)), distribution.model = "norm")
garchfit_21_e_n <- ugarchfit(data = rtsi_1519, spec = garchspec_21_e_n)
# Estimate the eGARCH(2.2) model with normal dist
garchspec_22_e_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "eGARCH", garchOrder = c(2, 2)), distribution.model = "norm")
garchfit_22_e_n <- ugarchfit(data = rtsi_1519, spec = garchspec_22_e_n)
# IC comparison
infocriteria(garchfit_11_e_n)
infocriteria(garchfit_12_e_n)
infocriteria(garchfit_21_e_n)
infocriteria(garchfit_22_e_n)

#_________________________________________________________________________
# 11. EGARCH(1.1) with Student dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_e_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), distribution.model = "std")
# Estimate the EGARCH(1.1) model with normal dist.
garchfit_11_e_std <- ugarchfit(data = rtsi_1519, spec = garchspec_11_e_std)
# EGARCH(1.2) with Student dist. 
garchspec_12_e_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "eGARCH", garchOrder = c(1, 2)), distribution.model = "std")
garchfit_12_e_std <- ugarchfit(data = rtsi_1519, spec = garchspec_12_e_std)
# Estimate the EGARCH(2.1) model with Student dist.
garchspec_21_e_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "eGARCH", garchOrder = c(2, 1)), distribution.model = "std")
garchfit_21_e_std <- ugarchfit(data = rtsi_1519, spec = garchspec_21_e_std)
# Estimate the EGARCH(2.2) model with Student dist
garchspec_22_e_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "eGARCH", garchOrder = c(2, 2)), distribution.model = "std")
garchfit_22_e_std <- ugarchfit(data = rtsi_1519, spec = garchspec_22_e_std)
# IC comparison
infocriteria(garchfit_11_e_std)
infocriteria(garchfit_12_e_std)
infocriteria(garchfit_21_e_std)
infocriteria(garchfit_22_e_std)
garchfit_11_e_std@fit$persistence
#_________________________________________________________________________
# 12. EGARCH(1.1) with skewed Student dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_e_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), distribution.model = "sstd")
# Estimate the EGARCH(1.1) model with normal dist.
garchfit_11_e_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_11_e_sstd)
# EGARCH(1.2) with Student dist. 
garchspec_12_e_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "eGARCH", garchOrder = c(1, 2)), distribution.model = "sstd")
garchfit_12_e_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_12_e_sstd)
# Estimate the EGARCH(2.1) model with Student dist.
garchspec_21_e_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "eGARCH", garchOrder = c(2, 1)), distribution.model = "sstd")
garchfit_21_e_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_21_e_sstd)
# Estimate the EGARCH(2.2) model with Student dist
garchspec_22_e_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "eGARCH", garchOrder = c(2, 2)), distribution.model = "sstd")
garchfit_22_e_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_22_e_sstd)
# IC comparison
infocriteria(garchfit_11_e_sstd)
infocriteria(garchfit_12_e_sstd)
infocriteria(garchfit_21_e_sstd)
infocriteria(garchfit_22_e_sstd)
#_________________________________________________________________________
######################
#####  GJR-GARCH  ####
######################
# 13. GJR-GARCH(1.1) with normal dist. 
# Specify a standard eGARCH model with constant mean
garchspec_11_gjr_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), distribution.model = "norm")
# Estimate the GJR-GARCH(1.1) model with normal dist.
garchfit_11_gjr_n <- ugarchfit(data = rtsi_1519, spec = garchspec_11_gjr_n)
# GJR-GARCH(1.2) with normal dist. 
garchspec_12_gjr_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "gjrGARCH", garchOrder = c(1, 2)), distribution.model = "norm")
garchfit_12_gjr_n <- ugarchfit(data = rtsi_1519, spec = garchspec_12_gjr_n)
# Estimate the GJR-GARCH(2.1) model with normal dist.
garchspec_21_gjr_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "gjrGARCH", garchOrder = c(2, 1)), distribution.model = "norm")
garchfit_21_gjr_n <- ugarchfit(data = rtsi_1519, spec = garchspec_21_gjr_n)
# Estimate the GJR-GARCH(2.2) model with normal dist
garchspec_22_gjr_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "gjrGARCH", garchOrder = c(2, 2)), distribution.model = "norm")
garchfit_22_gjr_n <- ugarchfit(data = rtsi_1519, spec = garchspec_22_gjr_n)
# IC comparison
infocriteria(garchfit_11_gjr_n)
infocriteria(garchfit_12_gjr_n)
infocriteria(garchfit_21_gjr_n)
infocriteria(garchfit_22_gjr_n)
#_________________________________________________________________________
# 14. GJR-GARCH(1.1) with Student dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_gjr_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), distribution.model = "std")
# Estimate the GJR-GARCH(1.1) model with normal dist.
garchfit_11_gjr_std <- ugarchfit(data = rtsi_1519, spec = garchspec_11_gjr_std)
# GJR-GARCH(1.2) with Student dist. 
garchspec_12_gjr_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "gjrGARCH", garchOrder = c(1, 2)), distribution.model = "std")
garchfit_12_gjr_std <- ugarchfit(data = rtsi_1519, spec = garchspec_12_gjr_std)
# Estimate the GJR-GARCH(2.1) model with Student dist.
garchspec_21_gjr_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "gjrGARCH", garchOrder = c(2, 1)), distribution.model = "std")
garchfit_21_gjr_std <- ugarchfit(data = rtsi_1519, spec = garchspec_21_gjr_std)
# Estimate the GJR-GARCH(2.2) model with Student dist
garchspec_22_gjr_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "gjrGARCH", garchOrder = c(2, 2)), distribution.model = "std")
garchfit_22_gjr_std <- ugarchfit(data = rtsi_1519, spec = garchspec_22_gjr_std)
# IC comparison
infocriteria(garchfit_11_gjr_std)
infocriteria(garchfit_12_gjr_std)
infocriteria(garchfit_21_gjr_std)
infocriteria(garchfit_22_gjr_std)
garchfit_11_gjr_std
plot(garchfit_11_gjr_std, which = "all")
#news impact curve comparison
plot(garchfit_11_gjr_std, which = 12)
plot(garchfit_11_std, which = 12)
plot(garchfit_11_e_std, which = 12)
plot(garchfit_11_e_std, which = "all")

#_________________________________________________________________________
# 15. GJR-GARCH(1.1) with skewed Student dist. 
# Specify a standard GJR-GARCH model with constant mean
garchspec_11_gjr_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), distribution.model = "sstd")
# Estimate the GJR-GARCH(1.1) model with normal dist.
garchfit_11_gjr_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_11_gjr_sstd)
# GJR-GARCH(1.2) with Student dist. 
garchspec_12_gjr_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 2)), distribution.model = "sstd")
garchfit_12_gjr_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_12_gjr_sstd)
# Estimate the GJR-GARCH(2.1) model with Student dist.
garchspec_21_gjr_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "gjrGARCH", garchOrder = c(2, 1)), distribution.model = "sstd")
garchfit_21_gjr_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_21_gjr_sstd)
# Estimate the GJR-GARCH(2.2) model with Student dist
garchspec_22_gjr_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "gjrGARCH", garchOrder = c(2, 2)), distribution.model = "sstd")
garchfit_22_gjr_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_22_gjr_sstd)
# IC comparison
infocriteria(garchfit_11_gjr_sstd)
infocriteria(garchfit_12_gjr_sstd)
infocriteria(garchfit_21_gjr_sstd)
infocriteria(garchfit_22_gjr_sstd)
0.031404+0.946317+0.030206/2
jarque.bera.test(garchfit_11_gjr_std@fit$residuals)
#_________________________________________________________________________
######################
#####   APARCH   #####
######################
# 16. APARCH(1.1) with normal dist. 
# Specify a standard APARCH model with constant mean
garchspec_11_ap_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "apARCH", garchOrder = c(1, 1)), distribution.model = "norm")
# Estimate the APARCH(1.1) model with normal dist.
garchfit_11_ap_n <- ugarchfit(data = rtsi_1519, spec = garchspec_11_ap_n)
# APARCH(1.2) with normal dist. 
garchspec_12_ap_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "apARCH", garchOrder = c(1, 2)), distribution.model = "norm")
garchfit_12_ap_n <- ugarchfit(data = rtsi_1519, spec = garchspec_12_ap_n)
# Estimate the APARCH(2.1) model with normal dist.
garchspec_21_ap_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "apARCH", garchOrder = c(2, 1)), distribution.model = "norm")
garchfit_21_ap_n <- ugarchfit(data = rtsi_1519, spec = garchspec_21_ap_n)
# Estimate the APARCH(2.2) model with normal dist
garchspec_22_ap_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "apARCH", garchOrder = c(2, 2)), distribution.model = "norm")
garchfit_22_ap_n <- ugarchfit(data = rtsi_1519, spec = garchspec_22_ap_n)
# IC comparison
infocriteria(garchfit_11_ap_n)
infocriteria(garchfit_12_ap_n)
infocriteria(garchfit_21_ap_n)
infocriteria(garchfit_22_ap_n)
#_________________________________________________________________________
# 17. APARCH(1.1) with Student dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_ap_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "apARCH", garchOrder = c(1, 1)), distribution.model = "std")
# Estimate the APARCH(1.1) model with normal dist.
garchfit_11_ap_std <- ugarchfit(data = rtsi_1519, spec = garchspec_11_ap_std)
# APARCH(1.2) with Student dist. 
garchspec_12_ap_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "apARCH", garchOrder = c(1, 2)), distribution.model = "std")
garchfit_12_ap_std <- ugarchfit(data = rtsi_1519, spec = garchspec_12_ap_std)
# Estimate the APARCH(2.1) model with Student dist.
garchspec_21_ap_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "apARCH", garchOrder = c(2, 1)), distribution.model = "std")
garchfit_21_ap_std <- ugarchfit(data = rtsi_1519, spec = garchspec_21_ap_std)
# Estimate the APARCH(2.2) model with Student dist
garchspec_22_ap_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "apARCH", garchOrder = c(2, 2)), distribution.model = "std")
garchfit_22_ap_std <- ugarchfit(data = rtsi_1519, spec = garchspec_22_ap_std)
# IC comparison
infocriteria(garchfit_11_ap_std)
infocriteria(garchfit_12_ap_std)
infocriteria(garchfit_21_ap_std)
infocriteria(garchfit_22_ap_std)
garchfit_11_ap_std
plot(garchfit_11_ap_std, which = "all")
#_________________________________________________________________________
# 18. APARCH(1.1) with skewed Student dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_ap_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "apARCH", garchOrder = c(1, 1)), distribution.model = "sstd")
# Estimate the APARCH(1.1) model with normal dist.
garchfit_11_ap_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_11_ap_sstd)
# GARCH(1.2) with Student dist. 
garchspec_12_ap_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "apARCH", garchOrder = c(1, 2)), distribution.model = "sstd")
garchfit_12_ap_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_12_ap_sstd)
# Estimate the APARCH(2.1) model with Student dist.
garchspec_21_ap_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "apARCH", garchOrder = c(2, 1)), distribution.model = "sstd")
garchfit_21_ap_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_21_ap_sstd)
# Estimate the APARCH(2.2) model with Student dist
garchspec_22_ap_sstd <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                  variance.model = list(model = "apARCH", garchOrder = c(2, 2)), distribution.model = "sstd")
garchfit_22_ap_sstd <- ugarchfit(data = rtsi_1519, spec = garchspec_22_ap_sstd)
# IC comparison
infocriteria(garchfit_11_ap_sstd)
infocriteria(garchfit_12_ap_sstd)
infocriteria(garchfit_21_ap_sstd)
infocriteria(garchfit_22_ap_sstd)
###############################   OPTIMAL MODELS  #################################
#1. GARCH(1,1) - std
garchfit_11_std
#2. IGARCH(1,1) - std
garchfit_11_i_std
#3. GJR-GARCH(1,1) - std
garchfit_11_gjr_std
#4. EGARCH(1,1) - std
garchfit_11_e_std
#5. APARCH(1,1) - std
garchfit_11_ap_std

par(mfrow = c(2,2))
plot(garchfit_11_std, which = 3)
plot(garchfit_11_gjr_std, which = 12)
plot(garchfit_11_e_std, which = 12)
plot(garchfit_11_ap_std, which = 12)
par(mfrow = c(1,1))

plot(rtsi_2020^2)
lines(proxy_daily, col = "red")



###############################   RANDOM FOREST  #################################
library(forecastML)
library(DT)
install.packages('glmnet', repos ='http://cran.rstudio.com', type='source')
library(glmnet) 
install.packages('randomForest', repos ='http://cran.rstudio.com', type='source')
library(randomForest) 
install.packages('caret', repos ='http://cran.rstudio.com', type='source')
library(caret)

data_ML <- data.frame(read_xlsx("/Users/dariakarpova/Desktop/Close_ts_1520.xlsx", sheet = 1, range = NULL), fix.empty.names = TRUE)
data_ML
colnames(data_ML)<- cbind("Date", "Close", "Log", "LogRet", "Sigma", "Sigma_t1", "ret_t1", "sign")
head(data_ML)
#### Convert Date to dates format #### 
dates <- (data_ML[,1])
dates_as_date <- ymd(dates)
class(dates_as_date)
#### Convert dates to a time series indices and "Close" to time series format
sigma_ml <- data_ML[,5]
sigma_ml <- xts(sigma_ml, order.by = dates_as_date)
names(sigma_ml) <- c("Sigma_ml")
head(sigma_ml)
#### Create train, validation and test sets
#set.seed(1)
#sample_sigma <- sample(1:3, size = nrow(data_ML), prob = c(0.7,0.15,0.15), replace = TRUE)

# Create a train, validation and tests from the original data frame 
train_sample <- data_ML[1:1260,5:8]
#valid_sample <- data_ML[1009:1260,5:8]
test_sample <-  data_ML[1261:1510,5:8]
sample_short <- data_ML[1382:1510,5:8]
#regressors only
train_sample_regr <- train_sample[,2:4]


# Train the tree
grade_model <- rpart(formula = Sigma ~ ., 
                     data = train_sample , 
                     method = "anova")
rpart.control(maxdepth = 30)
# Look at the model output                      
print(grade_model)
# Plot the tree model
rpart.plot(x = grade_model)
##########################
########  Forest  ########
##########################
set.seed(123)  # for reproducibility
forest_train <- randomForest(formula = train_sample$Sigma ~ ., 
                             data = train_sample, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
# Print the model output                             
print(forest_train)
plot(forest_train,main = "Error - number of trees", col="navy")
importance(forest_train)


#### tuning mtry
mtry_autofit <- tuneRF(x = train_sample_regr, y = train_sample$Sigma, ntreeTry = 200, doBest = TRUE)
print(mtry_autofit)
mtry_opt <- mtry_autofit[,"mtry"][which.min(mtry_autofit[,"OOBError"])]
print(mtry_opt)

#### Generate predicted classes using the model object
rf_prediction <- predict(object = forest_train,   # model object 
                            newdata = test_sample)  # test dataset
                            #type = "anova") # return classification labels
rf_predict <- xts(rf_prediction, order.by = index(proxy_daily))
plot(sqrt(proxy_daily), col = "grey")
lines(rf_predict, col = "red")

mape(sqrt(proxy_daily), rf_predict)

#tree_check
set.seed(1234)  # for reproducibility
train_check <- data_ML[1:500,5:8]
forest_check <- randomForest(formula = train_check$Sigma ~ ., 
                             data = train_check, ntree = 100, mtry = 1, importance=TRUE)
# Print the model output                             
print(forest_check)
plot(forest_check)

rf_pred_check <- predict(object = forest_check, newdata = test_sample)
rf_predict_check <- xts(rf_pred_check, order.by = index(proxy_daily))
plot(sqrt(proxy_daily), col = "grey")
lines(rf_predict_check, col = "red")

mse(sqrt(proxy_daily), rf_predict)
mape(sqrt(proxy_daily), rf_predict)

plot(rtsi_1520^2)

#### change forecast horizon
rf_prediction_hor <- predict(object = forest_train, newdata = sample_short)  
rf_predict_hor <- xts(rf_prediction_hor, order.by = index(proxy_daily["2020-07-02/2020-12-30"]))
plot(sqrt(proxy_daily["2020-07-02/2020-12-30"]), col = "grey")
lines(rf_predict_hor, col = "red")

#### mtry = 2 
forest_train_2 <- randomForest(formula = train_sample$Sigma ~ ., 
                             data = train_sample, ntree = 100, mtry = 2, importance=TRUE)
#### Generate predicted classes using the model object
rf_prediction_2 <- predict(object = forest_train_2,   # model object 
                         newdata = test_sample)  # test dataset
#type = "anova") # return classification labels
rf_predict_2 <- xts(rf_prediction_2, order.by = index(proxy_daily))
plot(sqrt(proxy_daily), col = "grey")
lines(rf_predict_2, col = "red")


## лес переобучающийся
train_long <- data_ML[1:1382,5:8]
sample_short <- data_ML[1382:1510,5:8]
set.seed(123)  # for reproducibility

forest_reest <- randomForest(formula = train_long$Sigma ~ ., 
                             data = train_long, ntree = 500, mtry = 1, nodesize = 5, importance = TRUE)
# Print the model output                             
print(forest_reest)
plot(forest_reest ,main = "Error - number of trees", col="navy")
importance(forest_reest )
#predict
rf_prediction_reest <- predict(object = forest_reest, newdata = sample_short)  # test dataset
#type = "anova") # return classification labels
rf_predict_reest <- xts(rf_prediction_reest, order.by = index(proxy_daily["2020-07-02/2020-12-30"]))
plot(sqrt(proxy_daily), col = "grey")
lines(rf_predict_reest, col = "red")

mape(sqrt(proxy_daily), rf_predict_reest)

#делаем частичный постянный реэстимейшн
train_1 <- data_ML[1:1260,5:8]
  sample_1 <- data_ML[1261:1280,5:8]
  
train_2 <- data_ML[21:1280,5:8]
  sample_2 <- data_ML[1281:1300,5:8]
  
train_3 <- data_ML[41:1300,5:8]
  sample_3 <- data_ML[1301:1320,5:8]
  
train_4 <- data_ML[61:1320,5:8]
  sample_4 <- data_ML[1321:1340,5:8]
  
train_5 <- data_ML[81:1340,5:8]
  sample_5 <- data_ML[1341:1360,5:8]
  
train_6 <- data_ML[101:1360,5:8]
  sample_6 <- data_ML[1361:1380,5:8]
  
train_7 <- data_ML[121:1380,5:8]
  sample_7 <- data_ML[1381:1400,5:8]
  
train_8 <- data_ML[141:1400,5:8]
  sample_8 <- data_ML[1401:1420,5:8]
  
train_9 <- data_ML[161:1420,5:8]
  sample_9 <- data_ML[1421:1440,5:8]
  
train_10 <- data_ML[181:1440,5:8]
  sample_10 <- data_ML[1441:1460,5:8]

train_11 <- data_ML[201:1460,5:8]
  sample_11 <- data_ML[1461:1480,5:8]
  
train_12 <- data_ML[221:1480,5:8]
  sample_12 <- data_ML[1481:1510,5:8]  


set.seed(123)  
forest_1 <- randomForest(formula = train_1$Sigma ~ ., 
                         data = train_1, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_1 <- predict(object = forest_1, newdata = sample_1)

#type = "anova") # return classification labels
predict_1 <- xts(pred_1, order.by = index(proxy_daily["2020-01-01/2020-01-31"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_1, col = "red")

#### forest 2 ####
forest_2 <- randomForest(formula = train_2$Sigma ~ ., 
                         data = train_2, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_2 <- predict(object = forest_2, newdata = sample_2)

#type = "anova") # return classification labels
predict_2 <- xts(pred_2, order.by = index(proxy_daily["2020-02-03/2020-03-02"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_2, col = "red")

#### forest 3 ####
forest_3 <- randomForest(formula = train_3$Sigma ~ ., 
                         data = train_3, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_3 <- predict(object = forest_3, newdata = sample_3)

#type = "anova") # return classification labels
predict_3 <- xts(pred_3, order.by = index(proxy_daily["2020-03-03/2020-03-31"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_3, col = "red")

#### forest 4 ####
forest_4 <- randomForest(formula = train_4$Sigma ~ ., 
                         data = train_4, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_4 <- predict(object = forest_4, newdata = sample_4)

#type = "anova") # return classification labels
predict_4 <- xts(pred_4, order.by = index(proxy_daily["2020-04-01/2020-04-28"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_4, col = "red")

#### forest 5 ####
forest_5 <- randomForest(formula = train_5$Sigma ~ ., 
                         data = train_5, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_5 <- predict(object = forest_5, newdata = sample_5)

#type = "anova") # return classification labels
predict_5 <- xts(pred_5, order.by = index(proxy_daily["2020-04-29/2020-05-28"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_5, col = "red")

#### forest 6 ####
forest_6 <- randomForest(formula = train_6$Sigma ~ ., 
                         data = train_6, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_6 <- predict(object = forest_6, newdata = sample_6)

#type = "anova") # return classification labels
predict_6 <- xts(pred_6, order.by = index(proxy_daily["2020-05-29/2020-06-29"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_6, col = "red")

#### forest 7 ####
forest_7 <- randomForest(formula = train_7$Sigma ~ ., 
                         data = train_7, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_7 <- predict(object = forest_7, newdata = sample_7)

#type = "anova") # return classification labels
predict_7 <- xts(pred_7, order.by = index(proxy_daily["2020-06-30/2020-07-28"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_7, col = "red")

#### forest 8 ####
forest_8 <- randomForest(formula = train_8$Sigma ~ ., 
                         data = train_8, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_8 <- predict(object = forest_8, newdata = sample_8)

#type = "anova") # return classification labels
predict_8 <- xts(pred_8, order.by = index(proxy_daily["2020-07-29/2020-08-25"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_8, col = "red")

#### forest 9 ####
forest_9 <- randomForest(formula = train_9$Sigma ~ ., 
                         data = train_9, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_9 <- predict(object = forest_9, newdata = sample_9)

#type = "anova") # return classification labels
predict_9 <- xts(pred_9, order.by = index(proxy_daily["2020-08-26/2020-09-22"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_9, col = "red")

#### forest 10 ####
forest_10 <- randomForest(formula = train_10$Sigma ~ ., 
                         data = train_10, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_10 <- predict(object = forest_10, newdata = sample_10)

#type = "anova") # return classification labels
predict_10 <- xts(pred_10, order.by = index(proxy_daily["2020-09-23/2020-10-20"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_10, col = "red")

#### forest 11 ####
forest_11 <- randomForest(formula = train_11$Sigma ~ ., 
                          data = train_11, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_11 <- predict(object = forest_11, newdata = sample_11)

#type = "anova") # return classification labels
predict_11 <- xts(pred_11, order.by = index(proxy_daily["2020-10-21/2020-11-18"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_11, col = "red")

#### forest 12 ####
forest_12 <- randomForest(formula = train_12$Sigma ~ ., 
                          data = train_12, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_12 <- predict(object = forest_12, newdata = sample_12)

#type = "anova") # return classification labels
predict_12 <- xts(pred_12, order.by = index(proxy_daily["2020-11-19/2020-12-30"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_12, col = "red")

rf_pred_fs <- c(predict_1, predict_2, predict_3, predict_4, predict_5, predict_6,
                predict_7, predict_8, predict_9, predict_10, predict_11, predict_12)
mse(sqrt(proxy_daily), rf_pred_fs)
mae(sqrt(proxy_daily),rf_pred_fs)
mape(sqrt(proxy_daily), rf_pred_fs)
smape(sqrt(proxy_daily), rf_pred_fs)

plot(sqrt(proxy_daily), col = "grey")
lines(predict_12, col = "red")
lines(predict_11, col = "red")
lines(predict_10, col = "red")
lines(predict_9, col = "red")
lines(predict_8, col = "red")
lines(predict_7, col = "red")
lines(predict_6, col = "red")
lines(predict_5, col = "red")
lines(predict_4, col = "red")
lines(predict_3, col = "red")
lines(predict_2, col = "red")
lines(predict_1, col = "red")
lines(sigma_pred_std, col = "green")


#set.seed(123)  
#forest_1 <- randomForest(formula = train_1$Sigma ~ ., 
#                         data = train_1, ntree = 500, mtry = 2, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
#pred_1 <- predict(object = forest_1, newdata = sample_1)

#type = "anova") # return classification labels
#predict_1 <- xts(pred_1, order.by = index(proxy_daily["2020-01-01/2020-01-31"]))
#plot(sqrt(proxy_daily), col = "grey")
#lines(predict_1, col = "red")

#### forest 2 ####
#forest_2 <- randomForest(formula = train_2$Sigma ~ ., 
#                         data = train_2, ntree = 500, mtry = 2, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
#pred_2 <- predict(object = forest_2, newdata = sample_2)

#type = "anova") # return classification labels
#predict_2 <- xts(pred_2, order.by = index(proxy_daily["2020-02-03/2020-03-02"]))
#plot(sqrt(proxy_daily), col = "grey")
#lines(predict_2, col = "red")

#### forest 3 ####
#forest_3 <- randomForest(formula = train_3$Sigma ~ ., 
 #                        data = train_3, ntree = 500, mtry = 2, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
#pred_3 <- predict(object = forest_3, newdata = sample_3)

#type = "anova") # return classification labels
#predict_3 <- xts(pred_3, order.by = index(proxy_daily["2020-03-03/2020-03-31"]))
#plot(sqrt(proxy_daily), col = "grey")
#lines(predict_3, col = "red")

#### forest 4 ####
#forest_4 <- randomForest(formula = train_4$Sigma ~ ., 
 #                        data = train_4, ntree = 500, mtry = 2, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
#pred_4 <- predict(object = forest_4, newdata = sample_4)

#type = "anova") # return classification labels
#predict_4 <- xts(pred_4, order.by = index(proxy_daily["2020-04-01/2020-04-28"]))
#plot(sqrt(proxy_daily), col = "grey")
#lines(predict_4, col = "red")

#### forest 5 ####
#forest_5 <- randomForest(formula = train_5$Sigma ~ ., 
#                         data = train_5, ntree = 500, mtry = 2, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
#pred_5 <- predict(object = forest_5, newdata = sample_5)

#type = "anova") # return classification labels
#predict_5 <- xts(pred_5, order.by = index(proxy_daily["2020-04-29/2020-05-28"]))
#plot(sqrt(proxy_daily), col = "grey")
#lines(predict_5, col = "red")

#### forest 6 ####
#forest_6 <- randomForest(formula = train_6$Sigma ~ ., 
 #                        data = train_6, ntree = 500, mtry = 2, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
#pred_6 <- predict(object = forest_6, newdata = sample_6)

#type = "anova") # return classification labels
#predict_6 <- xts(pred_6, order.by = index(proxy_daily["2020-05-29/2020-06-29"]))
#plot(sqrt(proxy_daily), col = "grey")
#lines(predict_6, col = "red")

#### forest 7 ####
#forest_7 <- randomForest(formula = train_7$Sigma ~ ., 
#                         data = train_7, ntree = 500, mtry = 2, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
#pred_7 <- predict(object = forest_7, newdata = sample_7)

#type = "anova") # return classification labels
#predict_7 <- xts(pred_7, order.by = index(proxy_daily["2020-06-30/2020-07-28"]))
#plot(sqrt(proxy_daily), col = "grey")
#lines(predict_7, col = "red")

#### forest 8 ####
#forest_8 <- randomForest(formula = train_8$Sigma ~ ., 
 #                        data = train_8, ntree = 500, mtry = 2, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
#pred_8 <- predict(object = forest_8, newdata = sample_8)

#type = "anova") # return classification labels
#predict_8 <- xts(pred_8, order.by = index(proxy_daily["2020-07-29/2020-08-25"]))
#plot(sqrt(proxy_daily), col = "grey")
#lines(predict_8, col = "red")

#### forest 9 ####
#forest_9 <- randomForest(formula = train_9$Sigma ~ ., 
#                         data = train_9, ntree = 500, mtry = 2, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
#pred_9 <- predict(object = forest_9, newdata = sample_9)

#type = "anova") # return classification labels
#predict_9 <- xts(pred_9, order.by = index(proxy_daily["2020-08-26/2020-09-22"]))
#plot(sqrt(proxy_daily), col = "grey")
#lines(predict_9, col = "red")

#### forest 10 ####
#forest_10 <- randomForest(formula = train_10$Sigma ~ ., 
#                          data = train_10, ntree = 500, mtry = 2, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
#pred_10 <- predict(object = forest_10, newdata = sample_10)

#type = "anova") # return classification labels
#predict_10 <- xts(pred_10, order.by = index(proxy_daily["2020-09-23/2020-10-20"]))
#plot(sqrt(proxy_daily), col = "grey")
#lines(predict_10, col = "red")

#### forest 11 ####
#forest_11 <- randomForest(formula = train_11$Sigma ~ ., 
#                          data = train_11, ntree = 500, mtry = 2, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
#pred_11 <- predict(object = forest_11, newdata = sample_11)

#type = "anova") # return classification labels
#predict_11 <- xts(pred_11, order.by = index(proxy_daily["2020-10-21/2020-11-18"]))
#plot(sqrt(proxy_daily), col = "grey")
#lines(predict_11, col = "red")

#### forest 12 ####
#forest_12 <- randomForest(formula = train_12$Sigma ~ ., 
#                          data = train_12, ntree = 500, mtry = 2, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
#pred_12 <- predict(object = forest_12, newdata = sample_12)

#type = "anova") # return classification labels
#predict_12 <- xts(pred_12, order.by = index(proxy_daily["2020-11-19/2020-12-30"]))
#plot(sqrt(proxy_daily), col = "grey")
#lines(predict_12, col = "red")

#квадратный лес (spoiler alert: он не работает...)
sigma_sq <- data_ML$Sigma^2
sigma_sq1 <- data_ML$Sigma_t1^2
ret_sq1 <- data_ML$ret_t1^2
sign_sq <- data_ML$sign
new_data_ML <- cbind(sigma_sq, sigma_sq1, ret_sq1, sign_sq)
View(new_data_ML)
train_sq <- new_data_ML[1:1260,]
#valid_sample <- data_ML[1009:1260,5:8]
test_sq <-  new_data_ML[1261:1510,]
  
forest_sq <- randomForest(formula = sigma_sq ~ ., 
                               data = train_sq, ntree = 200, mtry = 1, importance=TRUE)
#### Generate predicted classes using the model object
rf_prediction_sq <- predict(object = forest_sq,   # model object 
                           newdata = test_sq)  # test dataset
#type = "anova") # return classification labels
rf_predict_sq <- xts(rf_prediction_sq, order.by = index(proxy_daily))
plot((proxy_daily), col = "grey")
lines(rf_predict_sq, col = "red")
mape(proxy_daily, rf_predict_sq)


################################### FORECASTS ####################################
# Use the method sigma to retrieve the estimated volatilities 
garchvol <- sigma(garchfit_11_std)

#### Forecasts ####
# rolling forecast for GARCH(1,1) with normal dist on 2019 
fore_fit_s <- ugarchfit(garchspec_11_std, data = rtsi_1520, out.sample = 250)
fore_roll_s <- ugarchforecast(fore_fit_s, data = NULL, n.ahead = 1, n.roll = 250)
#plot(fore_roll_s)
sigma_11_std <- sigma(fore_roll_s)

sigma_pred_std <- xts(sigma(forecast_rolling_std)[-1], order.by = index(proxy_daily))
plot(sqrt(proxy_daily), col = "grey")
lines(sigma_pred_std, col = "navy")
lines(rf_pred_fs, col = "deeppink4")

mape(sqrt(proxy_daily), sigma_pred_std)

##############################   LOSS FUNCTIONS   ################################
#1. GARCH(1,1) - std
#garchfit_11_std
forefit_11_std <- ugarchfit(garchspec_11_std, data = rtsi_1520, out.sample = 250)
forecast_11_std <- ugarchforecast(forefit_11_std, data = NULL, n.ahead = 1, n.roll = 250)
sigma_11_std <- xts(sigma(forecast_11_std)[-1], order.by = index(proxy_daily))
#plot(sqrt(proxy_daily), col = "grey")
#lines(sigma_11_std , col = "deeppink4")
mse(sqrt(proxy_daily), sigma_11_std)
mae(sqrt(proxy_daily), sigma_11_std)
mape(sqrt(proxy_daily), sigma_11_std)
smape(sqrt(proxy_daily), sigma_11_std)

#2. IGARCH(1,1) - std
#garchfit_11_i_std
forefit_11_i_std <- ugarchfit(garchspec_11_i_std, data = rtsi_1520, out.sample = 250)
forecast_11_i_std <- ugarchforecast(forefit_11_i_std, data = NULL, n.ahead = 1, n.roll = 250)
sigma_11_i_std <- xts(sigma(forecast_11_i_std)[-1], order.by = index(proxy_daily))
#plot(sqrt(proxy_daily), col = "grey")
#lines(sigma_11_std , col = "deeppink4")
#lines(sigma_11_i_std , col = "navy")
mse(sqrt(proxy_daily), sigma_11_i_std)
mae(sqrt(proxy_daily), sigma_11_i_std)
mape(sqrt(proxy_daily), sigma_11_i_std)
smape(sqrt(proxy_daily), sigma_11_i_std)

#3. GJR-GARCH(1,1) - std
#garchfit_11_gjr_std
forefit_11_gjr_std <- ugarchfit(garchspec_11_gjr_std, data = rtsi_1520, out.sample = 250)
forecast_11_gjr_std <- ugarchforecast(forefit_11_gjr_std, data = NULL, n.ahead = 1, n.roll = 250)
sigma_11_gjr_std <- xts(sigma(forecast_11_gjr_std)[-1], order.by = index(proxy_daily))
plot(sqrt(proxy_daily), col = "grey")
lines(sigma_11_std , col = "deeppink4")
lines(sigma_11_i_std , col = "navy")
lines(sigma_11_gjr_std , col = "green")
mse(sqrt(proxy_daily), sigma_11_gjr_std)
mae(sqrt(proxy_daily), sigma_11_gjr_std)
mape(sqrt(proxy_daily), sigma_11_gjr_std)
smape(sqrt(proxy_daily), sigma_11_gjr_std)

#4. EGARCH(1,1) - std
#garchfit_11_e_std
forefit_11_e_std <- ugarchfit(garchspec_11_e_std, data = rtsi_1520, out.sample = 250)
forecast_11_e_std <- ugarchforecast(forefit_11_e_std, data = NULL, n.ahead = 1, n.roll = 250)
sigma_11_e_std <- xts(sigma(forecast_11_e_std)[-1], order.by = index(proxy_daily))
plot(sqrt(proxy_daily), col = "grey")
lines(sigma_11_std , col = "deeppink4")
lines(sigma_11_i_std , col = "navy")
lines(sigma_11_gjr_std , col = "green")
lines(sigma_11_e_std , col = "orange")
mse(sqrt(proxy_daily), sigma_11_e_std)
mae(sqrt(proxy_daily), sigma_11_e_std)
mape(sqrt(proxy_daily), sigma_11_e_std)
smape(sqrt(proxy_daily), sigma_11_e_std)

#5. APARCH(1,1) - std
#garchfit_11_ap_std
forefit_11_ap_std <- ugarchfit(garchspec_11_ap_std, data = rtsi_1520, out.sample = 250)
forecast_11_ap_std <- ugarchforecast(forefit_11_ap_std, data = NULL, n.ahead = 1, n.roll = 250)
sigma_11_ap_std <- xts(sigma(forecast_11_ap_std)[-1], order.by = index(proxy_daily))
mse(sqrt(proxy_daily), sigma_11_ap_std)
mae(sqrt(proxy_daily), sigma_11_ap_std)
mape(sqrt(proxy_daily), sigma_11_ap_std)
smape(sqrt(proxy_daily), sigma_11_ap_std)

par(no.readonly = TRUE)
par()
plot(sqrt(proxy_daily), col = "grey", main = "Predicted values")
lines(sigma_11_std , col = "firebrick3")
lines(sigma_11_i_std , col = "goldenrod2")
lines(sigma_11_gjr_std , col = "green3")
lines(sigma_11_e_std , col = "steelblue3")
lines(sigma_11_ap_std , col = "darkorchid3")
lines(rf_pred_fs, col = "black")
legend("left", legend = c("GARCH", "IGARCH", "GJR-GARCH", "EGARCH", "APARCH", "RF"), 
       col=c("deeppink4", "navy", "green", "orange", "red", "black"))
plot(sqrt(proxy_daily), col = "grey", main = "Predicted values")
lines(sigma_11_std , col = "green3")
lines(sigma_11_i_std , col = "goldenrod2")
lines(sigma_11_gjr_std , col = "firebrick3")
lines(sigma_11_e_std , col = "darkorchid3")
lines(sigma_11_ap_std , col = "steelblue3")
lines(rf_pred_fs, col = "black")

######################### Прогнозы по укороченной выборке ###################### 
plot(sqrt(proxy_daily)["2020-02-03/2020-07-03"])
nrow(proxy_daily["2020-05-04/2020-12-30"])
nrow(proxy_daily["2020-01-03/2020-04-30"])

data_short <- rtsi_1520["2015-01-06/2020-04-30"]
#1. GARCH(1,1) - std
#garchfit_11_std
forefit_11_std_sh <- ugarchfit(garchspec_11_std, data = rtsi_1520, out.sample = 168)
forecast_11_std_sh <- ugarchforecast(forefit_11_std_sh, data = NULL, n.ahead = 1, n.roll = 168)
sigma_11_std_sh <- xts(sigma(forecast_11_std_sh)[-1], order.by = index(proxy_daily["2020-05-04/2020-12-30"]))
plot(sqrt(proxy_daily), col = "grey")
lines(sigma_11_std_sh , col = "deeppink4")
mse(sqrt(proxy_daily["2020-05-04/2020-12-30"]), sigma_11_std_sh)
mae(sqrt(proxy_daily["2020-05-04/2020-12-30"]), sigma_11_std_sh)
mape(sqrt(proxy_daily["2020-05-04/2020-12-30"]), sigma_11_std_sh)
smape(sqrt(proxy_daily["2020-05-04/2020-12-30"]), sigma_11_std_sh)

#2. IGARCH(1,1) - std
#garchfit_11_i_std
forefit_11_i_std_sh <- ugarchfit(garchspec_11_i_std, data = rtsi_1520, out.sample = 168)
forecast_11_i_std_sh <- ugarchforecast(forefit_11_i_std_sh, data = NULL, n.ahead = 1, n.roll = 168)
sigma_11_i_std_sh <- xts(sigma(forecast_11_i_std_sh)[-1], order.by = index(proxy_daily["2020-05-04/2020-12-30"]))
plot(sqrt(proxy_daily), col = "grey")
lines(sigma_11_std_sh, col = "deeppink4")
lines(sigma_11_i_std_sh , col = "navy")
mse(sqrt(proxy_daily), sigma_11_i_std_sh)
mae(sqrt(proxy_daily), sigma_11_i_std_sh)
mape(sqrt(proxy_daily), sigma_11_i_std_sh)
smape(sqrt(proxy_daily), sigma_11_i_std_sh)

#3. GJR-GARCH(1,1) - std
#garchfit_11_gjr_std
forefit_11_gjr_std_sh <- ugarchfit(garchspec_11_gjr_std, data = rtsi_1520, out.sample = 168)
forecast_11_gjr_std_sh <- ugarchforecast(forefit_11_gjr_std_sh, data = NULL, n.ahead = 1, n.roll = 168)
sigma_11_gjr_std_sh <- xts(sigma(forecast_11_gjr_std_sh)[-1], order.by = index(proxy_daily["2020-05-04/2020-12-30"]))
plot(sqrt(proxy_daily), col = "grey")
lines(sigma_11_std_sh , col = "deeppink4")
lines(sigma_11_i_std_sh , col = "navy")
lines(sigma_11_gjr_std_sh , col = "green")
mse(sqrt(proxy_daily["2020-05-04/2020-12-30"]), sigma_11_gjr_std_sh)
mae(sqrt(proxy_daily["2020-05-04/2020-12-30"]), sigma_11_gjr_std_sh)
mape(sqrt(proxy_daily["2020-05-04/2020-12-30"]), sigma_11_gjr_std_sh)
smape(sqrt(proxy_daily["2020-05-04/2020-12-30"]), sigma_11_gjr_std_sh)

#4. EGARCH(1,1) - std
#garchfit_11_e_std
forefit_11_e_std_sh <- ugarchfit(garchspec_11_e_std, data = rtsi_1520, out.sample = 168)
forecast_11_e_std_sh <- ugarchforecast(forefit_11_e_std_sh, data = NULL, n.ahead = 1, n.roll = 168)
sigma_11_e_std_sh <- xts(sigma(forecast_11_e_std_sh)[-1], order.by = index(proxy_daily["2020-05-04/2020-12-30"]))
plot(sqrt(proxy_daily), col = "grey")
lines(sigma_11_std_sh , col = "deeppink4")
lines(sigma_11_i_std_sh , col = "navy")
lines(sigma_11_gjr_std_sh , col = "green")
lines(sigma_11_e_std_sh , col = "orange")
mse(sqrt(proxy_daily), sigma_11_e_std_sh)
mae(sqrt(proxy_daily), sigma_11_e_std_sh)
mape(sqrt(proxy_daily), sigma_11_e_std_sh)
smape(sqrt(proxy_daily), sigma_11_e_std_sh)

#5. APARCH(1,1) - std
#garchfit_11_ap_std
forefit_11_ap_std_sh <- ugarchfit(garchspec_11_ap_std, data = rtsi_1520, out.sample = 168)
forecast_11_ap_std_sh <- ugarchforecast(forefit_11_ap_std_sh, data = NULL, n.ahead = 1, n.roll = 168)
sigma_11_ap_std_sh <- xts(sigma(forecast_11_ap_std_sh)[-1], order.by = index(proxy_daily["2020-05-04/2020-12-30"]))
mse(sqrt(proxy_daily), sigma_11_ap_std_sh)
mae(sqrt(proxy_daily), sigma_11_ap_std_sh)
mape(sqrt(proxy_daily), sigma_11_ap_std_sh)
smape(sqrt(proxy_daily), sigma_11_ap_std_sh)

plot(sqrt(proxy_daily), col = "grey")
lines(sigma_11_std_sh , col = "deeppink4")
lines(sigma_11_i_std_sh , col = "navy")
lines(sigma_11_gjr_std_sh , col = "green")
lines(sigma_11_e_std_sh , col = "orange")
lines(sigma_11_ap_std_sh , col = "red3")


#делаем частичный постянный реэстимейшн
train_1_sh <- data_ML[1:1342,5:8]
sample_1_sh <- data_ML[1343:1362,5:8]

train_2_sh <- data_ML[21:1362,5:8]
sample_2_sh <- data_ML[1363:1382,5:8]

train_3_sh <- data_ML[41:1382,5:8]
sample_3_sh <- data_ML[1383:1402,5:8]

train_4_sh <- data_ML[61:1402,5:8]
sample_4_sh <- data_ML[1403:1422,5:8]

train_5_sh <- data_ML[81:1422,5:8]
sample_5_sh <- data_ML[1423:1442,5:8]

train_6_sh <- data_ML[101:1442,5:8]
sample_6_sh <- data_ML[1443:1462,5:8]

train_7_sh <- data_ML[121:1462,5:8]
sample_7_sh <- data_ML[1463:1482,5:8]

train_8_sh <- data_ML[141:1482,5:8]
sample_8_sh <- data_ML[1483:1510,5:8]


set.seed(123)  
forest_1_sh <- randomForest(formula = train_1_sh$Sigma ~ ., 
                         data = train_1_sh, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_1_sh <- predict(object = forest_1_sh, newdata = sample_1)

#type = "anova") # return classification labels
predict_1_sh <- xts(pred_1_sh, order.by = index(proxy_daily["2020-05-04/2020-06-01"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_1_sh, col = "red")

#### forest 2 ####
forest_2_sh <- randomForest(formula = train_2_sh$Sigma ~ ., 
                         data = train_2_sh, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_2_sh <- predict(object = forest_2_sh, newdata = sample_2_sh)

#type = "anova") # return classification labels
predict_2_sh <- xts(pred_2_sh, order.by = index(proxy_daily["2020-06-02/2020-07-02"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_2_sh, col = "red")

#### forest 3 ####
forest_3_sh <- randomForest(formula = train_3_sh$Sigma ~ ., 
                         data = train_3_sh, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_3_sh <- predict(object = forest_3_sh, newdata = sample_3_sh)

#type = "anova") # return classification labels
predict_3_sh <- xts(pred_3_sh, order.by = index(proxy_daily["2020-07-03/2020-07-30"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_3_sh, col = "red")

#### forest 4 ####
forest_4_sh <- randomForest(formula = train_4_sh$Sigma ~ ., 
                         data = train_4_sh, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_4_sh <- predict(object = forest_4_sh, newdata = sample_4_sh)

#type = "anova") # return classification labels
predict_4_sh <- xts(pred_4_sh, order.by = index(proxy_daily["2020-07-31/2020-08-27"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_4_sh, col = "red")

#### forest 5 ####
forest_5_sh <- randomForest(formula = train_5_sh$Sigma ~ ., 
                         data = train_5_sh, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_5_sh <- predict(object = forest_5_sh, newdata = sample_5_sh)

#type = "anova") # return classification labels
predict_5_sh <- xts(pred_5_sh, order.by = index(proxy_daily["2020-08-28/2020-09-24"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_5_sh, col = "red")

#### forest 6 ####
forest_6_sh <- randomForest(formula = train_6_sh$Sigma ~ ., 
                         data = train_6_sh, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_6_sh <- predict(object = forest_6_sh, newdata = sample_6_sh)

#type = "anova") # return classification labels
predict_6_sh <- xts(pred_6_sh, order.by = index(proxy_daily["2020-09-25/2020-10-22"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_6_sh, col = "red")

#### forest 7 ####
forest_7_sh <- randomForest(formula = train_7_sh$Sigma ~ ., 
                         data = train_7_sh, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_7_sh <- predict(object = forest_7_sh, newdata = sample_7_sh)

#type = "anova") # return classification labels
predict_7_sh <- xts(pred_7_sh, order.by = index(proxy_daily["2020-10-23/2020-11-20"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_7_sh, col = "red")

#### forest 8 ####
forest_8_sh <- randomForest(formula = train_8_sh$Sigma ~ ., 
                         data = train_8_sh, ntree = 500, mtry = 1, nodesize = 5, importance=TRUE)
#### Generate predicted classes using the model object
pred_8_sh <- predict(object = forest_8_sh, newdata = sample_8_sh)

#type = "anova") # return classification labels
predict_8_sh <- xts(pred_8_sh, order.by = index(proxy_daily["2020-11-23/2020-12-30"]))
plot(sqrt(proxy_daily), col = "grey")
lines(predict_8_sh, col = "red")

rf_pred_fs_sh <- c(predict_1_sh, predict_2_sh, predict_3_sh, predict_4_sh, predict_5_sh, predict_6_sh,
                predict_7_sh, predict_8_sh)
mse(sqrt(proxy_daily["2020-05-04/2020-12-30"]), rf_pred_fs_sh)
mae(sqrt(proxy_daily["2020-05-04/2020-12-30"]),rf_pred_fs_sh)
mape(sqrt(proxy_daily["2020-05-04/2020-12-30"]), rf_pred_fs_sh)
smape(sqrt(proxy_daily["2020-05-04/2020-12-30"]), rf_pred_fs_sh)

plot(sqrt(proxy_daily["2020-05-04/2020-12-30"]), col = "grey", main = "Predicted values")
lines(sigma_11_std_sh, col = "green3")
lines(sigma_11_i_std_sh , col = "goldenrod2")
lines(sigma_11_gjr_std_sh , col = "firebrick3")
lines(sigma_11_e_std_sh , col = "darkorchid3")
lines(sigma_11_ap_std_sh , col = "steelblue3")
lines(rf_pred_fs_sh, col = "black")


plot(sqrt(proxy_daily["2020-04-21/2020-12-30"]), col = "grey", main = "Predicted values")
lines(sigma_11_std_sh, col = "green3")
lines(sigma_11_i_std_sh , col = "goldenrod2")
lines(sigma_11_gjr_std_sh , col = "firebrick3")
lines(sigma_11_e_std_sh , col = "darkorchid3")
lines(sigma_11_ap_std_sh , col = "steelblue3")
lines(rf_pred_fs_sh, col = "black")



garchspec_11_e_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), distribution.model = "std")


forefit__e_1 <- ugarchfit(garchspec_11_e_std, data = rtsi_1520, out.sample = 168)
forecast_e_1 <- ugarchforecast(forefit__e_1, data = NULL, n.ahead = 1, n.roll = 20)
sigma_e_1 <- xts(sigma(forecast_e_1)[-1], order.by = index(proxy_daily["2020-05-04/2020-06-01"]))

forefit__e_2 <- ugarchfit(garchspec_11_e_std, data = rtsi_1520, out.sample = 148)
forecast_e_2 <- ugarchforecast(forefit__e_2, data = NULL, n.ahead = 1, n.roll = 20)
sigma_e_2 <- xts(sigma(forecast_e_2)[-1], order.by = index(proxy_daily["2020-06-02/2020-07-02"]))

forefit__e_3 <- ugarchfit(garchspec_11_e_std, data = rtsi_1520, out.sample = 128)
forecast_e_3 <- ugarchforecast(forefit__e_3, data = NULL, n.ahead = 1, n.roll = 20)
sigma_e_3 <- xts(sigma(forecast_e_3)[-1], order.by = index(proxy_daily["2020-07-03/2020-07-30"]))

forefit__e_4 <- ugarchfit(garchspec_11_e_std, data = rtsi_1520, out.sample = 108)
forecast_e_4 <- ugarchforecast(forefit__e_4, data = NULL, n.ahead = 1, n.roll = 20)
sigma_e_4 <- xts(sigma(forecast_e_4)[-1], order.by = index(proxy_daily["2020-07-31/2020-08-27"]))

forefit__e_5 <- ugarchfit(garchspec_11_e_std, data = rtsi_1520, out.sample = 88)
forecast_e_5 <- ugarchforecast(forefit__e_5, data = NULL, n.ahead = 1, n.roll = 20)
sigma_e_5 <- xts(sigma(forecast_e_5)[-1], order.by = index(proxy_daily["2020-08-28/2020-09-24"]))

forefit__e_6 <- ugarchfit(garchspec_11_e_std, data = rtsi_1520, out.sample = 68)
forecast_e_6 <- ugarchforecast(forefit__e_6, data = NULL, n.ahead = 1, n.roll = 20)
sigma_e_6 <- xts(sigma(forecast_e_6)[-1], order.by = index(proxy_daily["2020-09-25/2020-10-22"]))

forefit__e_7 <- ugarchfit(garchspec_11_e_std, data = rtsi_1520, out.sample = 48)
forecast_e_7 <- ugarchforecast(forefit__e_7, data = NULL, n.ahead = 1, n.roll = 20)
sigma_e_7 <- xts(sigma(forecast_e_7)[-1], order.by = index(proxy_daily["2020-10-23/2020-11-20"]))

forefit__e_8 <- ugarchfit(garchspec_11_e_std, data = rtsi_1520, out.sample = 28)
forecast_e_8 <- ugarchforecast(forefit__e_8, data = NULL, n.ahead = 1, n.roll = 28)
sigma_e_8 <- xts(sigma(forecast_e_8)[-1], order.by = index(proxy_daily["2020-11-23/2020-12-30"]))

sigma_e_full_sh <- c(sigma_e_1, sigma_e_2, sigma_e_3, sigma_e_4, 
                     sigma_e_5, sigma_e_6, sigma_e_7, sigma_e_8)

plot(sqrt(proxy_daily), col = "grey")
lines(sigma_11_e_std_sh , col = "maroon")
lines(sigma_e_full_sh, col = "navy")

sigma_11_e_std_sh-sigma_e_full_sh

mse(sqrt(proxy_daily["2020-05-04/2020-12-30"]), sigma_e_full_sh)
mae(sqrt(proxy_daily["2020-05-04/2020-12-30"]),sigma_e_full_sh)
mape(sqrt(proxy_daily["2020-05-04/2020-12-30"]), sigma_e_full_sh)
smape(sqrt(proxy_daily["2020-05-04/2020-12-30"]), sigma_e_full_sh)

plot(sqrt(proxy_daily), col = "grey")
lines(sigma_e_1, col = "navy")
lines(sigma_e_2, col = "green")
lines(sigma_e_3, col = "orange")
lines(sigma_e_4, col = "yellow")
lines(sigma_e_5, col = "blue2")
lines(sigma_e_6, col = "pink")
lines(sigma_e_7, col = "orchid3")
lines(sigma_e_8, col = "purple")
lines(sigma_11_e_std_sh , col = "maroon")

forefit_11_e_std_sh@fit$coef
garchfit_11_e_std@fit$coef
forefit__e_6@fit$coef
forefit__e_7@fit$coef

########################## Value-at-Risk ############################
install.packages('fExtremes', repos='http://cran.rstudio.com', type='source')
library(fExtremes)
CVaR(rf_pred_fs, alpha = 0.05, type = "sample", tail = c("upper"))
CVaR(sigma_11_e_std, alpha = 0.05, type = "sample", tail = c("upper"))

my_var <- VaR(sigma_11_e_std, 0.95, "modified")


roll = ugarchroll(garchspec_11_e_std, rtsi_1520, n.ahead = 1, forecast.length = 250, 
         n.start = 1261, refit.every = 249, refit.window = c("moving"), solver = "hybrid", fit.control = list(), 
         solver.control = list(), calculate.VaR = TRUE, VaR.alpha = c(0.01), 
         cluster = NULL, keep.coef = TRUE)
plot(roll)
report(roll, type = "VaR", VaR.alpha = 0.05, conf.level = 0.95)
garchVar <- quantile(roll, probs = 0.05)

VaRplot(alpha = 0.05, actual = rtsi_2020, VaR = garchVar)
VaRTest(alpha = 0.05, as.numeric(rtsi_2020), as.numeric(VaR = garchVar), conf.level = 0.95)


mu_rtsi <- mean(rtsi_1519)
rf_var = (-mu_rtsi - garchVar * rf_pred_fs)

my_quantile5 <- as.numeric(coredata(garchVar))
class(my_quantile5)
my_sigma_est_rf <- as.numeric(coredata(rf_pred_fs))
class(my_sigma_est_rf)
quant_to_sg_rf <- my_quantile5*my_sigma_est_rf
head(quant_to_sg_rf)
my_var_rf <- (-mu_rtsi+quant_to_sg_rf)
my_var_rf_ts <- xts(my_var_rf, order.by = index(proxy_daily))
plot(rtsi_2020, col = "grey")
plot(my_var_rf_ts, col = "maroon")
plot(rtsi_2020)

my_quantile <- xts(my_quantile5, order.by = index(proxy_daily))
plot(my_quantile)

quantile.zoo(rtsi_1519, 0.05)



#пробуем ролл
roll = ugarchroll(garchspec_11_n, rtsi_1520, refit.every = 100,
                  refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                  VaR.alpha = c(0.01, 0.05), keep.coef = TRUE)
show(roll)
roll = ugarchroll(garchspec_11_n, rtsi_1520, n.ahead = 1, forecast.length = 250, 
                  n.start = 1261, refit.every = 25, refit.window = c("recursive", "moving"), 
                  window.size = 500, solver = "hybrid", fit.control = list(), 
                  solver.control = list(), calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05), 
                  cluster = NULL, keep.coef = TRUE)
show(roll)
report(roll, type = "VaR", VaR.alpha = 0.05, conf.level = 0.95)

#### End of code #### 
