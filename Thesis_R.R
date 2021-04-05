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
View(proxy_df)
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

#### Progress to log-returns
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
plot(sq_2020)


#proxy_hours <- (dates_proxy$Time)/10000
#binded <- cbind.data.frame(proxy_days, proxy_hours)
#binded
#make_datetime() 
#proxy_dates <- ymd(dates_proxy)
#class(proxy_dates)
#head(proxy_dates)


#### Structural Brakes Test (CUSUM)
library(CPAT)
CUSUM.test(rtsi_1520, use_kernel_var = FALSE, stat_plot = FALSE,
           kernel = "ba", bandwidth = "and")
#returns do not have structural brakes
CUSUM.test(close_ts["2015-01-06/2020-12-30"], use_kernel_var = FALSE, stat_plot = FALSE,
           kernel = "ba", bandwidth = "and")
#prices do have structural brakes
mean(rtsi_1520)

#### Heteroscedasticity check
#ar1 <- ar(rtsi_1520, aic = TRUE, order.max = 1)
#summary(ar1)
#bptest(ar1, studentize = FALSE)


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
adf = ur.df(rtsi_1520, selectlags = 'BIC', type = 'none') 
summary(adf)
kpss_ur <- ur.kpss(rtsi_1520, type = c("mu"), lags = c("long")) #mu = with a drift, tau - linear trend
summary(kpss_ur)
kpss_ur_tau <- ur.kpss(rtsi_1520, type = c("tau"), lags = c("long")) #mu = with a drift, tau - linear trend
summary(kpss_ur_tau)

kpss_ur <- ur.kpss(close_ts["2015-01-06/2020-12-30"], type = c("mu"), lags = c("long")) #mu = with a drift, tau - linear trend
summary(kpss_ur)
kpss_ur_tau <- ur.kpss(close_ts["2015-01-06/2020-12-30"], type = c("tau"), lags = c("long")) #mu = with a drift, tau - linear trend
summary(kpss_ur_tau)

kpss.test(rtsi_1520, null = c("Level"))
kpss.test(rtsi_1520, null = c("Trend"))
kpss.test(close_ts["2015-01-06/2020-12-30"], null = c("Level"))
kpss.test(close_ts["2015-01-06/2020-12-30"], null = c("Trend"))
# according to all the test the returns are stationary

# the same result is achieved with rtsi_1519
Box.test(rtsi_1520,lag = 10, fitdf = 0, type="Lj")

# are squared returns stationary?
acf_1520 <- ggAcf(rtsi_1520, 40)
pacf_1520 <- ggPacf(rtsi_1520, 40)
acf_1520
pacf_1520
grid.arrange(acf_1520, pacf_1520, nrow = 2)


#### Check for ARCH - ?
#sq_ret_1520 <- rtsi_1520^2  #((1+rtsi_1520)^2)-1
#sq_ret_1520 <- abs(((1+rtsi_1520)^2)-1)
sq_ret_1520 <- rtsi_1520^2
plot(sq_ret_1520)
#plot(sq_ret_1520["2015-01-06/2020-12-30"])
#plot(abs(rtsi_1520["2019-10-01/2020-12-30"]))
#plot(tail(sq_ret_1520, 250))
tail(sq_ret_1520)
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
 


#### GARCH FAMILY ####
# 1. GARCH(1.1) with normal dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH"), 
                        distribution.model = "norm")
# Estimate the GARCH(1.1) model with normal dist.
garchfit_11_n <- ugarchfit(data = rtsi_1520, spec = garchspec_11_n)
summary(garchfit_11_n)

plot(garchfit_11_n, which = "all")
plot(garchfit_11_n, which = 5)

# 2. GARCH(1.1) with student dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH"), 
                             distribution.model = "std")
# Estimate the GARCH(1.1) model with normal dist.
garchfit_11_std <- ugarchfit(data = rtsi_1519, spec = garchspec_11_std)
garchfit_11_std

plot(garchfit_11_std, which = "all")
plot(garchfit_11_std, which = 8)

class(garchfit_11_std)
resid(garchfit_11_std)
jarque.test(resid(garchfit_11_std))

# 3. EGARCH(1.1) with student dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_e_st <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "eGARCH"), 
                               distribution.model = "std")
# Estimate the GARCH(1.1) model with normal dist.
garchfit_11_e_st <- ugarchfit(data = rtsi_1520, spec = garchspec_11_e_st)
garchfit_11_e_st

# 4. EGARCH(1.1) with norm dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_e_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "eGARCH"), 
                               distribution.model = "norm")
# Estimate the GARCH(1.1) model with normal dist.
garchfit_11_e_n <- ugarchfit(data = rtsi_1520, spec = garchspec_11_e_n)
garchfit_11_e_n

# 5. GJR-GARCH(1.1) with norm dist. 
# Specify a standard GARCH model with constant mean
garchspec_11_gjr_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                               variance.model = list(model = "gjrGARCH"), 
                               distribution.model = "norm")
# Estimate the GARCH(1.1) model with normal dist.
garchfit_11_gjr_n <- ugarchfit(data = rtsi_1520, spec = garchspec_11_gjr_n)
garchfit_11_gjr_n 

infocriteria(garchfit_11_n)
infocriteria(garchfit_11_std)
infocriteria(garchfit_11_e_st)
infocriteria(garchfit_11_e_n)
infocriteria(garchfit_11_gjr_n)
infocriteria(garchfit_11_ap_n)

# Use the method sigma to retrieve the estimated volatilities 
garchvol <- sigma(garchfit_11_n)

#### Do the same with in/out-of-sample 
## estimate GARCH(1,1) on 2015-2018; normal distr
garchfit_11_n_1518 <- ugarchfit(data = rtsi_1518, spec = garchspec_11_n)
garchfit_11_n_1518
#plot it
plot(garchfit_11_n_1518, which = "all")
plot(garchfit_11_n_1518, which = 8)



#APARCH
garchspec_11_ap_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                 variance.model = list(model = 'apARCH'), 
                                 distribution.model = "norm")
# Estimate the APARCH(1.1) model with normal dist.
garchfit_11_ap_n <- ugarchfit(data = rtsi_1520, spec = garchspec_11_ap_n)
garchfit_11_ap_n 

garchspec_11_ap_std <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                                variance.model = list(model = 'apARCH'), 
                                distribution.model = "std")
# Estimate the APARCH(1.1) model with normal dist.
garchfit_11_ap_std <- ugarchfit(data = rtsi_1520, spec = garchspec_11_ap_std)
garchfit_11_ap_std

#INFOCRITERIA
infocriteria(garchfit_11_n)
infocriteria(garchfit_11_std)
infocriteria(garchfit_11_e_st)
infocriteria(garchfit_11_e_n)
infocriteria(garchfit_11_gjr_n)
infocriteria(garchfit_11_ap_n)
infocriteria(garchfit_11_ap_std)

#### Forecasts ####


# future teller - basic forecast (and a really bad one)
forecast_11_n_19 <- ugarchforecast(garchspec_11_n, data = rtsi_1519, 
                                n.ahead = 252, n.roll = 0, out.sample = 252)

# let's roll - rolling forecast for GARCH(1,1) with normal dist on 2019 
garchfit_11_n_1519 <- ugarchfit(data = rtsi_1519, spec = garchspec_11_n, out.sample = 252)
forecast_11_n_1519 <- ugarchforecast(garchfit_11_n_1519, data = NULL, 
                                     n.ahead = 1, n.roll = 252, out.sample = 252)
show(forecast_11_n_1519)

 
forecast_fit <- ugarchfit(garchspec_11_std, data = rtsi_1520, out.sample = 250)
forecastt_rolling_std <- ugarchforecast(forecast_fit, data = NULL, n.ahead = 1, n.roll = 250,
                                        out.sample = 250)
show(forecastt_rolling_std)




#### 2020 included - worsen it all
# Estimate the GARCH(1.1) model with normal dist.
#garchfit_11_n_1520 <- ugarchfit(data = rtsi_1520, spec = garchspec_11_n)
#garchfit_11_n_1520
#plot(garchfit_11_n_1520, which = 8)
#plot(garchfit_11_n_1520, which = "all")



#________________ВСПОМОГАТЕЛЬНЫЕ РАСЧЕТЫ ДЛЯ ГАРЧ С ДРУГИМИ ПОРЯДКАМИ P и Q____________
#Суть: в целом модели очень похожи - различий по IC либо нет либо несущественные 
#Технически, можем оставить для прогнозов любую из моделей?

# GARCH(1.2) with normal dist. 
garchspec_12_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(1, 2)), 
                             distribution.model = "norm")
garchfit_12_n <- ugarchfit(data = rtsi_1520, spec = garchspec_12_n)

# Estimate the GARCH(2.1) model with normal dist.
garchspec_21_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(2, 1)), 
                             distribution.model = "norm")
garchfit_21_n <- ugarchfit(data = rtsi_1520, spec = garchspec_21_n)
summary(garchfit_21_n)

# Estimate the GARCH(2.1) model with normal dist
garchspec_22_n <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(2, 2)), 
                             distribution.model = "norm")
garchfit_22_n <- ugarchfit(data = rtsi_1520, spec = garchspec_22_n)
# IC comparison
infocriteria(garchfit_12_n)
infocriteria(garchfit_21_n)
infocriteria(garchfit_22_n)


# GARCH(2,1) with student dist. 
# Specify a standard GARCH model with constant mean
garchspec_21_std <- ugarchspec(mean.model = list(armaOrder = c(0,0), garchOrder = c(2, 1)),
                               variance.model = list(model = "sGARCH"), 
                               distribution.model = "std")
garchfit_21_std <- ugarchfit(data = rtsi_1519, spec = garchspec_21_std)

#### End of code #### 
