# Structure #####
rm(list=ls())
dev.off()

library(fredr)
fredr_set_key("a82c6ba96222edd5450dc4b0319c5ac6")
library(forecast)
library(scales)
require(urca)
library(texreg)
library("lmtest")

# Fetch and prepare data
data <- fredr(series_id = "ND000349Q", observation_start = as.Date("2002-01-01"), observation_end = as.Date("2024-01-01"))
PCE <- data$value
s <- 4
startyear <- 2002
datastart <- c(startyear, 1)
PCE_data <- ts(PCE, frequency = s, start = datastart)
log_PCE <- log(PCE_data)
T <- length(PCE)
t <- (1:T)
t2 <- t^2

### Plot --------------------------------------------------------
plot(PCE_data, 
     col = "royalblue3", 
     xlab = " ", 
     ylab = " ", 
     main = " ",
     xlim = c(2002, 2025),
     xaxt = "n")

years <- seq(2002, 2027, by = 1)
positions <- seq(2002, 2025, by = 1)
axis(1,at = positions, labels = years[1:length(positions)], mgp = c(3, 1, 0)) 
abline(v = positions, col = "lightgray", lty = 3)
mtext("Real Personal Consumption Expenditures", side = 3, line = 0.3, cex = 1)
mtext("Year", side = 1, line = 2.5, cex = 0.7)
mtext("Billions of Chained 2017 Dollars", side = 2, line = 2.5, cex = 0.7)

# Difference, check need for log transformation
plot(diff(PCE_data), xlab = "Year", main = "Difference")
abline(h = 0, col = "grey", lwd = 2) 

### Model 1: Deterministic Trend --------------------------------------------------------
# Fit linear and quadratic trend models
modelL <- lm(log_PCE ~ t)
modelQ <- lm(log_PCE ~ t + t2)

M <- seasonaldummy(log_PCE)
TrSeasQ <- model.matrix(~ t + t2 + M)
TrSeasL <- model.matrix(~ t + M)

screenreg(list(modelL, modelQ), digits = 6)
# Quadratic trend best

# Residual analysis for linear and quadratic models
par(mfrow=c(2,1), mar=c(3,5,3,3)) 

trendL <- log_PCE - modelL$residuals
residualsL <- log_PCE - trendL
plot(log_PCE, ylim = c(0.9 * min(log_PCE), max(log_PCE)), main = "Residuals, linear trend")
lines(trendL, col = "royalblue")
par(new = TRUE)
plot(residualsL, ylim = c(min(residualsL), 3 * max(residualsL)), ylab = '', axes = FALSE)
axis(4, pretty(c(min(residualsL), max(residualsL))))
abline(h = 0, col = 'grey')
mtext("Residuals", side = 4, line = 2.5, at = -0.05)

trendQ <- log_PCE - modelQ$residuals
residualsQ <- log_PCE - trendQ
plot(log_PCE, ylim = c(0.9 * min(log_PCE), max(log_PCE)), main = "Residuals, quadratic trend")
lines(trendQ, col = "blue")
par(new = TRUE)
plot(residualsQ, ylim = c(min(residualsQ), 3 * max(residualsQ)), ylab = '', axes = FALSE)
axis(4, pretty(c(min(residualsQ), max(residualsQ))))
abline(h = 0, col = 'grey')
mtext("Residuals", side = 4, line = 2.5, at = 0)

# Choose quadratic model
modelT <- modelQ
trend <- log_PCE - modelT$residuals
residuals <- log_PCE - trend

# Confirm neglected seasonality using ACF
acf(residuals)

### Adding seasonal component to the model ###
M <- seasonaldummy(log_PCE)
modelTS<- lm(log_PCE~t+t2+M)
screenreg(list(modelT, modelTS), digits = 6)

#Checking for serial correlation
dwtest(modelTS)

#Model fit and residuals
TS <- log_PCE-modelTS$residuals
residuals <- log_PCE-TS
plot(log_PCE,ylim=c(0.9*min(log_PCE),max(log_PCE)), main="Model TS fit and residuals")
lines(TS,col="blue")
par(new=T)
plot(residuals,ylim=c(min(residuals),3*max(residuals)),ylab='',axes=F)
axis(4, pretty(c(min(residuals),max(residuals))))
abline(h=0,col='grey')
mtext("Residuals",side=4,line=2.5,at=0)

### Adding an ARIMA model ###
#Which model to choose
par(mfrow=c(2,1), mar=c(3,5,3,3)) 
acf(residuals,main="Deterministic trend, residuals")
pacf(residuals,main="Deterministic trend, residuals")

# Quadratic trend
model.arma <- auto.arima(log_PCE, d=0, seasonal = FALSE, ic = "aic", xreg = TrSeasQ, 
                         stepwise = FALSE, approximation = FALSE, 
                         trace = TRUE)
# Best model: Regression with ARIMA(1,0,1) 

# Linear trend
model.arma <- auto.arima(log_PCE, d=0, seasonal = FALSE, ic = "aic", xreg = TrSeasL, 
                         stepwise = FALSE, approximation = FALSE, 
                         trace = TRUE)
# Best model: Regression with ARIMA(1,0,0)

#Computing the ARMA(1,0,1) model
model.ARMA11 <- Arima(log_PCE,order = c(1, 0, 1),include.mean = FALSE,xreg=TrSeasQ)
model.ARMA11

#Computing the AR(1) model
model.AR1 <- Arima(log_PCE,order = c(1, 0, 0),include.mean = FALSE,xreg=TrSeasQ)
model.AR1

##Comparing the models
screenreg(list(model.AR1, model.ARMA11), digits = 6)
par(mfrow=c(2,1))

#Model using AR(1)
TSC.AR1 <- log_PCE-model.AR1$residuals
residuals.AR1 <- log_PCE-TSC.AR1
plot(log_PCE,ylim=c(0.9*min(log_PCE),max(log_PCE)),main="Model TSC fit and residuals, AR(1)")
lines(TSC.AR1,col="blue")
par(new=T)
plot(residuals.AR1,ylim=c(min(residuals.AR1),3*max(residuals.AR1)),ylab='',axes=F)
axis(4, pretty(c(min(residuals.AR1),max(residuals.AR1))))
abline(h=0,col='grey')
mtext("Residuals",side=4,line=2.5,at=0)

#Model using ARMA(1,1)
TSC.ARMA11 <- log_PCE-model.ARMA11$residuals
residuals.ARMA11 <- log_PCE-TSC.ARMA11
par(mar=c(5,5,5,5))
plot(log_PCE,ylim=c(0.9*min(log_PCE),max(log_PCE)),main="Model TSC fit and residuals, ARMA(1,1)")
lines(TSC.ARMA11,col="blue")
par(new=T)
plot(residuals.ARMA11,ylim=c(min(residuals.ARMA11),3*max(residuals.ARMA11)),ylab='',axes=F)
axis(4, pretty(c(min(residuals.ARMA11),max(residuals.ARMA11))))
abline(h=0,col='grey')
mtext("Residuals",side=4,line=2.5,at=0)

par(mfrow=c(2,1), mar=c(3,5,3,3)) 
acf(residuals.AR1,main="AR1, residuals")
pacf(residuals.AR1,main="AR1, residuals")

acf(residuals.ARMA11,main="ARMA11, residuals")
pacf(residuals.ARMA11,main="ARMA11, residuals")

dev.off()

summary(model.AR1)
summary(model.ARMA11)

# AR(1,0,1) model
screenreg(model.ARMA11, digits = 6)
TSC <- log_PCE-model.ARMA11$residuals
residuals <- log_PCE-TSC
plot(log_PCE,ylim=c(0.9*min(log_PCE),max(log_PCE)),main="Model TSC fit and residuals")
lines(TSC,col="blue")
par(new=T)
plot(residuals,ylim=c(min(residuals),3*max(residuals)),ylab='',axes=F)
axis(4, pretty(c(min(residuals),max(residuals))))
abline(h=0,col='grey')
mtext("Residuals",side=4,line=2.5,at=0)

### De-seasonalize and Dickey-Fuller Test --------------------------------------------------------
y <- data$value
s <- 4 #frequency of the data - quarterly
startyear <- 2002; startmonth <- 1

data1 <- ts(data$value, start=c(startyear,startmonth),frequency=s)

# Create a time series object
data1 <- ts(y, start=c(startyear, startmonth), frequency=s)
plot(data1)

# Data is seasonal, so de-seasonalize
T <- length(data1) # set up the length of your data
years <- T/s # NOTE: works only if each year has a full set of seasonal observations

log_PCE <- log(data1)
T <- length(log_PCE)
y <- log_PCE

# Remove seasonality:
# First, compute trend by averaging over s consecutive observations and remove from the data
endperiod <- startmonth - 1
if(endperiod == 0) {
  endperiod <- s
}

detrended <- data1 - ma(data1, order = s)

# Remove the first and last year because of missing values
detrended <- window(detrended, start=c(startyear+1, startmonth), end=c(startyear+years-2, endperiod))
plot(detrended)

# Compute the seasonal factors
seasonal.matrix <- matrix(detrended, ncol=s, byrow=TRUE) 
seasonalFactor <- apply(seasonal.matrix, 2, mean)
seasonality <- rep(seasonalFactor, length.out=length(y))

# De-seasonalize the data
y <- data1 - seasonality
plot(y, xlab = "Year", ylab = "Billions of Chained 2017 Dollars", main="De-seasonalize data")

#Perform a formal test on Unit roots: Dickey-Fuller Test
data <- y
test <- ur.df(data,type=c("trend"),lags=10,selectlags="AIC")
summary(test)

#DO not reject unit root. There is evidence in favour of the unit root.

test <- ur.df(diff(data),type=c("drift"),lags=10,selectlags="AIC")
summary(test)

PCE_deseasonalized <- y-seasonality
diff1 <- diff(PCE_deseasonalized)
plot(diff1, main="De-seasonlaized, first difference",xlab="Year")
abline(h = 0, col = "grey", lwd = 2,lty=2) 

### Model 2: Stochastic Trend and Deterministic Seasonality ------------------------------------------
# Fit ARIMA model
M <- seasonaldummy(log_PCE)
XREG <- model.matrix(~0+M)

###Fitting ARIMA model
model.arima <- auto.arima(log_PCE, d=1, seasonal = FALSE, ic = "aic",
                          stepwise = FALSE, approximation = FALSE,
                          trace = TRUE, xreg = XREG)
#Best model is ARIMA(0,1,1)

#ARIMA(0,1,1)
model.ARIMA011 <- Arima(log_PCE,order = c(0,1,1),xreg=XREG)
model.arima011 <- log_PCE-model.ARIMA011$residuals
residuals.arima011 <- log_PCE-model.arima011

plot(log_PCE,ylim=c(0.9*min(log_PCE),max(log_PCE)),main="ARIMA(1,1,0) fit and residuals")
lines(model.arima011,col="blue")
par(new=T)
plot(residuals.arima011,ylim=c(min(residuals.arima011),3*max(residuals.arima011)),ylab='',axes=F)
axis(4, pretty(c(min(residuals.arima011),max(residuals.arima011))))
abline(h=0,col='grey')
mtext("Residuals",side=4,line=2.5,at=0)

par(mfrow=c(2,1), mar=c(3,5,3,3)) 
acf(residuals.arima011,main="ARIMA011, residuals")
pacf(residuals.arima011,main="ARIMA011, residuals")

#ARIMA(1,1,1)
model.ARIMA111 <- Arima(log_PCE, order = c(1, 1, 1), xreg = XREG)
model.arima111 <- log_PCE - model.ARIMA111$residuals
residuals.arima111 <- log_PCE - model.arima111

screenreg(model.ARIMA011, digits = 6)

plot(log_PCE, ylim = c(0.9 * min(log_PCE), max(log_PCE)), main = "Stochastic trend, deterministic seasonality")
lines(model.arima111, col = "royalblue")
par(new = TRUE)
plot(residuals.arima111, ylim = c(min(residuals.arima111), 3 * max(residuals.arima111)), ylab = '', axes = FALSE)
axis(4, pretty(c(min(residuals.arima111), max(residuals.arima111))))
abline(h = 0, col = 'grey')
mtext("Residuals", side = 4, line = 2.5, at = 0)

### Model 3: Seasonal ARIMA --------------------------------------------------------
# Seasonal differencing
Sdiff <- diff(log_PCE, lag = s)
plot(Sdiff, main = "Seasonal differenced data", ylab = "log PCE",xlab="Year")

#Stationarity test: Dickey-Fuller Test
test <- ur.df(Sdiff,type=c("drift"),lags=10,selectlags="AIC")
summary(test)
par(mfrow=c(3,1), mar=c(3,5,3,3))
plot(PCE_data, main="PCE");
plot(log_PCE, main="PCE, log-transformed");
plot(Sdiff, main="PCE, after seasonal differencing");
dev.off()

#Taking first difference
Sdiff.diff1 <- diff(Sdiff)
plot(Sdiff.diff1,main = "Seasonal differenced data, first difference", ylab = "log PCE",xlab="Year")
abline(h = 0, col = "grey", lwd = 2,lty=2) 

#Dickey Fuller Test again
test1 <- ur.df(Sdiff.diff1,type=c("drift"),lags=10,selectlags="AIC")
summary(test1)
par(mfrow=c(2,1), mar=c(3,5,3,3))
acf(Sdiff, lag.max = 3*s, main="Sdiff") 
pacf(Sdiff, lag.max = 3*s, main="Sdiff") 
dev.off()

### Fitting ARIMA 
model.arima <- auto.arima(log_PCE, d=0, seasonal = TRUE, ic = "aic",
                          stepwise = FALSE, approximation = FALSE,
                          trace = TRUE)
# Best model: ARIMA(1,0,0)(2,1,0)[4] with drift         

# Fit SARIMA model
model.Sarima <- Arima(log_PCE,order = c(1,0,0),seasonal = c(2,1,0))
model.Sar1 <- Arima(log_PCE,order = c(1, 0, 0),seasonal = c(0,1,1))
screenreg(list(model.Sar1, model.Sarima), digits = 6)
par(mfrow=c(2,1), mar=c(3,5,3,3))
acf(model.Sarima$residuals,main="SARIMA(100)(210), residuals")
pacf(model.Sarima$residuals,main="SARIMA(100)(210), residuals")
dev.off()

### Model 4: Holt-Winters Exponential Smoothing ----------------------------------------------------
# Holt-Winters Exponential Smoothing
HWsmooth <- HoltWinters(log_PCE)
plot(HWsmooth,col="royalblue", main = "Holt-Winters Smoothing", ylab = "log PCE")
legend("bottomright", legend = c("log_PCE data", "Holt-Winters fitted Line"), col = c("royalblue", "red"), lty = 1)

HWsmooth

### Model Comparison and selection of best model --------------------------------------------------------
rm(list=ls())
dev.off()
library(fredr); fredr_set_key("a82c6ba96222edd5450dc4b0319c5ac6")
library(forecast)
library(scales) #package to help customizing plot-axis
require(urca)

data <- fredr(series_id = "ND000349Q", observation_start = as.Date("2002-01-01"),observation_end = as.Date("2024-01-01"))
PCE <- data$value 
s <- 4 
startyear <- 2002
datastart <- c(startyear,1)
T <- length(PCE)
years <- T/s

#Turning the data into a time series:
PCE_data <- ts(PCE, frequency=s, start=datastart)
options(scipen=999)
log_PCE <- log(PCE_data)


### Deterministic trend, deterministic seasonality
t <- (1:T)
t2 <- t^2
M <- seasonaldummy(log_PCE)
TrSeasQ <- model.matrix(~ t+t2+M)
model.AR1 <- Arima(log_PCE,order = c(1, 0, 1),include.mean = FALSE,xreg=TrSeasQ)
screenreg(model.AR1, digits = 6)
summary(model.AR1)
TSC <- log_PCE-model.AR1$residuals
residuals <- log_PCE-TSC

plot(log_PCE,ylim=c(0.9*min(log_PCE),max(log_PCE)),main="Deterministic trend, deterministic seasonality")
lines(TSC,col="royalblue")
par(new=T)
plot(residuals,ylim=c(min(residuals),3*max(residuals)),ylab='',axes=F)
axis(4, pretty(c(min(residuals),max(residuals))))
abline(h=0,col='grey')
mtext("Residuals",side=4,line=2.5,at=0)

### Stochastic trend, deterministic seasonality
M <- seasonaldummy(log_PCE)
XREG <- model.matrix(~0+M)
model.ARIMA111 <- Arima(log_PCE,order = c(0,1,1),xreg=XREG)
model.arima111 <- log_PCE-model.ARIMA111$residuals
residuals.arima111 <- log_PCE-model.arima111

plot(log_PCE,ylim=c(0.9*min(log_PCE),max(log_PCE)),main="Stochastic trend, deterministic seasonality")
lines(model.arima111,col="royalblue")
par(new=T)
plot(residuals.arima111,ylim=c(min(residuals.arima111),3*max(residuals.arima111)),ylab='',axes=F)
axis(4, pretty(c(min(residuals.arima111),max(residuals.arima111))))
abline(h=0,col='grey')
mtext("Residuals",side=4,line=2.5,at=0)

summary(model.ARIMA111)

### Stochastic trend, stochastic seasonality
# Fit SARIMA model
Sdiff <- diff(log_PCE, lag=s)
plot(Sdiff, main="Seasonal differenced data", ylab="log PCE")
model.Sar1 <- Arima(log_PCE,order = c(1, 0, 0),seasonal = c(2,1,0))
model.seasAR1 <- log_PCE-model.Sar1$residuals
residuals.seasAR1 <- log_PCE-model.seasAR1

plot(log_PCE, ylim = c(0.9 * min(log_PCE), max(log_PCE)), main = "Stochastic trend, stochastic seasonality")
lines(model.seasAR1, col = "royalblue")
par(new = TRUE)
plot(residuals.seasAR1, ylim = c(min(residuals.seasAR1), 3 * max(residuals.seasAR1)), ylab = '', axes = FALSE)
axis(4, pretty(c(min(residuals.seasAR1), max(residuals.seasAR1))))
abline(h = 0, col = 'grey')
mtext("Residuals", side = 4, line = 2.5, at = 0)

summary(model.Sar1)

### Smoothing technique
HWsmooth <- HoltWinters(log_PCE)
plot(HWsmooth, main="Holt-Winters Smoothing", ylab="log PCE")
legend("bottomright", legend = c("log_PCE data", "Holt-Winters fitted Line"), col = c("black", "red"), lty=1)

par(mfrow=c(1,1)); par(mar=c(5,5,5,5))
HWfitted <- HWsmooth$fitted[,1]
HWresiduals <- log_PCE-HWfitted

plot(log_PCE, ylim=c(0.9*min(log_PCE), max(log_PCE)), main="Holt-Winters exponential smoothing")
lines(HWfitted, col="royalblue")
par(new=T)
plot(HWresiduals, ylim=c(min(HWresiduals), 3*max(HWresiduals)), ylab='', axes=F)
axis(4, pretty(c(min(HWresiduals), max(HWresiduals))))
abline(h=0, col='grey')
mtext("Residuals", side=4, line=2.5, at=0)

par(mfrow=c(2,1), mar=c(3,5,3,3)) 
acf(HWresiduals, main="Holt-Winters, residuals")
pacf(HWresiduals, main="Holt-Winters, residuals")

#Compare - In-sample forecasts
h <- s*2

# Split the data into training (y) and test (z) sets
y <- subset(log_PCE, end=length(log_PCE)-h); z <- subset(log_PCE, start=length(log_PCE)-h+1)

# Generate seasonal dummy variables for the training set
M_train <- seasonaldummy(y)
XREG_train <- model.matrix(~0 + M_train)

# Generate seasonal dummy variables for the test set
M_test <- seasonaldummy(z)
XREG_test <- model.matrix(~0 + M_test)

# Add Deterministic Trend, Deterministic Seasonality forecast
T <- length(log_PCE)
t <- 1:T
t2 <- t^2
M_full <- seasonaldummy(log_PCE)
TrSeasQ <- model.matrix(~ t + t2 + M_full)

# Fit ARIMA model with deterministic trend and deterministic seasonality
model.det <- Arima(log_PCE, order = c(1, 0, 1), include.mean = FALSE, xreg = TrSeasQ)

# Create the design matrix for the forecast horizon
t_forecast <- (T + 1):(T + h)
t2_forecast <- t_forecast^2
seasonality <- frequency(log_PCE)
M_forecast <- seasonaldummy(ts(rep(1, h), frequency = seasonality))
TrSeasQ_forecast <- model.matrix(~ t_forecast + t2_forecast + M_forecast)

# Forecast using the deterministic trend and deterministic seasonality model
forecast.det <- forecast(model.det, h = h, xreg = TrSeasQ_forecast)
forecast.det # In-sample forecast

# Plot the deterministic trend and deterministic seasonality forecast
plot(forecast.det, ylab = "log PCE", main = "Deterministic trend, deterministic seasonality")
lines(log_PCE)
lines(fitted(model.det), col = "royalblue", lty = 1, lwd = 1)

# Generate seasonal dummy variables for the training set
M <- seasonaldummy(y)
XREG <- model.matrix(~0 + M)

# Fit ARIMA model with stochastic trend and deterministic seasonality
model.StochDet <- Arima(y, order = c(1, 1, 1), xreg = XREG)
e1 <- model.StochDet$residuals
FIT.StochDet <- y - e1

# Generate seasonal dummy variables for the test set
M <- seasonaldummy(z)
XREG <- model.matrix(~0 + M)

# Forecast using the ARIMA model
forecast.StochDet <- forecast(model.StochDet, h = h, xreg = XREG)
forecast.StochDet # In-sample forecast

# Plot the ARIMA forecast
plot(forecast.StochDet, ylab = "log PCE", main = "Stochastic trend, deterministic seasonality")
lines(log_PCE)
lines(FIT.StochDet, col = "royalblue", lty = 1, lwd = 1)

# Fit ARIMA model with stochastic trend and stochastic seasonality
Tr.model.Sar1 <- Arima(y, order = c(1, 0, 0), seasonal = c(0, 1, 1))
e2 <- Tr.model.Sar1$residuals
FIT.Sar1 <- y - e2

# Forecast using the seasonal ARIMA model
forecast.Sar1 <- forecast(Tr.model.Sar1, h = h)
forecast.Sar1 # In-sample forecast

# Plot the seasonal ARIMA forecast
plot(forecast.Sar1, ylab = "log PCE", main = "Stochastic trend, stochastic seasonality")
lines(log_PCE)
lines(FIT.Sar1, col = "royalblue", lty = 1, lwd = 1)

# Fit Holt-Winters exponential smoothing model
Tr.HWsmooth <- HoltWinters(y)
e3 <- Tr.HWsmooth$fitted[, 1]
FIT.HWsmooth <- y - e3

# Forecast using the Holt-Winters model
forecast.HW <- forecast(Tr.HWsmooth, h = h)
forecast.HW # In-sample forecast

# Plot the Holt-Winters forecast
plot(forecast.HW, ylab = "log PCE", main = "Holt-Winters exponential smoothing")
lines(log_PCE)
lines(FIT.HWsmooth, col = "royalblue", lty = 1, lwd = 1)

# Calculate accuracy of the forecasts
accuracy(forecast.StochDet, z)
accuracy(forecast.Sar1, z)
accuracy(forecast.HW, z)

### Forecast --------------------------------------------------------
rm(list=ls())
dev.off()

# Load required libraries
library(fredr) # For fetching FRED data
library(forecast) # For forecasting methods
library(scales) # For customizing plot axes
library(urca) # For unit root and cointegration tests

# Set FRED API key
fredr_set_key("a82c6ba96222edd5450dc4b0319c5ac6")

# Fetch data from FRED
data <- fredr(series_id = "ND000349Q", observation_start = as.Date("2002-01-01"),
              observation_end = as.Date("2024-01-01"))

# Extract PCE values and setup time series
PCE <- data$value
s <- 4 # Frequency of the data (quarterly)
startyear <- 2002
datastart <- c(startyear, 1)
h <- s*2 # Forecast horizon (5 years)
fstart <- c(2024, 2) # Forecast starts in Q2 of 2024
T <- length(PCE)
years <- T / s
PCE_data <- ts(PCE, frequency = s, start = datastart)
t <- (1:T)
t2 <- t^2

# Log transformation of the data
log_PCE <- log(PCE_data)

# Computing forecast using ARIMA with deterministic seasonality
y <- subset(log_PCE, end=length(log_PCE)); z <- subset(log_PCE, start=length(log_PCE)-h+1)

# Create seasonal dummy variables
M <- seasonaldummy(y)
XREG <- model.matrix(~ t + t2 + M)

# Fit ARIMA model
model.SAR1 <- Arima(y, order = c(1, 0, 0), seasonal = c(0, 1, 1))
FIT <- y - model.SAR1$residuals

# Prepare seasonal dummy variables for forecasting
t_forecast <- (T + 1):(T + h)
t2_forecast <- t_forecast^2
M_forecast <- seasonaldummy(ts(rep(1, h), frequency = s, start = fstart))
XREG_forecast <- model.matrix(~ t_forecast + t2_forecast + M_forecast)

# Forecast
Forecast <- forecast(model.SAR1, h = h, xreg = XREG_forecast)
Forecast

# Un-log the forecast
Forecast$mean <- exp(Forecast$mean)
Forecast$upper <- exp(Forecast$upper)
Forecast$lower <- exp(Forecast$lower)
Forecast$x <- exp(Forecast$x)
Forecast

# Plot forecast and fitted values
plot(Forecast, col="royalblue", main = " ", xaxt = "n", lwd = 2)
# lines(exp(FIT), col = "slateblue2", lty = 1, lwd = 1)

model_q <- lm(PCE_data ~ t + t2) # Run a quadratic regression
q_trend <- PCE_data - model_q$residuals
lines(q_trend, col = "firebrick1", lwd = 1)

years <- seq(2002, 2030, by = 1)
positions <- seq(2002, 2030, by = 1)
axis(1, at = positions, labels = years[1:length(positions)], mgp = c(3, 1, 0)) 
abline(v = positions, col = "lightgray", lty = 3)

# legend("topleft", legend = c("Actual PCE", "Fitted", "Quadratic Trend", "Prediction 95%", "Forecast Mean"), col = c("black", "slateblue2", "firebrick1", "firebrick3", "steelblue1"), lty = c(1, 1, 1, 1), lwd = c(2, 1, 1, 1), bty = "n")

legend("topleft", legend = c("Actual PCE", "Quadratic Trend", "Prediction 95%", "Forecast Mean"),
       col = c("royalblue", "firebrick1", "firebrick3", "steelblue1"), lty = c(1, 1, 1, 1),
       lwd = c(2, 1, 1, 1), bty = "n")

tf <- (T + 1):(T + h)
tf <- data.frame(t = tf, t2 = tf^2)
prediction <- predict(model_q, newdata = tf, interval = "predict") # interval = "predict" is 95% confidence interval
prediction <- ts(prediction, start = fstart, frequency = s)

# Plot the forecast based on your chosen model
lines(prediction[, 1], col = "firebrick4")
lines(prediction[, 2], col = "firebrick4", lty = 2)
lines(prediction[, 3], col = "firebrick4", lty = 2)

mtext("Forecast Real Personal Consumption Expenditures", side = 3, line = 0.3, cex = 1)
mtext("Year", side = 1, line = 2.5, cex = 0.7)
mtext("Billions of Chained 2017 Dollars", side = 2, line = 2.5, cex = 0.7)

