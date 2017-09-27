# Andrea Bruckner
# Predict 413: Final
# Forecasting monthly rainfall

## QUESTIONS ##

# 1.) Is there an easy way to see where the -9999 values exist? I knew entire decades or consecutive years were missing from using Excel. 
# 2.) Is there an easy way to see if any dates from the subset excluding -9999 values are missing?
# I confirmed that all the dates were present using http://www.timeanddate.com/date/durationresult.html?m1=01&d1=01&y1=1984&m2=05&d2=31&y2=1995&ti=on

# Load libraries
install.packages("fpp")
library(fpp) # fpp package allows xreg
install.packages("forecast")
library(forecast)
install.packages("TTR")
library(TTR)
install.packages("car")
library(car)
install.packages("MASS")
library(MASS)
install.packages("lubridate")
library(lubridate)
install.packages("fma")
library(fma)
install.packages("party")
library(party)
install.packages("randomForest")
library(randomForest)
install.packages("dyn")
library(dyn)


# Disable scientific notation for labeling plots
options(scipen = 10)

# Read in file
final <- read.csv(file.path("/Users/annie/Desktop/Northwestern/PREDICT_413/FinalExam","data.csv"),sep=",")

# Get overview of data
summary(final) 
str(final)
head(final)
class(final)
names(final)
nrow(final) # 24806

# Find missing values
sum(final$PRCP == -9999) # 0
sum(final$ACMH == -9999) # 13698
sum(final$AWND == -9999) # 13636

# Percent missing values
13698/24806 # = 0.5522051 for ACMH
13636/24806 # = 0.5497057 for AWND

### Create Data Sets ###

## 1 -- justrain ##

# Create new data frame with just rain data
justrain <- data.frame(final$DATE, final$PRCP)
head(justrain)

# Shorten variable names for justrain
justrain$date <- justrain$final.DATE
justrain$rain <- justrain$final.PRCP

# Remove duplicate columns for justrain
justrain$final.DATE <- NULL
justrain$final.PRCP <- NULL

head(justrain)

# Convert date to date class for justrain
justrain$date <- as.Date(as.character(justrain$date), "%Y%m%d")
head(justrain) # starts 1946-09-01
tail(justrain) # ends 2014-07-31
str(justrain) # confirms it's a date format
nrow(justrain) # 24806
# This confirms that no dates are missing from justrain: http://www.timeanddate.com/date/durationresult.html?m1=09&d1=01&y1=1946&m2=07&d2=31&y2=2014&ti=on

# Sort by the date format for justrain
justrain <- justrain[order(as.Date(justrain$date,format="%Y-%m-%d")),]
head(justrain)

# Renumber rows just in case any dates were out of order
rownames(justrain) <- NULL

# Create year and month variables using lubridate for justrain
justrain$year <- year(justrain$date)
justrain$month <- month(justrain$date)
head(justrain) # confirms this worked
tail(justrain) # confirms this worked

## 2 -- rain, clouds, wind ##

# Create new data frame with rain, clouds, and wind
weather <- data.frame(final$DATE, final$PRCP, final$ACMH, final$AWND)
head(weather)

# Shorten variable names
weather$date <- weather$final.DATE
weather$rain <- weather$final.PRCP
weather$clouds <- weather$final.ACMH
weather$wind <- weather$final.AWND

head(weather)

# Remove duplicate columns
weather$final.DATE <- NULL
weather$final.PRCP <- NULL
weather$final.ACMH <- NULL
weather$final.AWND <- NULL

head(weather)

# Subset data to exclude all -9999 values
weathersub <- subset(weather, clouds != -9999 & wind != -9999) # using | instead of & would leave in some -9999 values
weathersub
nrow(weathersub) # 4169 rows of an original 24806 rows
4169/24806 # 0.1680642 percent of original data set remains

# Get overview of subsetted data
summary(weathersub) # wind = 3263.700 is an outlier
str(weathersub)
head(weathersub)

## Wind Outlier Fixing Process ##

# Subset data to exclude outlier to calculate mean for rest of wind values
windsub <- subset(weathersub, wind != 3263.7)
head(windsub)
nrow(windsub) # should be 4168 - yep!
mean(windsub$wind) # 7.712956 -- will impute 7.7 to be consistent with number of decimal places for rest of the data

# Identify which row the outlier is in
which(weathersub$wind == 3263.7) # row 865, date = 19860514
weathersub[865,] # confirms it's in the 865th row (all the row names are still the original row numbers, so it's confusing)

# Renumber rows
rownames(weathersub) <- NULL
head(weathersub) # confirms numbering worked

# Impute the new value
weathersub[865,4] = 7.7 # Here I'm selecting the 865th row, 4th column since wind is the 4th column
weathersub[865,] # confirms the value has been imputed

# Convert date to date class
weathersub$date <- as.Date(as.character(weathersub$date), "%Y%m%d")
head(weathersub) # starts 1984-01-01
tail(weathersub) # ends 1995-05-31
str(weathersub) # confirms it's a date format

# Sort by the date format
weathersub<-weathersub[order(as.Date(weathersub$date,format="%Y-%m-%d")),]
head(weathersub)

# Renumber rows just in case any dates were out of order
rownames(weathersub) <- NULL

# Create year and month variables using lubridate
weathersub$year <- year(weathersub$date)
weathersub$month <- month(weathersub$date)
head(weathersub) # confirms this worked
tail(weathersub) # confirms this worked

### Create Time Series ###

## 1 -- justrain ##

# Remove date column
justrain2 <- justrain
justrain2$date <- NULL

# Create ts
justrainTS<-ts(justrain2, start=c(1946,9,1), frequency = 7)
head(justrainTS)

# Aggregate rainfall by year and month
shortdate <- strftime(justrain$date, "%Y-%m")
justrainagg <- aggregate(rain ~ shortdate, FUN = sum, data = justrain)
head(justrainagg)

justrainagg2 <- aggregate(rain ~ shortdate, FUN = sum, data = justrain)
head(justrainagg2)

# Remove shortdate column
justrainagg$shortdate <- NULL
head(justrainagg)

# Create ts for aggregated justrain data
justrainaggTS<-ts(justrainagg, start=c(1946,9), frequency = 12)
head(justrainaggTS)
justrainaggTS

# A log transformation is not a good idea because it creates negative values--can't have negative rainfall
#log_justrainagg <- log(justrainagg)
#log_justrainaggTS<-ts(log_justrainagg, start=c(1946,9), frequency = 12)
#head(log_justrainaggTS)
#log_justrainaggTS
#plot(log_justrainaggTS)

## 2 -- rain, clouds, wind ##
weathersub2 <- weathersub
weathersub2$date <- NULL
weatherTS<-ts(weathersub2, start=c(1984,1,1), frequency = 7)
head(weatherTS)
usenames<-c("date","rain","clouds","wind")
colnames(weatherTS)<-usenames
head(weatherTS)

## Create Plots and Models ##

par(mfrow=c(1,1))

# Create plot for justrainaggTS
plot(justrainaggTS, main="Monthly Precipitation \n (September 1946 to July 2014)",
     xlab="Year", ylab="Rainfall (inches)")

par(mfrow=c(1,2))
# Season Plot -- reveals bell shape
seasonplot(justrainaggTS, ylab="Rainfall (inches)", xlab="Month", 
           main="", year.labels=FALSE, year.labels.left=FALSE, col=1:20, pch=20)

# Month Plot -- reveals the biggest variation occurs in July and August, the peak months
monthplot(justrainaggTS, ylab="Rainfall (inches)",xlab="Month",xaxt="n",
          main="")
axis(1,at=1:12,labels=month.abb,cex=0.8)

# Decompose justrainaggTS
justrainaggTScomponents <- decompose(justrainaggTS)

justrainaggTScomponents$seasonal
# May, June, September, and October have positive values--the other months have negative values
# June has the highest value, December has the lowest value
justrainaggTScomponents$trend
justrainaggTScomponents$random

plot(justrainaggTScomponents)

plot(justrainaggTScomponents$figure)
abline(0,0, col="gray")
# Anything above or below 1 shows seasonality--everything is seasonal--OR IS IT ABOVE OR BELOW 0, NOT 1?

# Simple forecasts
h <- 24 # To forecast next 2 years of data (2015-2016)
meanf(justrainaggTS, h) 
naive(justrainaggTS, h) 
snaive(justrainaggTS, h)
rwf(justrainaggTS, h)
rwf(justrainaggTS, h, drift=TRUE)

justrainaggTS2 <- window(justrainaggTS,start=c(1946,9),end=c(2014,7))
fit1 <- meanf(justrainaggTS2, h)
fit2 <- naive(justrainaggTS2, h)
fit3 <- snaive(justrainaggTS2, h)
fit4 <- rwf(justrainaggTS2, h, drift=TRUE)

plot(fit1, plot.conf=FALSE, 
     main="Monthly Precipitation Forecasts\n (August 2014 to July 2016)",
     xlab="Year", ylab="Precipitation (inches)")
lines(fit2$mean,col=2)
lines(fit3$mean,col=3)
lines(fit4$mean,col=8)
legend("topright",lty=1,col=c(4,2,3,8),
       legend=c("Mean method","Naive method","Seasonal naive method", "Drift method"))

# Create Test Train Sets
train <- ts(justrainaggTS[1:610],frequency=12, start=c(1946,9)) # represents Sept 1946 to June 1997 data
train
test <- ts(justrainaggTS[611:815],frequency=12, start=c(1997,7)) # represents July 1997 to July 2014 data
test

# Test for Stationary
adf.test(justrainaggTS, alternative = "stationary")
kpss.test(justrainaggTS)

justrainaggTS.dif <- diff(justrainaggTS)

par(mfrow=c(2,1))
acf(justrainaggTS)
acf(justrainaggTS.dif)

par(mfrow=c(2,1))
plot(justrainaggTS, xlab="Year", ylab="Precipitation (inches)")
plot(justrainaggTS.dif, xlab="Year", ylab="Precipitation (inches)")


# MODEL: Multiple Linear Regression Model
fitLM <- tslm(train~trend+season)
summary(fitLM)
par(mfrow=c(1,1))
lmf <- forecast(fitLM, h=205, level=c(80,90)) # forecast entire test set
plot(lmf, fcol = "deepskyblue", flwd = 3, main = "Multiple Linear Regression Precipitation Forecasts", ylab = "Rainfall (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
lmf # confidence intervals
lmacc <- accuracy(lmf,test)
lmacc # accuracy statistics

# Linear Regression Residuals
par(mfrow=c(1,2))
residlm <- residuals(fitLM)
plot(residlm, main="Multiple Linear Regression \n Residuals", type="p")
abline(0,0, col="gray")
hist(residlm, main="Histogram of Residuals")

# MODEL: Seasonal Naive Model -- can use stlm from fpp package
fitSnaive <- snaive(train, h=205, level=c(80,90))
fitSnaive

snf <- forecast(fitSnaive, h=205) # forecast entire test set
par(mfrow=c(1,1))
plot(snf, fcol = "deepskyblue", flwd = 3, main = "Seasonal Naive Precipitation Forecasts", ylab = "Rainfall (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
snf # confidence intervals
acc <- accuracy(snf,test)
acc
snacc <- accuracy(fitSnaive)  # accuracy for snaive
snacc

# Seasonal Naive Residuals
par(mfrow=c(1,2))
residsnf <- residuals(fitSnaive)
plot(residsnf2, main="Residuals", type="p")
abline(0,0, col="gray")
hist(residsnf, main="Histogram of Residuals")

# MODEL: Auto ARIMA Seasonal
ARIMAseasonal <- auto.arima(train, allowdrift=TRUE, allowmean=TRUE, seasonal=TRUE)
arimaS <- forecast(ARIMAseasonal, h=205) # forecast entire test set
par(mfrow=c(1,1))
plot(arimaS, fcol = "deepskyblue", flwd = 3, main = "Seasonal ARIMA Precipitation Forecasts", ylab = "Rainfall (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
arimaS # confidence intervals
accarimaS <- accuracy(arimaS,test)
accarimaS
arimaSacc <- accuracy(arimaS)  # accuracy
arimaSacc

# Seasonal ARIMA Residuals
par(mfrow=c(1,2))
residarimaS <- residuals(ARIMAseasonal)
plot(residarimaS, main="Seasonal Auto ARIMA \n Residuals", type="p")
abline(0,0, col="gray")
hist(residarimaS, main="Histogram of Residuals")

# MODEL: Auto ARIMA Non-Seasonal
ARIMAnotseasonal <- auto.arima(train, allowdrift=TRUE,allowmean=TRUE, seasonal=FALSE)

arimaNS <- forecast(ARIMAnotseasonal, h=205) # forecast entire test set
par(mfrow=c(1,1))
plot(arimaNS, fcol = "deepskyblue", flwd = 3, main = "Seasonal ARIMA Precipitation Forecasts", ylab = "Rainfall (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
arimaNS # confidence intervals
accarimaNS <- accuracy(arimaNS,test)
accarimaNS
arimaNSacc <- accuracy(arimaNS)  # accuracy
arimaNSacc

# Non-Seasonal ARIMA Residuals
par(mfrow=c(1,2))
residarimaNS <- residuals(ARIMAnotseasonal)
plot(residarimaNS, main="Non-Seasonal Auto ARIMA \n Residuals", type="p")
abline(0,0, col="gray")
hist(residarimaNS, main="Histogram of Residuals")

# MODEL: Simple Holt Winters -- this no longer plots correctly
HW <- holt(train, initial="simple", h=205)
HWf <- forecast(HW, h=205) # forecast entire test set
par(mfrow=c(1,1))
plot(HWf, fcol = "deepskyblue", flwd = 3, main = "Simple Holt Winters Precipitation Forecasts", ylab = "Rainfall (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
HWf # confidence intervals
accHWf <- accuracy(HW,test)
accHWf
HWfacc <- accuracy(HWf)  # accuracy
HWfacc

# Simple HW Residuals
par(mfrow=c(1,2))
residHWf <- residuals(HW)
plot(residHWf, main="Residuals", type="p")
abline(0,0, col="gray")
hist(residHWf, main="Histogram of Residuals")

# MODEL: Holt Winters Additive
HWA <- hw(train, seasonal="additive", h=205)
HWAf <- forecast(HWA, h=205) # forecast entire test set
par(mfrow=c(1,1))
plot(HWAf, fcol = "deepskyblue", flwd = 3, main = "Holt-Winters Additive Precipitation Forecasts", ylab = "Rainfall (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
HWAf # confidence intervals
accHWAf <- accuracy(HWA,test)
accHWAf
HWAfacc <- accuracy(HWAf)  # accuracy
HWAfacc

# HWA Residuals
par(mfrow=c(1,2))
residHWAf <- residuals(HWA)
plot(residHWAf, main="Holt-Winters Additive \n Residuals", type="p")
abline(0,0, col="gray")
hist(residHWAf, main="Histogram of Residuals")

# MODEL: Holt Winters Multiplicative == doesn't work
#HWM <- hw(train, seasonal="multiplicative", h=24)
#HWMf <- forecast(HWM, h=24) # forecast 2 years
#par(mfrow=c(1,1))
#plot(HWMf, fcol = "deepskyblue", flwd = 3, main = "Seasonal ARIMA Precipitation Forecasts", ylab = "Precipitation (inches)", xlab = "Year")
#lines(test,col="red", lty = 2)
#legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
#HWMf # confidence intervals
#accHWMf <- accuracy(arimaNS,test)
#accHWMf
#HWMfacc <- accuracy(HWMf)  # accuracy
#HWMfacc

# HWM Residuals
#par(mfrow=c(1,2))
#residHWMf <- residuals(HWMf)
#plot(residHWMf, main="Residuals", type="p")
#abline(0,0, col="gray")
#hist(residHWMf, main="Histogram of Residuals")


# MODEL: Neural Net
net <- nnetar(train)
netf <- forecast(net, h = 205) # forecast 2 years
par(mfrow=c(1,1))
plot(netf, fcol = "deepskyblue", flwd = 3, main = "Neural Network Precipitation Forecasts", ylab = "Rainfall (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
netf # confidence intervals
accnetf <- accuracy(netf,test)
accnetf
netfacc <- accuracy(netf)  # accuracy
netfacc

# Neural Net Residuals
par(mfrow=c(1,2))
residnetf <- residuals(net)
plot(residnetf, main="Neural Network \n Residuals", type="p")
abline(0,0, col="gray")
hist(residnetf, main="Histogram of Residuals")

# MODEL: Random Forest - I kept getting errors after hours of tinkering

# Prepare aggregated data for random forest
#justrainagg2$date <- as.factor(justrainagg2$shortdate)
#str(justrainagg2)

# Subset just last 4 years of data since RF can't handle more than 53 factor levels (had to make shortdate into a factor since RF can't read characters)
#lastfour <- justrainagg2[767:815,]
#head(lastfour)

#rainrf <- randomForest(rain ~ date, ntree=500, importance=T, data = lastfour)
#summary(rainrf)
#rainrff <- forecast(rainrf, h = 24) # forecast 2 years
#par(mfrow=c(1,1))
#plot(rainrff, fcol = "deepskyblue", flwd = 3, main = "Random Forest Precipitation Forecasts", ylab = "Precipitation (inches)", xlab = "Year")
#lines(test,col="red", lty = 2)
#legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
#rainrff # confidence intervals
#accrainrff <- accuracy(rainrf,test)
#accrainrff
#rainrffacc <- accuracy(rainrff)  # accuracy
#rainrffacc

# Random Forest Residuals
#par(mfrow=c(1,2))
#residnetf <- residuals(rainrf)
#plot(residnetf, main="Residuals", type="p")
#abline(0,0, col="gray")
#hist(rainrff, main="Histogram of Residuals")

## Four Featured Models Plots for Report

par(mfrow=c(1,2))
#par(mfrow=c(2,2))
# MODEL: Multiple Linear Regression Model
fitLM <- tslm(train~trend+season)
summary(fitLM)
lmf <- forecast(fitLM, h=205, level=c(80,90)) # forecast entire test set
plot(lmf, fcol = "deepskyblue", flwd = 3, main = "Multiple Linear Regression \n Precipitation Forecasts", ylab = "Rainfall (inches)", xlab = "Year")
lines(test,col="red", lty = 2, lwd = 0.5)
#legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))

# MODEL: Holt Winters Additive
HWA <- hw(train, seasonal="additive", h=205)
HWAf <- forecast(HWA, h=205) # forecast entire test set
plot(HWAf, fcol = "deepskyblue", flwd = 3, main = "Holt-Winters Additive \n Precipitation Forecasts", ylab = "Rainfall (inches)", xlab = "Year")
lines(test,col="red", lty = 2, lwd = 0.5)
#legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))

# MODEL: Neural Net
net <- nnetar(train)
netf <- forecast(net, h = 205) # forecast 2 years
plot(netf, fcol = "deepskyblue", flwd = 3, main = "Neural Network \n Precipitation Forecasts", ylab = "Rainfall (inches)", xlab = "Year")
lines(test,col="red", lty = 2, lwd = 0.5)
#legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))

# MODEL: Auto ARIMA Seasonal
ARIMAseasonal <- auto.arima(train, allowdrift=TRUE, allowmean=TRUE, seasonal=TRUE)
arimaS <- forecast(ARIMAseasonal, h=205) # forecast entire test set
plot(arimaS, fcol = "deepskyblue", flwd = 3, main = "Seasonal Auto ARIMA \n Precipitation Forecasts", ylab = "Rainfall (inches)", xlab = "Year")
lines(test,col="red", lty = 2, lwd = 0.5)
#legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))




## Kaggle Submissions ##

# Kaggle Submission: multipleLM01.csv -- BEST
fitLM2 <- tslm(justrainaggTS~trend+season)
summary(fitLM2)
lmf2 <- forecast(fitLM2, h=24, level=c(80,90)) # forecast 2 years
lmf2

# I rounded the point forecasts to be consistent with the format of the known data.
round(lmf2$mean, 2)

# Kaggle Submission: snaive01.csv
fitSnaive2 <- snaive(justrainaggTS, h=24, level=c(80,90))
fitSnaive2
snf2 <- forecast(fitSnaive2, h=24) # forecast 2 years
plot(snf2, fcol = "deepskyblue", flwd = 3, main = "Seasonal Naive Precipitation Forecasts", ylab = "Precipitation (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
snf2 # confidence intervals
round(snf2$mean, 2)

# Kaggle Submission: autoarima01.csv
ARIMAseasonal2 <- auto.arima(justrainaggTS, allowdrift=TRUE, allowmean=TRUE, seasonal=TRUE)
arimaS2 <- forecast(ARIMAseasonal2, h=24) # forecast 2 years
par(mfrow=c(1,1))
plot(arimaS2, fcol = "deepskyblue", flwd = 3, main = "Seasonal ARIMA Precipitation Forecasts", ylab = "Precipitation (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
arimaS2 # confidence intervals
round(arimaS2$mean, 2)

# Kaggle Submission: autoarimaNS01.csv
ARIMAnotseasonal2 <- auto.arima(justrainaggTS, allowdrift=TRUE,allowmean=TRUE, seasonal=FALSE)
arimaNS2 <- forecast(ARIMAnotseasonal2, h=24) # forecast 2 years
par(mfrow=c(1,1))
plot(arimaNS2, fcol = "deepskyblue", flwd = 3, main = "Seasonal ARIMA Precipitation Forecasts", ylab = "Precipitation (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
arimaNS2 # confidence intervals
round(arimaNS2$mean, 2)

# Kaggle Submission: hwsimple01.csv -- WORST
HW2 <- holt(justrainaggTS, initial="simple", h=24)
HWf2 <- forecast(HW2, h=24) # forecast 2 years
par(mfrow=c(1,1))
plot(HWf2, fcol = "deepskyblue", flwd = 3, main = "Simple Holt Winters Precipitation Forecasts", ylab = "Precipitation (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
HWf2 # confidence intervals
round(HWf2$mean, 2)

# Kaggle Submission: hwa01.csv -- SECOND BEST
HWA2 <- hw(justrainaggTS, seasonal="additive", h=24)
HWAf2 <- forecast(HWA2, h=24) # forecast 2 years
par(mfrow=c(1,1))
plot(HWAf2, fcol = "deepskyblue", flwd = 3, main = "Holt Winters Additive Precipitation Forecasts", ylab = "Precipitation (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
HWAf2 # confidence intervals
round(HWAf2$mean, 2)


# Kaggle Submission: nnet01.csv -- THIRD BEST
net2 <- nnetar(justrainaggTS)
netf2 <- forecast(net2, h = 24) # forecast 2 years
par(mfrow=c(1,1))
plot(netf2, fcol = "deepskyblue", flwd = 3, main = "Neural Net Precipitation Forecasts", ylab = "Precipitation (inches)", xlab = "Year")
lines(test,col="red", lty = 2)
legend("topright", lty = c(2,1), lwd = c(1,3), legend = c("Actual", "Predicted"), col = c("red","deepskyblue"))
netf2 # confidence intervals
round(netf2$mean, 2)
       