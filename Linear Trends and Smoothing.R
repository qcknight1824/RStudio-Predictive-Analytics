install.packages('forecast')
install.packages("zoo")

# dfTB
dfTB <- read.csv("AustralianWines.csv",na.strings=c('NA',''))
str(dfTB)
head(dfTB)
summary(dfTB)

#delete rows with NAs
dfTB <- na.omit(dfTB)

# Convert data into time series object
library(forecast)

library(lubridate)
dfTB$Month <- parse_date_time2(dfTB$Month, "my")

# start and frequency
RedWine <- ts(dfTB$Red, start=c(1980,1),frequency = 12)
RedWine
plot(RedWine)


#-------------------------------------------------
# Linear Trend
#-------------------------------------------------

AusWine.lm <- tslm(RedWine~trend)
summary(AusWine.lm)

# Data partition using 24 months for validation set
nValid <- 24
nTrain <- length(RedWine)-nValid

train.ts <- window(RedWine,start=c(1980,1),end=c(1980,nTrain))
valid.ts <- window(RedWine,start=c(1980,nTrain+1),end=c(1980,nTrain+nValid))

train.lm <- tslm(train.ts~trend)
summary(train.lm)
train.lm.pred <- forecast(train.lm,h=nValid,level=0)

# Visualize the linear trend model
par(mfrow = c(1, 1))
plot(train.lm.pred, ylim = c(300, 4000),  ylab = "RedWine", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1980,1995),main = "", flty = 2)
axis(1, at = seq(1980, 1995, 1), labels = format(seq(1980, 1995, 1)))
lines(train.lm.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

# Evaluate model performance
accuracy(train.lm.pred,valid.ts)

# Polynomial Trend
train.lm.poly.trend <- tslm(train.ts ~ trend + I(trend^2))
summary(train.lm.poly.trend)
train.lm.poly.trend.pred <- forecast(train.lm.poly.trend, h = nValid, level = 0)
accuracy(train.lm.poly.trend.pred,valid.ts)

#-------------------------------------------------
# SEASONALITY
#-------------------------------------------------

train.lm.season <- tslm(train.ts ~ season)
summary(train.lm.season)
train.lm.season.pred <- forecast(train.lm.season, h = nValid, level = 0)
accuracy(train.lm.season.pred,valid.ts)

#-------------------------------------------------
# TREND + SEASONALITY
#-------------------------------------------------
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.lm.trend.season)
train.lm.trend.season.pred <- forecast(train.lm.trend.season, h = nValid, level = 0)
accuracy(train.lm.trend.season.pred,valid.ts)

#-------------------------------------------------
# Simple Exponential Smoothing
#-------------------------------------------------
library(zoo)
ma <- rollmean(RedWine,k=12,align="right")
summary(ma)

# difference between forecasted ma vs original data RedWine
ma
RedWine

# Calculate MAPE
MAPE = mean(abs((ma-RedWine)/RedWine),na.rm=T)


# run simple exponential smoothing
# and alpha = 0.8 to fit simple exponential smoothing.
ses <- ses(train.ts, alpha = 0.8, h=36)
autoplot(ses)
accuracy(ses,valid.ts)

# Use ses function to estimate alpha
ses1 <- ses(train.ts, alpha = NULL, h=36)
summary(ses1)
accuracy(ses1,valid.ts)
