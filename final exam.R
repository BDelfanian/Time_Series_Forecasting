### Exercise 1###

Fund=read.csv("8 Funds.csv",header=TRUE)
Fund

time=as.Date(Fund$Date,format="%m/%d/%Y")
time

Fundnew=Fund[,-1]
Fundnew

library(xts)
Fund.xts=xts(Fundnew,order.by=time)
Fund.xts
colnames(Fund.xts)

Fund2=Fund.xts[,2]
Fund2
summary(Fund2)
class(Fund2)
dim(Fund2)
colnames(Fund2)
head(Fund2)
tail(Fund2)
start(Fund2)
end(Fund2)
length(Fund2)

plot(Fund2,main="Fund2",ylab="Price",xlab="Date")

library(tseries)
par(mfrow=c(1,2))
acf(Fund2,lag.max=40,type="correlation")
pacf(Fund2,lag.max=40)
adf.test(Fund2)

return=diff(log(Fund2))
return
return=na.omit(return)
return
plot(return,main="Returns of Fund2",ylab="return",xlab="Date")
adf.test(return)
par(mfrow=c(1,2))
acf(return,lag.max=40,type="correlation")
pacf(return,lag.max=40)

model1=arima(return,order=c(0,0,1))
model1
model2=arima(return,order=c(1,0,0))
model2
model3=arima(return,order=c(0,0,2))
model3
model4=arima(return,order=c(1,0,2))
model4
AIC(model1,model2,model3,model4)

##### answers #####
##### the original price series is not stationary, but the returns are:
Price Series (Fund2): the (ADF) test yielded a p-value of 0.6281 (> 0.05). We cannot reject the null hypothesis of a unit root, it is non-stationary.
Return Series (return): after taking the log-difference, the ADF test yielded a p-value of 0.01 (< 0.05). We reject the null hypothesis, it is stationary.
Since differencing the data once produces a stationary series (the return), the original process is integrated of order 1 (random walk)
The ARMA(1,2) model is the best because it has the lowest AIC (-20202.00)
The plots of ACF and PACF confirm stationarity and mostly stay within the significance bounds####

#### Exercise 2 #####

MSFT=read.csv("MSFT.csv",header=TRUE)
MSFT
time=as.Date(MSFT$Date,format="%m/%d/%Y")
time
MSFTnew=MSFT[,-1]
MSFTnew
library(xts)
MSFT.xts=xts(MSFTnew,time)
MSFT.xts

MSFT.ts=ts(MSFT[,2],start=c(2010,1),end=c(2019,12),frequency=12)
MSFT.ts
library(zoo)
MSFT_MA=rollmean(MSFT.ts,10)
MSFT_MA


plot(MSFT.ts,main="Original vs MA",ylab="Ad.C.P.",xlab="Time",col ="red",lwd=2)
lines(MSFT_MA,col="blue",lwd=2)
legend("topleft",legend=c("Original","MA"),col=c("red","blue"),lty=1,lwd=2)

dec=decompose(MSFT.ts,type="additive")
dec
plot(dec)

ad_dec=MSFT.ts-dec$seasonal
plot(MSFT.ts,main="Original vs Ad.S",ylab="Price",xlab="Time",col="red",lwd=2)
lines(ad_dec,col="blue",lwd=2)
legend("topleft",legend=c("Original","Adjusted"),col=c("red","blue"),lty=1,lwd=2)

HW=HoltWinters(MSFT.ts,seasonal="additive")
HW
plot(HW,main="HoltWinters")

prediction=predict(HW,n.ahead=10,prediction.interval=T,level=0.95)
prediction
plot(prediction,main="MSFT prediction using HoltWinters")

library(forecast)
forecast=forecast(HW,h=10,level=c(0.80,0.95))
forecast
plot(forecast,main="MSFT forecast using HoltWinters")

residuals=forecast$residuals
residuals
residuals=na.omit(residuals)
residuals
plot(residuals,main="Residuals",ylab="Residuals",xlab="Time")
abline(h = 0, col = "red", lty = 2)

par(mfrow=c(1,2))
acf(residuals)
pacf(residuals)
Box.test(residuals,lag=20,type="Ljung-Box")
library(tseries)
shapiro.test(residuals)
jarque.bera.test(residuals)
ks.test(residuals,"pnorm",mean=mean(residuals),sd=sd(residuals))

#### Answers: ACF & PACF Plot: The residuals show no significant autocorrelation, as almost all lags fall within the significance bounds
Ljung-Box Test: With a p-value of 0.1236 (> 0.05), we fail to reject the null hypothesis of no autocorrelation.(white noise)
the residuals are not perfectly normal (Shapiro test  # jurqe bera < 0.05)###

###### Exercise 3 #####

library(readxl)
data=read_excel("c7ex5.xls")
data
ser1.ts=ts(data$X1,start=1,frequency=1)
ser1
ser2.ts=ts(data$X2,start=1,frequency=1)
ser2
ser3.ts=ts(data$X3,start=1,frequency=1)
ser3

par(mfrow = c(3,1))
plot(ser1.ts,main="ser1",ylab="value",col="blue",lwd=2)
plot(ser2.ts,main="ser2",ylab="value",col="red",lwd=2)
plot(ser3.ts,main="ser3",ylab="value",col="green",lwd=2)

summary(ser1.ts)
summary(ser2.ts)
summary(ser3.ts)


par(mfrow=c(1,2))
acf(ser1.ts,lag.max=40,type="correlation")
pacf(ser1.ts,lag.max=40)
adf.test(ser1.ts)
pp.test(ser1.ts)
kpss.test(ser1.ts)

par(mfrow=c(1,2))
acf(ser2.ts,lag.max=40,type="correlation")
pacf(ser2.ts,lag.max=40)
adf.test(ser2.ts)
pp.test(ser2.ts)
kpss.test(ser2.ts)

par(mfrow=c(1,2))
acf(ser3,lag.max=40,type="correlation")
pacf(ser3,lag.max=40)
adf.test(ser3.ts)
pp.test(ser3.ts)
kpss.test(ser3.ts)

##### all 3 series are non-stationary and correlation: 
ACF stays positive & pacf has a few significant spekes
adf & pp p-value > 0.05 - kpss p-value < 0.05
we should keep going with return and difference####


return1=diff(ser1.ts)
return1
return2=diff(ser2.ts)
return2
return3=diff(ser3.ts)
return3

adf.test(return1)
pp.test(return1)
kpss.test(return1)

adf.test(return2)
pp.test(return2)
kpss.test(return2)

adf.test(return3)
pp.test(return3)
kpss.test(return3)

par(mfrow=c(3,2))
acf(return1,lag.max=40,type="correlation")
pacf(return1,lag.max=40)
acf(return2,lag.max=40,type="correlation")
pacf(return2,lag.max=40)
acf(return3,lag.max=40,type="correlation")
pacf(return3,lag.max=40)

#### or we can do it: returns_total=cbind(return1,return2,return3)
apply(returns_total,2,Box.test,lag=20,type="Ljung-Box")####


model1=auto.arima(return1)
model1

model2=auto.arima(return2)
model2

model3=auto.arima(return3)
model3

tsdiag(model1)
checkresiduals(model1)

tsdiag(model2)
checkresiduals(model2)

tsdiag(model3)
checkresiduals(model3)

res1=residuals(model1)
res1
par(mfrow=c(1,2))
acf(res1,lag.max=40,type="correlation")
pacf(res1,lag.max=40)
Box.test(res1,lag=20,type="Ljung-Box")

res2=residuals(model2)
res2
par(mfrow=c(1,2))
acf(res2,lag.max=40,type="correlation")
pacf(res2,lag.max=40)
Box.test(res2,lag=20,type="Ljung-Box")

res3=residuals(model3)
res3
par(mfrow=c(1,2))
acf(res3,lag.max=40,type="correlation")
pacf(res3,lag.max=40)
Box.test(res3,lag=20,type="Ljung-Box")

shapiro.test(res1)
ks.test(res1, "pnorm", mean = mean(res1), sd = sd(res1))

shapiro.test(res2)
ks.test(res2, "pnorm", mean = mean(res2), sd = sd(res2))

shapiro.test(res3)
ks.test(res3, "pnorm", mean = mean(res3), sd = sd(res3))


######All 3 series are stationary. Using auto.arima on series:
we obtain low-order ARIMA(p,1,q) models:
ARIMA(0,1,2), ARIMA(1,1,1) and ARIMA(3,1,1) for the series) 
The ACF/PACF of the residuals show no significant autocorrelation
the Ljung–Box tests on residuals (lag 20) have large p-values (> 0.5).(white noise)####

forecast1=forecast(model1,h=5)
forecast1
plot(forecast1,main="Forecast for Series1")

forecast2=forecast(model2,h=5)
forecast2
plot(forecast2,main="Forecast for Series2")

forecast3=forecast(model3,h=5)
forecast3
plot(forecast3,main="Forecast for Series3")

###### we then computed 5-step-ahead forecasts for each return series using a forecasting function
obtaining point forecasts and 95% prediction intervals for the next 5 period
