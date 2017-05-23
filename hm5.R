#3.31
plot(gnp)
acf2(gnp)
gnpd=diff(log(gnp))
gnpar1=sarima(gnpd,1,0,0)#decreasing variance in residuals(GARCH analysis is necessary)
Box.test(resid(gnpar1$fit), type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
#3.32
plot(oil)
doil=diff(oil)
plot(doil)
acf2(doil)
dloil=diff(log(oil))
plot(dloil)
acf2(dloil)
sarima(dloil,1,0,1)
ar6=sarima(dloil,1,0,1)
Box.test(resid(ar6$fit), type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
sarima(dloil,2,0,2)
ar7=sarima(dloil,2,0,2)
Box.test(resid(ar7$fit), type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
sarima(dloil,3,0,2)
sarima(dloil,3,0,3)
sarima(dloil,2,0,1)
#ARMA(1,1) will be the best model based on acf,pacf,AIC,BIC estimates.

#3.36
phi=c(rep(0,11),.8)
ACF=ARMAacf(ar=phi,ma=.5,50)[-1]
PACF=ARMAacf(ar=phi,ma=.5,50,pacf = T)
par(mfrow=c(1,2))
plot(ACF,type="h",main="ACF");abline(h=0)
plot(PACF,type="h",main="PACF");abline(h=0)

#3.39
plot(jj)
ljj=log(jj)
plot(ljj)
acf2(ljj)
dljj=diff(ljj)
sdljj=diff(dljj,4)
plot(sdljj)
plot(dljj)
acf2(dljj)
acf2(sdljj)
sarima(sdljj,3,0,3,1,0,0,4)
sarima(sdljj,1,0,1,1,0,0,4)#ARIMA(1,1,1)(1,1,0).
sarima(sdljj,0,0,1,0,0,1,4)
ar8=sarima(sdljj,1,0,1,1,0,0,4)
Box.test(resid(ar8$fit), type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)