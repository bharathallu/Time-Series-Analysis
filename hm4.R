#3.20
ar2 = arima.sim(list(order=c(1,0,1), ar=c(.9),ma=c(-.9)), n = 500)#simulating 500 observations
plot(ar2)
acf2(ar2)
arima(ar2,order=c(1,0,1))#fitting ARMA(1,1)
#when you fit arma(1,1) with equal coefficients it is equivalent to white noise.
#3.10
(reg=ar.ols(cmort,order=2,demean=F, intercept = T))#fittting AR2 model.
predict(reg,n.ahead = 4)#forecasting 4 weeks ahead

#3.18
(ar=ar.ols(cmort,order=2))#fittting AR2 model.
(yw=ar.yw(cmort,order=2))#fitting yule walker
(ar$asy.se.coef)#standard error from ar fitting
(yw$asy.se.coef)#returns null value.
(sqrt(diag(yw$asy.var.coef)))#asymptotic standard error from yule walker.

set.seed(666)
x=arima.sim(list(order=c(0,1,1),ma=-.8),n=100)
