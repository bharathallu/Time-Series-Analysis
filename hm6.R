#2.8

library(astsa)
plot(varve,main="before log transformation")
length(varve)
par(mfrow=c(2,1))
var1=varve[0:317]
plot(var1,type="l",main="variance=133.457")
var(var1)#variance of first half
var2=varve[318:634]
plot(var2,type="l",main="variance=594.04")
var(var2)#variance of second half
yt=log(varve)
plot(yt,type="l",main="after log transformation")
yt1=yt[0:317]
var(yt1)
plot(yt1,type="l",main="variance=.270")
yt2=yt[318:634]
var(yt2)
plot(yt2,type="l",main="variance=.451")
par(mfrow=c(3,1))
hist(varve)
hist(yt)
par(mfrow=c(1,1))
plot(yt)#250 -350 is similar to global temp trend
acf(yt)#exponentially going to zero
ut=diff(yt)
plot(ut,type="l")
(acf(ut))
(var(ut))

#5.6
par(mfrow=c(1,1))
gnp
length(gnp)
plot(gnp)
gnpgr=diff(log(gnp))
plot(gnpgr)
sarima(gnpgr,1,0,0)
ar1=sarima(gnpgr,1,0,0)
ar2=sarima(gnpgr,0,0,2)
ar3=sarima(gnpgr,1,0,2)
r5=residuals(ar3$fit)
r25=r5*r5
acf2(r25)
r=residuals(ar1$fit)
r1=residuals(ar2$fit)
r12=r1*r1
r2=r*r
acf2(r2)
acf2(r12)
library(fGarch)
summary(garchFit(~arma(1,0)+garch(1,0),gnpgr))
summary(garchFit(~arma(0,2)+garch(1,1),gnpgr))
summary(garchFit(~arma(1,2)+garch(1,1),gnpgr))
(fit2=(garchFit(~arma(1,2)+garch(1,1),gnpgr)))


#5.11
param(mfrow=c(2,1))
plot(sales,type="l")
st=diff(sales)
plot(lead,type="l")
lt=diff(lag(lead,3))
reg=lm(formula=st~lt)#regression as shown in the problem.
re=resid(reg)
acf2(re)#plot indicates ARMA(1,1) to residuals.
(fit=arima(st,order=c(1,0,1),xreg=lt))# fitting ARMA(1,1) to model after regression
res=resid(fit)
acf2(res)#residulas look like white noise
plot(res,type="l")

#5.13
param(mfrow=c(2,1))
plot(climhyd$Precip,type="l")
pt=sqrt(climhyd$Precip)
plot(pt,type="l")
sarima(pt,0,0,0,0,1,1,12)
plot(climhyd$Inflow,type="l")
It=log(climhyd$Inflow)
plot(It,type="l")
sarima(It,0,0,0,0,1,1,12)

#5.15


set.seed=1000
ut=econ5$unemp
gt=econ5$gnp
ct=econ5$consum
utl=log(ut)
plot(utl,type="l")
utreg=lm(formula = utl~time(ut))
x1t=residuals(utreg)
gtl=log(gt)
gtreg=lm(formula = gtl~time(gt))
x2t=residuals(gtreg)
ctl=log(ct)
ctreg=lm(formula=ctl~time(ct))
x3t=residuals(ctreg)
data<-cbind(x1t,x2t,x3t)
library(mts)
Eccm(data,maxp=2,maxq=2)
xt=VARMA(data,p=2,q=1)
VARselect(data,lag.max=5,type="both")
r31=residuals(xt)

#part-II
set.seed(500)
w=rnorm(100,0,1)
xt1=arima.sim(n=100,list(order=c(1,0,0),ar=.5))
xt2=arima.sim(n=100,list(order=c(12,0,0),ar=c(rep(0,11),.9)))
yt1=rep(0,100)
yt2=rep(0,100)
for(i in 1:100) {
  yt1[i]=xt1[i]+sum(w[1:i])
  yt2[i]=xt2[i]+5*sum(w[1:i])
}

zt=5*yt1-yt2
plot(xt1,type="l")
plot(xt2, type="l")
par(mfrow=c(3,1))
plot(yt1,type="l")
plot(yt2,type="l")
plot(zt,type="l")
(ccf(yt1,yt2))
acf2(zt)

w=seq(-.5,.5,by=.1)
xy=2*(1-cos(2*pi*w))
set.seed(1200)
ar2=arma=arima.sim(list(order=c(1,0,1),ar=c(.8),ma=c(2)),n=100)
                   
