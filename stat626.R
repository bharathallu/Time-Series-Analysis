# This script will test the possibility of fitting a model for both retail and ip

# Clearing
rm(list=ls())

# Accessing necessary libraries
library(astsa)
library(forecast)
library(fGarch)

# Reading in data
data = read.csv(file="C:/Users/jayabharath/Dropbox/Stat 626 Project/seasonal_full.csv", header=TRUE, sep=",")

# Initializing necessary variables
retail=ts(data[,11], frequency = 12, start=c(1992,1), end=c(2016, 3))
ip=ts(data[,6], frequency = 12, start=c(1992,1), end=c(2016,3))
cpi=ts(data[,7], frequency = 12, start=c(1992,1), end=c(2016,3))
ur=ts(data[,10], frequency = 12, start=c(1992,1), end=c(2016, 3))
retail=ts(data[,11], frequency = 12, start=c(1992,1), end=c(2016, 3))
# Setting directory (ONLY NECESSARY IF PRODUCING PDF OUTPUTS)
setwd("/Users/jonandr01/Dropbox/Stat 626 Project/code/ipfcast_pdfs")

# Detrending retail data
pdf('retail.pdf')
plot(retail, main="Retail Sales Data (January 1992 - March 2016)", ylab='U.S. Dollars')
dev.off()

# Taking first difference
dretail=diff(retail)
pdf('dretail.pdf')
plot(dretail, main='Retail Sales Data, First Difference', ylab='U.S. Dollars') # Data appears stationary
dev.off()

# Checking the ACF and PACF of retail
pdf('dretail_acf_pacf.pdf')
par(mfrow=c(2,1))
acf(dretail, main='ACF of Retail Sales Data, First Difference' ) # Close shop!
pacf(dretail, main='PACF of Retail Sales Data, First Difference')
dev.off()

# Detrending ip data
ip=na.omit(ip)
pdf('ip.pdf')
plot(ip, main='Industrial Production, (May 1992 - March 2016)', ylab='Index (2012=100)')
dev.off()

# Taking first difference
dip=diff(ip)
pdf('dip.pdf')
plot(dip, main='Industrial Production, First Difference', ylab=NULL)
dur=diff(diff(sqrt(ur)))
dev.off()

# Taking second difference
ddip=diff(dip)
pdf('ddip.pdf')
par(mfrow=c(2,1))
plot(ddip, main='Industrial Production, Second Difference', ylab='Change in Index')
plot(diff(diff(log(ip))), main='Industrial Production, Second Difference Log Transform', ylab = NULL) # We decided on ddip instead of the B2log(ip) transformation
dev.off()

# Checking the ACF and PACF
pdf('ddip_acf_pacf.pdf')
par(mfrow=c(2,1))
acf(ddip, main='ACF of IP, Second Difference')
pacf(ddip, main='PACF of IP, Second Difference')
dev.off()

# Checking scatterplot matrix
pdf('ddip_scatter.pdf')
lag1.plot(ddip,4)
dev.off()

# From the ACF, PACF, and the scatterplot matrix, we determine that our best models are AR(1), AR(2), AR(3), and MA(1)
sarima(ddip,1,0,0, no.constant=TRUE)
sarima(ddip,2,0,0, no.constant=TRUE)
sarima(ddip,3,0,0, no.constant=TRUE)
sarima(ddip,0,0,1, no.constant=TRUE)

# The psi weights do not imply identical models, so we merge to form ARMA models
sarima(ddip,1,0,1, no.constant=TRUE)
pdf('ddip_residuals.pdf')
sarima(ddip,2,0,1, no.constant=TRUE)
dev.off()
sarima(ddip,3,0,1, no.constant=TRUE)

# Holding to perform ljung-box tests
ar1=sarima(ddip,1,0,0, no.constant=TRUE)
ar2=sarima(ddip,2,0,0, no.constant=TRUE)
ar3=sarima(ddip,3,0,0, no.constant=TRUE)
ma1=sarima(ddip,0,0,1, no.constant=TRUE)
arma11=sarima(ddip,1,0,1, no.constant=TRUE) # Second Best
arma21=sarima(ddip,2,0,1, no.constant=TRUE) # Best Model
arma31=sarima(ddip,3,0,1, no.constant=TRUE)

# Checking to see this residuals of an ARMA(2,1) and ARMA(1,1)
Box.test(resid(arma21$fit), type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
Box.test(resid(arma11$fit), type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)

# Checking square of residuals of  ARMA(2,1) and ARMA(1,1)
# For ARMA(1,1)
square11 = resid(arma11$fit)^2
plot(square11, type = "l")
acf(square11, main = "ACF of ARMA(1,1) Squared Residuals") # Implies GARCH(1,0) or GARCH(1,1)
pacf(square11, main = "PACF of ARMA(1,1) Squared Residuals")

# For ARMA(2,1)
square21 = resid(arma21$fit)^2
plot(square21, type = "l")
acf(square21, main = "ACF of ARMA(2,1) Squared Residuals") # Implies GARCH(1,0) or GARCH(1,1)
pacf(square21, main = "PACF of ARMA(2,1) Squared Residuals")

# Our analysis shows that the garch fit for an ARMA(2,1) process hurts our model
# Thus we shall perform a garch fit for only and ARMA(1,1)
# Modeling GARCH(1,0)
summary(arma11.g10 <- garchFit(~arma(1,1) + garch(1,0), ddip, include.mean = F)) 
summary(arma11.g20 <- garchFit(~arma(1,1) + garch(2,0), ddip, include.mean = F)) 
plot(arma11.g10@residuals, type ="l") 
acf(arma11.g10@residuals) 

# Before and after of residuals. The residuals don't appear to be any different.
par(mfrow = c(2,1))
plot(resid(arma11$fit)[1:50], type = "l")
plot(arma11.g10@residuals[1:50], type ="l") 
dev.off()

# Preparing forecast
part1 = ip[1 : as.integer(length(ip)*.9)]
part2 = ip[as.integer(length(ip)*.9) + 1 : length(ip)]
part1a = ddip[1 : as.integer(length(ddip)*.9)]
part2b = ddip[as.integer(length(ddip)*.9) + 1 : length(ddip)]

f5 = sarima.for(part1a, n.ahead = 29, 2, 0, 1)

pref2 = garchFit(~arma(1,1) + garch(1,0), part1a, include.mean = F)
f2 = predict(pref2)


#exponential smoothing

lag1.plot(ddip,10)
ip.esm=ses(ip, h=12, alpha=.8, initial="simple")# forecasting using exponential smoothening
summary(ip.esm)
ip.esm1=ses(ip, h=12, alpha=.9, initial="simple")
summary(ip.esm1)



plot(ip.esm1)

# VAR with GDP

data1=cbind(ip[5:281],oils[5:281])
library(MTS)
Eccm(data1,maxp=7,maxq=7)#indicate VARMA(3,0)
VARselect(data1,lag.max=5,type="both")#Indicates VAR(3)
ipvar=VAR(data1,p=3,type="both")
ipvar1=VARMA(data1,p=3,q=0)
summary(ipvar1)


# VAR with unemployment rate.
data3=cbind(ddip[5:281],dur[5:281],doils[5:281])
Eccm(data3,maxp=7,maxq=7)
VARselect(data3,lag.max=5,type="both")#Indicates VAR(4)
ipvar=VAR(data3,p=2,type="both")
ipvar1=VARMA(data3,p=2,q=1)
summary(ipvar1)
acf(resid(ipvar))


#not relevant
doils1=lag(doils,3)
data2=cbind(ddip[9:277],doils1[9:277])
Eccm(data2,maxp=7,maxq=7)
VARselect(data2,lag.max=5,type="both")

#diagnostics
acf(resid(ipvar1),12)


# co integration test

ll=cbind(ip,ur,retail)
h1<-ca.jo(ll,type="trace",ecdet="none",K=2)

