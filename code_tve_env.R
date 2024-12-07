rm(list=ls())

library(tseries)
library(evd)

setwd("~/00_Ensai/tv_extrem/projet-tve-env/")

data_temp <- read.csv("FX_SOUID115726.txt",skip = 18,header = TRUE,na.strings = -9999)
# 88997
data_clean <- data_temp[!is.na(data_temp$FX),]
data_clean$DATE <- as.character(data_clean$DATE)
data_clean$DATE <- as.Date(data_clean$DATE,format="%Y%m%d")
data_clean$FX <- data_clean$FX/10
summary(data_clean$FX)

fx <- data_clean$FX
par(mfrow=c(1,1))
ts.plot(fx)
acf(fx,lag.max = 200)
pacf(fx,lag.max = 200)
adf.test(fx)

# Approche par GPD 
?mrlplot
mrlplot(fx, main="Mean Residual Life Plot")

par(mfrow=c(1,2))
tcplot(fx, c(30,38))

fitted <- fpot(fx,32,npp=365)
fitted

par(mfrow=c(2,2))
plot(fitted)


ic_scale <- fitted$estimate["scale"] + c(-1,1) *  qnorm(1-0.05/2) * fitted$std.err["scale"]
ic_shape <- fitted$estimate["shape"] + c(-1,1) * qnorm(1-0.05/2)*fitted$std.err["shape"]
confint(fitted)


par(mfrow=c(2,1))
prof <- profile(fitted)
plot(prof)
confint(prof)


res <- function(seuil, sigma, shape, T, lambda){
  return(seuil+(sigma/shape)*((T*lambda)**(shape) -1))
}

lambda <- sum(fx>385)/length(fx)
res(385,fitted$estimate["scale"],fitted$estimate["shape"],1000,lambda)



# Approche GEV 
