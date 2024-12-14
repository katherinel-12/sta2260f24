library(ISLR)

forestfires <- read.csv("~/Desktop/STA 2260/forestfires.csv") 
head(forestfires)
class(forestfires)
colnames(forestfires)

plot(forestfires[, 1:13])
plot(forestfires[, 5:13])
plot(forestfires[, c(5:10)])
# Dataset: 
# https://archive.ics.uci.edu/dataset/162/forest+fires
# northeast region of Portugal
# Information: 
# https://climatedata.ca/resource/understanding-fire-weather-and-climate-change-basics/ 
# 5. FFMC - FFMC index from the FWI system: 18.7 to 96.20 
# Fine Fuel Moisture Code: "the dryness of fuels on the surface of the forest floor. This layer is most likely to catch fire due to human activity."
# 7. DC - DC index from the FWI system: 7.9 to 860.6 
# Drought Code: "fuel dryness deeper in the forest floor. The dryness of this layer factors into how hard it is to put out deep burning wildfires."
# 8. ISI - ISI index from the FWI system: 0.0 to 56.10 
# Initial Spread Index: "ISI = FFMC + Wind Speed. This index combines wind speed conditions with the surface fuel dryness (FFMC) to estimate how fast fires could spread."
# 9. temp - temperature in Celsius degrees: 2.2 to 33.30 
# 10. RH - relative humidity in %: 15.0 to 100 

# entries which have FFMC index > 38.75
ffmc_greater_than_midpoint <- which(forestfires[,5] > 38.75)
forestfires[ffmc_greater_than_midpoint, 5]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~One Predictor Models~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Lower AIC is better and higher R^2 is better

# Temp response to predictors
m1.ffmcT <- lm(temp ~ FFMC, data=forestfires)
summary(m1.ffmcT)
# Adjusted R-squared:  0.1846 

m1.isiT <- lm(temp ~ ISI, data=forestfires)
summary(m1.isiT)
# Adjusted R-squared:  0.1538 

m1.rhT <- lm(temp ~ RH, data=forestfires)
summary(m1.rhT)
# Adjusted R-squared:  0.2767

AIC(m1.ffmcT, m1.isiT, m1.rhT)
#          df      AIC
# m1.ffmcT  3 3184.451
# m1.isiT   3 3203.632
# m1.rhT    3 3122.484 

# ISI response to predictors
# yhat = -30.79473 + 0.43926FFMC
m1.ffmcI <- lm(ISI ~ FFMC, data=forestfires)
summary(m1.ffmcI)
# Adjusted R-squared:  0.2814 
plot(forestfires$FFMC, forestfires$ISI)
abline(m1.ffmcI, col="orange", lw=3)
plot(m1.ffmcI) # to get Residuals vs. Fitted, second not "Normal QQ"

# ISI = beta0 + beta1*FFMC + beta2*FFMC^2
m1.ffmcI.squared <- lm(ISI ~ FFMC + I(FFMC^2), data=forestfires)
summary(m1.ffmcI.squared)
# Adjusted R-squared:  0.4488 

m1.ffmcI.inv <- lm(ISI ~ FFMC + I(1/FFMC), data=forestfires)
summary(m1.ffmcI.inv) # R^2 and AIC worse, use m1.ffmcI.squared instead
# Adjusted R-squared:  0.3798 
AIC(m1.ffmcI.inv) # 2793.948

AIC(m1.ffmcI, m1.tempI, m1.rhI, m1.ffmcI.squared)
# m1.ffmcI.squared:  df:4 AIC:2733.053
plot(m1.ffmcI.squared)
x <- forestfires$FFMC
xmesh <- seq(0.5*min(x), 2*max(x), by=0.1) 
yhat <- predict(m1.ffmcI.squared, newdata=data.frame(FFMC=xmesh))

plot(forestfires$FFMC, forestfires$ISI, xlab="FFMC", ylab="ISI", main="ISI vs FFMC")
abline(m1.ffmcI, col="orange", lw=3)
lines(xmesh, yhat, col="pink", lw=3)
# where to draw, line names, leave alone, line thickness
legend("topright", 
       c("Linear", "Quadratic"), 
       lty=c(1,1), 
       lwd=c(2, 2), 
       col=c("orange", "pink"))

m1.tempI <- lm(ISI ~ temp, data=forestfires)
summary(m1.tempI)
# Adjusted R-squared:  0.1538 
plot(forestfires$temp, forestfires$ISI, xlab="Temperature", ylab="ISI", main="ISI vs Temperature")
abline(m1.tempI, col="green", lw=3)

m1.rhI <- lm(ISI ~ RH, data=forestfires)
summary(m1.rhI)
# Adjusted R-squared:  0.01565 
plot(forestfires$RH, forestfires$ISI, xlab="Relative Humidity (RH)", ylab="ISI", main="ISI vs Relative Humidity")
abline(m1.rhI, col="green", lw=3)

m1.dmcI <- lm(ISI ~ DMC, data=forestfires)
summary(m1.dmcI)
# Adjusted R-squared:  0.09134 
plot(forestfires$DMC, forestfires$ISI, xlab="Duff Moisture Code (DMC)", ylab="ISI", main="ISI vs Duff Moisture Code")
abline(m1.dmcI, col="green", lw=3)

m1.dcI <- lm(ISI ~ DC, data=forestfires)
summary(m1.dcI)
# Adjusted R-squared:  0.05067 
plot(forestfires$DC, forestfires$ISI, xlab="Drought Code (DC)", ylab="ISI", main="ISI vs Drought Code")
abline(m1.dcI, col="green", lw=3)

AIC(m1.ffmcI, m1.tempI, m1.rhI)
#          df      AIC
# m1.ffmcI  3 2869.112 (THIS, lowest AIC and highest R^2)
# m1.tempI  3 2953.619
# m1.rhI    3 3031.815

# FFMC response to ISI, Temp, and RH predictors
m1.isiF <- lm(FFMC ~ ISI, data=forestfires)
summary(m1.isiF)
# Residual standard error: 4.679 
# Adjusted R-squared:  0.2814 
plot(forestfires$ISI, forestfires$FFMC)
abline(m1.isiF, col="red", lw=3)

plot(m1.isiF)
m1.isiF.squared <- lm(FFMC ~ ISI + I(ISI^2), data=forestfires)
summary(m1.isiF.squared)
AIC(m1.isiF.squared)

m1.isiF.inv <- lm(FFMC ~ ISI + I(1/ISI), data=forestfires)
summary(m1.isiF.inv) # Gives an error, cannot use 

plot(m1.isiF.squared) # Residuals Vs Fitted LESS parabolic 
# Use this? AIC AND R^2 are not as good... nevermind
a <- forestfires$ISI
amesh <- seq(0.5*min(a), 2*max(a), by=0.1) 
ywithhat <- predict(m1.isiF.squared, newdata=data.frame(ISI=amesh))

plot(forestfires$ISI, forestfires$FFMC, xlab="ISI", ylab="FFMC", main="FFMC vs ISI")
abline(m1.isiF, col="red", lw=3)
lines(amesh, ywithhat, col="green", lw=3)
# where to draw, line names, leave alone, line thickness
legend("topright", 
       c("Linear", "Quadratic"), 
       lty=c(1,1), 
       lwd=c(2, 2), 
       col=c("red", "green"))

m1.tempF <- lm(FFMC ~ temp, data=forestfires)
summary(m1.tempF) 
# Residual standard error: 4.985
# Adjusted R-squared:  0.1846 

m1.rhF <- lm(FFMC ~ RH, data=forestfires)
summary(m1.rhF) 
# Residual standard error: 5.269 
# Adjusted R-squared:  0.08883 

AIC(m1.isiF, m1.tempF, m1.rhF)
#             df      AIC
# m1.isiF      3 3066.802
# m1.tempF     3 3132.129
# m1.rhF       3 3189.566

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Two-Predictor Models~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remember: Lower AIC is better and higher R^2 is better

# m1.ffmcI <- lm(ISI ~ FFMC, data=forestfires)
# Adjusted R-squared:  0.2814 

# m1.ffmcI.squared <- lm(ISI ~ FFMC + I(FFMC^2), data=forestfires)
# Adjusted R-squared:  0.4488 

# m1.tempI <- lm(ISI ~ temp, data=forestfires)
# Adjusted R-squared:  0.1538 

# m1.rhI <- lm(ISI ~ RH, data=forestfires)
# Adjusted R-squared:  0.01565 

# m1.dmcI <- lm(ISI ~ DMC, data=forestfires)
# Adjusted R-squared:  0.09134 

# m1.dcI <- lm(ISI ~ DC, data=forestfires)
# Adjusted R-squared:  0.05067 

AIC(m1.ffmcI, m1.ffmcI.squared, m1.tempI, m1.rhI, m1.dmcI, m1.dcI)
#                  df      AIC
# m1.ffmcI          3 2869.112
# m1.ffmcI.squared  4 2733.053
# m1.tempI          3 2953.619
# m1.rhI            3 3031.815
# m1.dmcI           3 2990.450
# m1.dcI            3 3013.087

# ISI = beta0 + beta1*FFMC + beta2*temp
m2.ffmc.temp <- lm(ISI ~ FFMC + temp, data=forestfires)
summary(m2.ffmc.temp)
# Adjusted R-squared:  0.3135 

m2.ffmc.temp.sq <- lm(ISI ~ FFMC + I(FFMC^2) + temp + I(temp^2), data=forestfires)
summary(m2.ffmc.temp.sq)
# Adjusted R-squared:  0.4506 

AIC(m2.ffmc.temp, m2.ffmc.temp.sq)
#                 df      AIC
# m2.ffmc.temp     4 2846.477
# m2.ffmc.temp.sq  6 2733.281

m2.ffmc.rh <- lm(ISI ~ FFMC + RH, data=forestfires)
summary(m2.ffmc.rh)
# Adjusted R-squared:  0.2809

m2.ffmc.rh.sq <- lm(ISI ~ FFMC + I(FFMC^2) + RH + I(RH^2), data=forestfires)
summary(m2.ffmc.rh.sq)
# Adjusted R-squared:  0.4506 

AIC(m2.ffmc.rh, m2.ffmc.rh.sq)
#               df      AIC
# m2.ffmc.rh     4 2870.509
# m2.ffmc.rh.sq  6 2733.331

m2.ffmc.dmc <- lm(ISI ~ FFMC + DMC, data=forestfires)
summary(m2.ffmc.dmc)
# Adjusted R-squared:  0.2922 

m2.ffmc.dmc.sq <- lm(ISI ~ FFMC + I(FFMC^2) + DMC + I(DMC^2), data=forestfires)
summary(m2.ffmc.dmc.sq)
# Adjusted R-squared:  0.4556

AIC(m2.ffmc.dmc, m2.ffmc.dmc.sq)
#               df      AIC
# m2.ffmc.dmc     4 2862.311
# m2.ffmc.dmc.sq  6 2728.619

m2.ffmc.dc <- lm(ISI ~ FFMC + DC, data=forestfires)
summary(m2.ffmc.dc)
# Adjusted R-squared:  0.2832 

m2.ffmc.dc.sq <- lm(ISI ~ FFMC + I(FFMC^2) + DC + I(DC^2), data=forestfires)
summary(m2.ffmc.dc.sq)
# Adjusted R-squared:  0.4714 

AIC(m2.ffmc.dc, m2.ffmc.dc.sq)
#               df      AIC
# m2.ffmc.dc     4 2868.800
# m2.ffmc.dc.sq  6 2713.372

overfit <- lm(ISI ~ ., data=forestfires)
summary(overfit)

m1.windI <- lm(ISI ~ wind, data=forestfires)
summary(m1.windI)
AIC(m1.windI)
# Adjusted R-squared:  0.009492 
# AIC: 3035.041

m2.ffmc.wind <- lm(ISI ~ FFMC + wind, data=forestfires)
summary(m2.ffmc.wind)
# Adjusted R-squared:  0.295 
m2.ffmc.wind.sq <- lm(ISI ~ FFMC + I(FFMC^2) + wind + I(wind^2), data=forestfires)
summary(m2.ffmc.wind.sq)
# Adjusted R-squared:  0.4768 
AIC(m2.ffmc.wind, m2.ffmc.wind.sq)
#                 df      AIC
# m2.ffmc.wind     4 2860.265
# m2.ffmc.wind.sq  6 2708.006

m2.interaction <- lm(ISI ~ FFMC*DC, data=forestfires)
summary(m2.interaction)
AIC(m2.interaction)
# Adjusted R-squared:  0.2969 
# AIC:  2859.862

m2.interaction1 <- lm(ISI ~ FFMC*wind, data=forestfires)
summary(m2.interaction1)
AIC(m2.interaction1)
# Adjusted R-squared:  0.3434 
# AIC:  2824.435

yhat_m2 <- predict(m2.ffmc.wind.sq)
rss_m2model <- sum((forestfires$ISI - yhat_m2)^2); rss_m2model


