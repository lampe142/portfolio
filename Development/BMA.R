# Bayesian Model Average
# example 1
library(BMS)
data("attitude")
View(attitude)
att = bms(X.data = attitude, mprior = "uniform", g = "UIP", user.int = F)
coef(att)
summary(att)
topmodels.bma(att)[, 1:3]
image(att)
