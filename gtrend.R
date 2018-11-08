# google search trends

assetsID$ShortName[3:13]
library(gtrendsR)
google.trends = gtrends(assetsID$ShortName[3:7], gprop = "web")[[1]]
google.trends = gtrends(assetsID$ShortName[10], gprop = "web")[[1]]
google.trends = gtrends(assetsID$ShortName[10], gprop = "news")[[1]]


google.trends = gtrendsR::gtrends(c("namaste"), gprop = "web", time = "all")
gt.namaste <- xts::xts(x=google.trends[[1]]$hits, order.by = google.trends[[1]]$date)

# plot(gt.namaste)

library(nowcasting)
data(USGDP)
gdp <- nowcasting::month2qtr(x = USGDP$base[,"RGDPGR"])

gdp_position <- which(colnames(USGDP$base) == "RGDPGR")
base <- Bpanel(base = USGDP$base[,-gdp_position], 
               trans = USGDP$legend$Transformation[-gdp_position], aggregate = TRUE)

nowcastUSGDP <- nowcast(y = gdp, x = base, r = 2, p = 2, q = 2, method = '2sq')

# y forecasts
tail(nowcastUSGDP$yfcst,8)

# the regression between y and its factors can be accessed using `$reg`.
summary(nowcastUSGDP$reg)

# the results related to the estimation of factors 
tail(nowcastUSGDP$factors$dynamic_factors) # factors
head(nowcastUSGDP$factors$Lambda) # Lambda matrix
nowcastUSGDP$factors$A # A matrix
nowcastUSGDP$factors$BB # BB': u's variance covariance matrix (factor equation)
diag(nowcastUSGDP$factors$Psi) # Psi: epsilon's variance covariance matrix (x equation)

# the forecasts of the explanatory variables are in `$xfcst`.
tail(nowcastUSGDP$xfcst[,1:5]) # x forecasts (first 5 variables)

# y fcst
nowcast.plot(nowcastUSGDP, type = "fcst")

# factors
nowcast.plot(nowcastUSGDP, type = "factors") 

# how much of the variability in the dataset is explained by each factor 
nowcast.plot(nowcastUSGDP, type = "eigenvalues")

# importance of each variable in the first factor
nowcast.plot(nowcastUSGDP, type = "eigenvectors") 


