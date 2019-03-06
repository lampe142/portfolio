# install.packages("devtools")
devtools::install_github("carlanetto/M4comp2018")
require(M4comp2018)
data(M4)

names(M4[[1]])


d_M4 <- Filter(function(l) l$period == "Daily", M4)
t <- as.xts(ts(c(d_M4[[1]]$x, d_M4[[1]]$xx),
          start=start(d_M4[[1]]$x), frequency = frequency(d_M4[[1]]$x)))
for(i in 2:20){
t <-   cbind(t,as.xts(ts(c(d_M4[[i]]$x, d_M4[[i]]$xx),
   start=start(d_M4[[i]]$x), frequency = frequency(d_M4[[i]]$x))))
}
plot(t)

d_M4 <- Filter(function(l) l$period == "Monthly", M4)
t <- as.xts(ts(c(d_M4[[1]]$x, d_M4[[1]]$xx),
               start=start(d_M4[[1]]$x), frequency = frequency(d_M4[[1]]$x)))
for(i in 2:20){
  t <-   cbind(t,as.xts(ts(c(d_M4[[i]]$x, d_M4[[i]]$xx),
                           start=start(d_M4[[i]]$x), frequency = frequency(d_M4[[i]]$x))))
}
plot(t['1990-01-01/2010-01-01'])

selT <- t['1990-01-01/2010-01-01']
selT <- ts(selT)
base
tt <- nowcast(y = selT[1], x = selT[-1], r = 2, p = 2, q = 2, method = '2sq')

ICQ1 <- nowcasting::ICshocks(x = selT[-1], r = 2, p = 2)
now <- nowcast(y = selT[1], x = selT[-1], r = 2, q = 2 , p = 2)
ICR1 <- ICfactors(x = x, type = 1)

source('Function/nowcasting.R')
nc <- list()
model <- list()
nc$fed <- readxl::read_excel(path='Data/FED/US/2017-01-27.xls', sheet='data')
nc$fed.spec <- readxl::read_excel(path='Data/FED/Spec_US_example.xls', sheet='spec')
nc$fed.trans <- transStationary(data=nc$fed, spec=nc$fed.spec)


ggplot2::ggplot(nc$fed, aes(Date, GACDISA066MSFRBNY)) + geom_line()

nowcastUSGDP <- nowcasting::nowcast(y = gdp, x = base, r = 2, p = 2, q = 2, method = '2sq')
library(tensorflow)
library(greta)
devtools::install_github("rbagd/dynfactoR")

dataModel <- as.matrix(nc$fed.trans[c(-1,-385),c("PAYEMS","UNRATE","PERMIT","TCU")])
dataModel <- as.matrix(nc$fed.trans[c(-1,-385),-1])
model[1] <- dynfactoR::dfm(X=dataModel,r=2, p=2, q=1)

x <- matrix(rnorm(50*10), 50, 10)
W <- as.logical(matrix(rbinom(50*10, 1, 0.1), 50, 10))
x[W] <- NA
dynfactoR::dfm(x, 2, 2, 1)


###################
devtools::install_github("fate-ewi/bayesdfa")
library(bayesdfa)
vignette("bayesdfa")


###################
# https://github.com/nmecsys/nowcasting
devtools::install_github("nmecsys/nowcasting")
library(nowcasting)
vignette("nowcasting")
data(USGDP)
View(USGDP)
gdp <- month2qtr(x = USGDP$base[,"RGDPGR"], reference_month = 3)
plot(gdp)
gdp_position <- which(colnames(USGDP$base) == "RGDPGR")
base <- Bpanel(base = USGDP$base[,-gdp_position], 
trans = USGDP$legend$Transformation[-gdp_position], aggregate = TRUE)

nowcastUSGDP <- nowcast(y = gdp, x = base, r = 2, p = 2, q = 2, method = '2sq')
plot(base[,1:5])

# y fcst
nowcast.plot(nowcastUSGDP, type = "fcst")
# factors
nowcast.plot(nowcastUSGDP, type = "factors") 
# how much of the variability in the dataset is explained by each factor 
nowcast.plot(nowcastUSGDP, type = "eigenvalues")
# importance of each variable in the first factor
nowcast.plot(nowcastUSGDP, type = "eigenvectors")
