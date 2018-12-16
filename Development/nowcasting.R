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

