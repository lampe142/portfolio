################################################################################
# import data Test
################################################################################
# tail(getHisDataAV(symbolI='EEM', verbose=F, os='compact'),1)
# ailhVaR <- PerformanceAnalytics::VaR(R=dm$ret , method="gaussian",p=.95)

################################################################################
# 
################################################################################
# source('install_packages.R')
setwd('/home/rstudio/portfolio_aws')
source('profileRep.R')

fileName = 'PositionInfo/full_Port.xlsx'
dp <<- list()
dm <<- list()
dp$position <- readxl::read_excel(path=fileName, sheet='Position')
dp$risk <- readxl::read_excel(path=fileName, sheet='Risk')#[,1:6]
row.names(dp$risk) <- dp$risk$AlphaVantage
row.names(dp$position) <- dp$risk$AlphaVantage

#Download data
getAllData(FXsource='ECB') 
# tail(dp$avAdjClose$EEM,1)
# tail(dp$FX$USEURO,1)
# tail(dp$FX$rf1y,1)
# addTodp(downAssets = c('MMNFF', 'KSHB')) 

updatePositionRisk()
mergeCloseRet()
filterMSGARCHLogRet()
updateRisk(dm$logRet, rf=dp$FX$rf1y)
getRisk(logR = dm$logRet)
bootRisk(logR = dm$logRet)

# tail(dm$logRet$EEM)

save.image(paste0(here::here(),"/Data/Portfolio_Market.RData"))

rmarkdown::render("dash.Rmd",output_file='Portfolio_Performance_Risk.html')
# browseURL("Portfolio_Performance_Risk.html")

rmarkdown::render("dash2.Rmd",output_file='Portfolio_Risk.html')
# browseURL("Portfolio_Risk.html")

# tail(dp$avAdjClose$EEM,1)
# view(dp$position)
# View(dp$risk)
# View(dm$logRetFilMSGARCH1)
# View(dm$logRet)
# View(can.price)

save.image(paste0("~/Documents/Julia/Portfolio/Data/",Sys.Date()," market.RData"))
#sum(dp$risk$`VaR Share`,na.rm=T)