
here::here()
source('profileRep.R')


################################################################################
# import data Test
################################################################################
tail(getHisDataAV(symbolI='MMNFF', verbose=F, os='compact'),1)
ailhVaR <- PerformanceAnalytics::VaR(R=dm$ret , method="gaussian",p=.95)

################################################################################
# 
################################################################################
fileName = 'PositionInfo/full_Port.xlsx'
dp <<- list()
dm <<- list()
dp$position <- readxl::read_excel(path=fileName, sheet='Position')
dp$risk <- readxl::read_excel(path=fileName, sheet='Risk')
row.names(dp$risk) <- dp$risk$AlphaVantage
row.names(dp$position) <- dp$risk$AlphaVantage

#Download data
getAllCurrentAV()
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


save.image(paste0(here::here(),"/Data/Portfolio_Market.RData"))
rmarkdown::render("dash.Rmd",output_file='Portfolio_Performance_Risk.html')
browseURL("Portfolio_Performance_Risk.html")
rmarkdown::render("dash2.Rmd",output_file='Portfolio_Risk.html')
browseURL("Portfolio_Risk.html")
source('send_portfolio_email.R')

view(dp$position)
View(dp$risk)
View(dm$logRetFilMSGARCH1)
View(dm$logRet)

save.image(paste0("~/Documents/Julia/Portfolio/Data/",Sys.Date()," market.RData"))

exportToExcleOpen()
