################################################################################
# import data Test
################################################################################
# tail(getHisDataAV(symbolI='EEM', verbose=F, os='compact'),1)
# ailhVaR <- PerformanceAnalytics::VaR(R=dm$ret , method="gaussian",p=.95)

################################################################################
# 
################################################################################
print(paste('########### 1. Set working directory',getwd(), Sys.time()))
# source('install_packages.R')
 setwd('/home/rstudio/portfolio')
# setwd(thisfile())
# setwd(here::here())

source('profileRep.R')

print(paste('########### 2. Read Input'))
fileName = 'PositionInfo/full_Port.xlsx'
dp <<- list()
dm <<- list()
dp$position <- readxl::read_excel(path=fileName, sheet='Position')[-21,]
dp$risk <- readxl::read_excel(path=fileName, sheet='Risk')[-21,]
row.names(dp$risk) <- dp$risk$AlphaVantage
row.names(dp$position) <- dp$risk$AlphaVantage

# delete old files
# file.remove('Dashboard/portfolio_performance.html')
# file.remove('Dashboard/portfolio_risk.html')
# file.remove('Data/Portfolio_Market.RData')

# Download equity, FX, rate data
print(paste('########### 3. Get Market Data'))
getAllData(FXsource ='ECB') 
print(paste('Latest EEM:',index(tail(dp$avAdjClose$EEM,1)),tail(dp$avAdjClose$EEM,1)))
print(paste('Latest FX US EURO:',index(tail(dp$FX$USEURO,1)),tail(dp$FX$USEURO,1)))
# addTodp(downAssets = c('MMNFF', 'KSHB')) 

# Run Analysis
print(paste('########### 4. Run Risk and Performance Analysis'))
# load("~/portfolio/Data/Portfolio_Market.RData")
updatePositionRisk()
mergeCloseRet()
print(paste('Latest merged return:',index(tail(dm$logRet,1))))

# Analysis
filterMSGARCHLogRet(verbose = F)
updateRisk(dm$logRet, rf=dp$FX$rf1y)
getRisk(logR = dm$logRet)
bootRisk(logR = dm$logRet, nBoot = 1000)

# tail(dm$logRet$EEM)
print(paste('########### 5. Save output'))
save.image(paste0(here::here(),"/Data/Portfolio_Market.RData"))

## upload backup to dropbox
print(paste('########### 5.1 Save output to dropbox'))
save.image(paste0(here::here(),"/Backup/",Sys.Date()," Portfolio_Market.RData"))
rdrop2::drop_upload(paste0(here::here(),"/Backup/",Sys.Date()," Portfolio_Market.RData"), 
                    path = "PortfolioData",file = "token.rds")
file.remove(paste0(here::here(),"/Backup/",Sys.Date()," Portfolio_Market.RData"))

savePortRisk(verbose = F)
load(paste0(here::here(),"/Data/portRisk.RData"))
print(paste('Port VaR:',tail(portRisk$CloseDate,1),tail(portRisk$VaR_bootstrap,1)))

# Render Dashboards
print(paste('########### 6. Render Dashboard'))
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
rmarkdown::render("Dashboard/dash_performance.Rmd",output_file="../DashboardOutput/portfolio_performance.html")
# browseURL("DashboardOutput/portfolio_performance.html")
rmarkdown::render("Dashboard/dash_risk.Rmd",output_file="../DashboardOutput/portfolio_risk.html")
# browseURL("DashboardOutput/portfolio_risk.html")

print(paste('########### 7. Send Dashboards by E-Mail'))
source('E-Mail/send_portfolio_email.R')

print(paste('########### 8. Run web scrapping'))
source('Webscrap/web_scrap_price_weed.R')
source('Dashboard/canUSplot.R')

print(paste('########### 9. updated completed'))
.rs.restartR()
# tail(dp$avAdjClose$EEM,1)
# tail(dm$logRet,1)
# view(dp$position)
# View(dp$risk)
# View(dm$logRetFilMSGARCH1)
# View(dm$logRet)
# View(can.price)
#sum(dp$risk$`VaR Share`,na.rm=T)