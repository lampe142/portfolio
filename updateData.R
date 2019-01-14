################################################################################
# import data Test
################################################################################
# tail(getHisDataAV(symbolI='EEM', verbose=F, os='compact'),1)
# ailhVaR <- PerformanceAnalytics::VaR(R=dm$ret , method="gaussian",p=.95)

################################################################################
# 
################################################################################
# source('install_packages.R')
 setwd('/home/rstudio/portfolio')
# setwd(thisfile())
# setwd(here::here())
print(paste('########### 1. Set working directory',getwd()))
source('profileRep.R')

print(paste('########### 2. Read Input'))
fileName = 'PositionInfo/full_Port.xlsx'
dp <<- list()
dm <<- list()
dp$position <- readxl::read_excel(path=fileName, sheet='Position')
dp$risk <- readxl::read_excel(path=fileName, sheet='Risk') #[,1:6]
row.names(dp$risk) <- dp$risk$AlphaVantage
row.names(dp$position) <- dp$risk$AlphaVantage

# delete old files
# file.remove('Dashboard/portfolio_performance.html')
# file.remove('Dashboard/portfolio_risk.html')
# file.remove('Data/Portfolio_Market.RData')

# Download equity, FX, rate data
print(paste('########### 3. Get Market Data'))
# getAllData(FXsource ='ECB') 
# tail(dp$avAdjClose$EEM,1)
# tail(dp$FX$USEURO,1)
# tail(dp$FX$rf1y,1)
# addTodp(downAssets = c('MMNFF', 'KSHB')) 

# Run Analysis
print(paste('########### 4. Run Risk and Performance Analysis'))
load("~/portfolio/Data/Portfolio_Market.RData")
updatePositionRisk()
mergeCloseRet()
filterMSGARCHLogRet()
updateRisk(dm$logRet, rf=dp$FX$rf1y)
getRisk(logR = dm$logRet)
bootRisk(logR = dm$logRet)

# tail(dm$logRet$EEM)

print(paste('########### 5. Save output'))
save.image(paste0(here::here(),"/Data/Portfolio_Market.RData"))
save.image(paste0(here::here(),"/Backup/",Sys.Date()," Portfolio_Market.RData"))
savePortRisk()

rmarkdown::render("Dashboard/dash_performance.Rmd",output_file="portfolio_performance.html")
# browseURL("Dashboard/portfolio_performance.html")
rmarkdown::render("Dashboard/dash_risk.Rmd",output_file="portfolio_risk.html")
# browseURL("Dashboard/portfolio_risk.html")

print(paste('########### 6. Send Dashboards by E-Mail'))
source('E-Mail/send_portfolio_email.R')

print(paste('########### 7. updated completed'))
# tail(dp$avAdjClose$EEM,1)
# tail(dm$logRet,1)
# view(dp$position)
# View(dp$risk)
# View(dm$logRetFilMSGARCH1)
# View(dm$logRet)
# View(can.price)
#sum(dp$risk$`VaR Share`,na.rm=T)