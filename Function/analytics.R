

portfolioAnalysis  <- function(verbose=F) {
  
  # trans <- readxl::read_excel(path="Data/current_Transactions_2019-03-12.xlsx")
  
  trans <- readxl::read_excel(path="Data/current_Transactions_2019-05-04T17_03_45Z.xlsx")
  assets <- readxl::read_excel(path='PositionInfo/full_Port_v2.xlsx')
  # trans <- read.csv("Data/current_Transactions_2019-03-12.csv", stringsAsFactors = F)
  trans %>% subset(Type!="Dividend" & Matched=='x') %>%
    merge(y=assets[,c('AlphaVantage','FT')],by.x='Name',by.y='FT') %>%
    mutate(Quantity= gsub(',', '', Quantity)) -> trans
  
  if(verbose) View(head(trans))
  
  dates <- seq(from = as.Date("2017-01-11"), to = Sys.Date(), by = 1)
  quantity <- xts(matrix(0, nrow=length(dates), ncol=length(unique(trans$Name))), order.by=dates)
  names(quantity) <- unique(trans$AlphaVantage)
  weights <- xts(matrix(0, nrow=length(dates), ncol=length(unique(trans$Name))), order.by=dates)
  names(weights) <- unique(trans$AlphaVantage)
  
  trans$Date <- gsub('.{0}$', '', trans$Date)
  
  quantity[1,'NFLX'] <- 10
  
  #i=462 '2018-04-17' == index(quantity)
  for(i in 2:length(quantity[,1])){
    quantity[i,] <- quantity[i-1,]
    trans %>% subset(Date == index(quantity[i,])) -> transD
    if(dim(transD)[1] != 0){
      for(j in 1:length(transD$AlphaVantage)){
        if(transD$Type[j] == "Long buy"){
          quantity[i,transD$AlphaVantage[j]]  <- quantity[i,transD$AlphaVantage[j]] + as.numeric(transD$Quantity[j])
        }else if(transD$Type[j] == "Long sell"){
          quantity[i,transD$AlphaVantage[j]]  <- quantity[i,transD$AlphaVantage[j]] - as.numeric(transD$Quantity[j])
        }
      }    
    }
  }
  
  sel <- intersect(names(quantity), names(dm$adjClose))
  value <- quantity[,sel] * dm$adjClose[,sel]
  valuePort <- xts(rowSums(value,na.rm=TRUE), order.by = index(value))
  
  w <- merge(value, valuePort)
  for (i in 1:(dim(w)[2]-1)){
    w[,i] <- w[,i] / w$valuePort
  }
  
  # w <- w[,-34]
  w <- w[,-ncol(w)]
  
  if(verbose) print(paste('check if weights sum to 1 result:',sum(tail(w,1))))
  
  w <- replace(w, is.na(w),0)
  
  if(verbose) View(tail(w))
  if(verbose) View(head(w))
  
#  sDate <- '2018-02-21/'
  sDate <- '2017-01-11/'
  
  dp$portfolio_returns <<- PerformanceAnalytics::Return.portfolio(R=dm$logRet[sDate,sel], weights=w[sDate,] ,verbose=F)$portfolio.returns
  dp$portfolio_weight <<- w
  
  return() 
}
  
  