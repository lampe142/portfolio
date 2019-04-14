################################################################################
# compute and export beta and capm return
# logR = dm$logRet[,1:49]
# updateRisk(logR = dm$logRet,rf = dp$FX$rf1y)
# rf: 1 year risk free rate
updateRisk <- function(logR, rf, nBoot=100){
#  risk <- readxl::read_excel(path=fileName, sheet='Risk')
  lY <- lubridate::ymd(index(tail(logR[,"ACWI"],1))) - lubridate::years(1)
  tD <- lubridate::ymd(index(tail(logR[,"ACWI"],1)))
  
  dp$position$'Beta' <<- NA
  dp$position$'Beta1Y' <<- NA
  dp$position$std_log_returns <<- NA
  dp$position$std_log_returns_1Y <<- NA
  dp$position$CAPM.Return <<- NA
  dp$position$CAPM.Return.D1 <<- NA
  dp$position$'250D.CAPM.Return' <<- NA
  dp$position$CAPM.Return.D5 <<- NA
  dp$position$CAPM.Return.D23 <<- NA  
  dp$position$Jensen.Alpha <<- NA  
  
  for(iAss in colnames(logR)){
    iAssPos <- which(dp$position$AlphaVantage == iAss)
    # Beta entire time period
    dp$position$'Beta'[iAssPos] <<- stats::cov(logR[,"ACWI"], logR[,iAss]) / stats::var(logR[,"ACWI"])
    
    # Beta last year
    dp$position[iAssPos, "Beta1Y"] <<- stats::cov(logR[paste(lY,tD,sep='/'), 'ACWI'], logR[paste(lY,tD,sep='/'), iAss]) /
      stats::var(logR[paste(lY,tD,sep='/'), 'ACWI'])
    
    dm$logRetPort
    
    # standard deviations
    dp$position[iAssPos, "std_log_returns"]  <<- sd(logR[,iAss])
    dp$position[iAssPos, "std_log_returns_1Y"] <<- sd(logR[paste(lY,tD,sep='/'), iAss])
  }
  # compute stats for the entire Portfolio:
  dp$position['portfolio','Beta'] <<- stats::cov(logR[,"ACWI"], dm$logRetPort) / stats::var(logR[,"ACWI"])
  dp$position['portfolio','Beta1Y'] <<- stats::cov(logR[paste(lY,tD,sep='/'), 'ACWI'], dm$logRetPort[paste(lY,tD,sep='/')]) /
  stats::var(logR[paste(lY,tD,sep='/'), 'ACWI'])
  dp$position['portfolio', "std_log_returns"]  <<- sd(dm$logRetPort)
  dp$position['portfolio', "std_log_returns_1Y"] <<- sd(dm$logRetPort[paste(lY,tD,sep='/')])
  
  dp$position$'CAPM.Return.D1' <<- as.vector(coredata(tail(rf,1))) + dp$position$Beta1Y*
    (coredata(tail(logR[,"ACWI"],1)) -coredata(tail(rf,1)))
  while (is_empty(as.vector(dm$adjClose[lY,"ACWI"]))) { lY <- date+1}
  rmD250 <- as.vector(dm$adjClose[tD,"ACWI"])/ as.vector(dm$adjClose[lY,"ACWI"])-1
  dp$position$'250D.CAPM.Return' <<- as.vector(coredata(tail(rf,1))) + dp$position$Beta1Y*
    as.vector(( rmD250 * length(logR[paste(lY,tD,sep='/'),"ACWI"])
               -coredata(tail(rf,1))))

  date <- lubridate::ymd(index(tail(logR[,"ACWI"],1))) - lubridate::days(5)
  while (is_empty(as.vector(dm$adjClose[date,"ACWI"]))) { date <- date+1}
  rmD5 <- as.vector(dm$adjClose[tD,"ACWI"]) / as.vector(dm$adjClose[date,"ACWI"])-1
  dp$position$'CAPM.Return.D5' <<- as.vector(coredata(tail(rf,1))) + dp$position$Beta1Y *
    as.vector(( rmD5 * length(logR[paste(lY,tD,sep='/'),"ACWI"]) - coredata(tail(rf,1))))
    
  date <- lubridate::ymd(index(tail(logR[,"ACWI"],1))) - lubridate::days(30)
  while (is_empty(as.vector(dm$adjClose[date,"ACWI"]))) { date <- date+1}
  rmD23 <- as.vector(dm$adjClose[tD,"ACWI"])/ as.vector(dm$adjClose[date,"ACWI"])-1
  dp$position$'CAPM.Return.D23' <<- as.vector(coredata(tail(rf,1))) + dp$position$Beta1Y *
    as.vector(( rmD23 * length(logR[paste(lY,tD,sep='/'),"ACWI"])
                -coredata(tail(rf,1))))
  
  
  dp$position['portfolio','CAPM.Return'] <<- as.vector(coredata(tail(rf,1))) + dp$position['portfolio','Beta1Y'] *
    (coredata(tail(logR[,"ACWI"],1)) - coredata(tail(rf,1)))

  dp$position$'Jensen.Alpha' <<- dp$position$'250D.logReturn' - dp$position$'250D.CAPM.Return'
  
  posPort <- which(dp$position$Name=="Portfolio:")
  dp$position$'Beta'[posPort] <<- replace(dp$position$'Beta', is.na(dp$position$'Beta'),0) %*% 
    replace(dp$position$SharePortfolio, is.na(dp$position$SharePortfolio),0)
  dp$position$'Beta1Y'[posPort] <<- replace(dp$position$'Beta1Y', is.na(dp$position$'Beta1Y'),0) %*% 
    replace(dp$position$SharePortfolio, is.na(dp$position$SharePortfolio),0)
  
  # standard deviations
  
  mergIndex <- dp$position$AlphaVantage %in% names(dm$logRet)
  dp$position[posPort, "std_log_returns"]  <<- sd(dm$logRet %*% 
   dp$position$SharePortfolio[mergIndex])
  dp$position[posPort, "std_log_returns_1Y"] <<- sd(dm$logRet[paste(lY,tD,sep='/'),] %*%
    dp$position$SharePortfolio[mergIndex])
  
  dp$position$Category[posPort] <<-'Portfolio:'
  
  return()
}

# compute and export Risk measures
getRisk <- function(logR, nBoot=100){
  lY <- lubridate::ymd(index(tail(logR[,"ACWI"],1))) - lubridate::years(1)
  tD <- lubridate::ymd(index(tail(logR[,"ACWI"],1)))
  
  dp$position$Value <<- replace(dp$position$Value, is.na(dp$position$Value),0)
  weight <- dp$position$Value[-length(dp$position$Value)] / sum(dp$position$Value[-length(dp$position$Value)])
  hVaR.index <- dp$position$AlphaVantage %in% colnames(dm$logRet)
  hVaR.weight <- dp$position$Value[hVaR.index] / sum(dp$position$Value[hVaR.index])
  # check share of portfolio included in the hVaR
  # sum(position$SharePortfolio[position$AlphaVantage %in% colnames(dm$logRet)], na.rm = T)
  # x <- colnames(dm$logRet) %!in%  dp$position$AlphaVantage 
  # colnames(dm$logRet)[x]
  
  #compute portfolio VaR
  dp$position$'VaR HS 1Y' <<- NA
  dp$position[dp$position$Name=="Portfolio:", "VaR HS 1Y"] <- 
    compVaRPort(logR=dm$logRet, weight= hVaR.weight)
  #compute portfolio ES
  dp$position$ES <<- NA
  dp$position[dp$position$Name=="Portfolio:", "ES"] <- 
    compESPort(logR=dm$logRet, weight= hVaR.weight)
  # compute incremental VaR
  dp$position$'incremental VaR' <<- NA
  dp$position[hVaR.index,'incremental VaR'] <- incVaR(logR=dm$logRet, hVaR.weight= hVaR.weight)
  dp$position$'incremental VaR to Port VaR (share Portfolio VaR)' <<- NA
  dp$position[hVaR.index,'incremental VaR to Port VaR (share Portfolio VaR)'] <- dp$position[hVaR.index,'incremental VaR'] / 
    unlist(dp$position[dp$position$Name=="Portfolio:", "VaR HS 1Y"])
  dp$position$'incremental VaR per Value' <<- NA
  dp$position[hVaR.index,'incremental VaR per Value'] <- dp$position[hVaR.index,'incremental VaR'] / dp$position$Value[hVaR.index]
  
  dp$position$'share Portfolio VaR / share portfolio' <<- NA
  dp$position[hVaR.index,'share Portfolio VaR / share portfolio'] <- 
    dp$position[hVaR.index,'incremental VaR to Port VaR (share Portfolio VaR)'] /
    dp$position[hVaR.index,'SharePortfolio']
  
  # compute marginal VaR
  dp$position$'marginal VaR in €' <<- NA
  dp$position[hVaR.index,'marginal VaR in €'] <- mVaR(dm$logRet, hVaR.weight, dp$position, hVaR.index) 
  
  #compute individual VaR and ES 
  dp$position$'VaR Normal std1Y' <<- NA
  dp$position$'VaR Normal' <<- NA
  dp$position$'VaR HS 1Y' <<- NA
  dp$position$'individual VaR Bootstrap' <<- NA
  for(iAss in colnames(logR)){
    iAssPos <- which(dp$position$AlphaVantage == iAss)
    dp$position[iAssPos, "VaR Normal std1Y"] <- -2.33 * sd(logR[paste(lY,tD,sep='/'), iAss])
    dp$position[iAssPos, "VaR Normal"] <- -2.33 * sd(logR[,iAss])
    dp$position[iAssPos, "VaR HS 1Y"] <- unlist(quantile(logR[paste(lY,tD,sep='/'), iAss], 0.01))
    dp$position[iAssPos, "individual VaR Bootstrap"] <- mean(bootstrap::bootstrap(logR[,iAss], nBoot, theta=quantile, p=0.01)$thetastar)
  }
  
  return()
}

#logR <-  dm$logRet
# compute Value at Risk Portfolio level
compVaRPort <- function(logR, weight=NULL){
  if(is.null(weight)) {weight <- rep(1/length(logR[1,]), length(logR[1,]))}
  p.logR <- logR %*% weight
  return(quantile(p.logR , probs=0.01, na.rm = T))
}

# compute Expected Shortfall Portfolio level
compESPort <- function(logR, weight=NULL){
  if(is.null(weight)) {weight <- rep(1/length(logR[1,]), length(logR[1,]))}
  p.logR <- logR %*% weight
  VaR <- quantile(p.logR , probs=0.01, na.rm = T) 
  ES <- mean(p.logR[p.logR < VaR])
  return( mean(p.logR[p.logR < VaR]))
}

# computes the incremental VaR
incVaR <- function(logR, hVaR.weight){
  incVaR <- numeric(length(hVaR.weight))
  varP <- compVaRPort(logR, hVaR.weight)
  for(i in 1:length(hVaR.weight)){
    incVaR[i] <- varP - compVaRPort(logR, replace(hVaR.weight, hVaR.weight==hVaR.weight[i], 0))
  }
    return(incVaR)
}

# computes the marginal VaR
#mVaR(logR, hVaR.weight, position, hVaR.index) 
mVaR <- function(logR, hVaR.weight, position, hVaR.index){
  mVaR <- numeric(length(hVaR.weight))
  varP <- compVaRPort(logR, hVaR.weight)
  portValue <- sum(position$Value[hVaR.index])
  selPosValue <- position$Value[hVaR.index]
  for(i in 1:length(selPosValue)){
    selPosValue2 <- selPosValue
    selPosValue2[i] <- selPosValue[i]+1
    newWeight <- selPosValue2 / portValue
    mVaR[i] <- (compVaRPort(logR, newWeight) - varP)*portValue
  }
  return(mVaR)
}

# add column
# portRisk <- cbind(portRisk, data.frame(VolaN1_MSGARCH=matrix(data = rep(NA,nrow(portRisk)))))
savePortRisk <- function(verbose = F){
  load(paste0(here::here(),"/Data/portRisk.RData"))
  portRisk <- rbind(portRisk, dp$position["portfolio",c("CloseDate","Beta1Y","Beta","Jensen.Alpha","250D.CAPM.Return","std_log_returns_1Y","VaR bootstrap 05Quantile","VaR_bootstrap","VaR bootstrap 95Quantile",
                                                 "VolaN1_MSGARCH","MSGARCH_prob","ES_MSGARCH", "ES bootstrap 05Quantile","ES bootstrap" ,"ES bootstrap 95Quantile")])
  if(verbose) View(portRisk)
  save(file=paste0(here::here(),"/Data/portRisk.RData"), portRisk)
}


