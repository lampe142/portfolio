test <- function(){
  dp$position$TEST <- NA
  dp$position <<- dp$position
}


################################################################################
# compute and export beta and capm return
# logR = dm$logRet[,1:49]
# updateRisk(logR = dm$logRet,rf = dp$FX$rf1y)
# rf: 1 year risk free rate
updateRisk <- function(logR, rf, nBoot=100){
#  risk <- readxl::read_excel(path=fileName, sheet='Risk')
  lY <- lubridate::ymd(index(tail(logR[,"ACWI"],1))) - lubridate::years(1)
  
  tD <- lubridate::ymd(index(tail(logR[,"ACWI"],1)))
  
  dp$position$'beta' <- NA
  dp$position$'beta_1Y' <- NA
  dp$position$std_log_return <- NA
  dp$position$std_log_return_1Y <- NA
  dp$position$CAPM_return <- NA
  dp$position$CAPM_return_D1 <- NA
  dp$position$CAPM_return_D250 <- NA
  dp$position$CAPM_return_D125 <- NA
  dp$position$CAPM_return_D5 <- NA
  dp$position$CAPM_return_D23 <- NA  
  dp$position$jensen_alpha <- NA  
  dp$position$jensen_alpha_D1 <- NA  
  dp$position$jensen_alpha_D5 <- NA  
  dp$position$jensen_alpha_D23 <- NA  
  dp$position$jensen_alpha_D125 <- NA  
  dp$position$jensen_alpha_D250 <- NA   
  
  
  for(iAss in colnames(logR)){
    iAssPos <- which(dp$position$AlphaVantage == iAss)
    # beta entire time period
    dp$position$beta[iAssPos] <- stats::cov(logR[,"ACWI"], logR[,iAss]) / stats::var(logR[,"ACWI"])
    
    # beta last year
    dp$position[iAssPos, "beta_1Y"] <- stats::cov(logR[paste(lY,tD,sep='/'), 'ACWI'], logR[paste(lY,tD,sep='/'), iAss]) /
      stats::var(logR[paste(lY,tD,sep='/'), 'ACWI'])
    
    dm$logRetPort
    
    # standard deviations
    dp$position[iAssPos, "std_log_return"]  <- sd(logR[,iAss])
    dp$position[iAssPos, "std_log_return_1Y"] <- sd(logR[paste(lY,tD,sep='/'), iAss])
  }
  # compute stats for the entire Portfolio:
  dp$position['portfolio','beta'] <- stats::cov(logR[,"ACWI"], dm$logRetPort) / stats::var(logR[,"ACWI"])
  dp$position['portfolio','beta_1Y'] <- stats::cov(logR[paste(lY,tD,sep='/'), 'ACWI'], dm$logRetPort[paste(lY,tD,sep='/')]) /
  stats::var(logR[paste(lY,tD,sep='/'), 'ACWI'])
  dp$position['portfolio', "std_log_return"]  <- sd(dm$logRetPort)
  dp$position['portfolio', "std_log_return_1Y"] <- sd(dm$logRetPort[paste(lY,tD,sep='/')])
  

  while (is_empty(as.vector(dm$adjClose[lY,"ACWI"]))) { lY <- date+1}
  dp$position$'CAPM_return_D1' <- as.vector(coredata(tail(rf,1))) + dp$position$beta_1Y *
    as.vector(coredata(tail(logR[,"ACWI"],1)) - coredata(tail(rf,1)))
  rmD250 <- as.vector(dm$adjClose[tD,"ACWI"])/ as.vector(dm$adjClose[lY,"ACWI"])-1
dp$position$'CAPM_return_D250' <- as.vector(coredata(tail(rf,1))) + dp$position$beta_1Y*
    as.vector(( rmD250 - coredata(tail(rf,1))))

slY <- lubridate::ymd(index(tail(logR[,"ACWI"],1))) - lubridate::days(364/2)
while (is_empty(as.vector(dm$adjClose[lY,"ACWI"]))) { lY <- date+1}
rmD125 <- as.vector(dm$adjClose[tD,"ACWI"])/ as.vector(dm$adjClose[lY,"ACWI"])-1
dp$position$CAPM_return_D125 <- as.vector(coredata(tail(rf,1))) + dp$position$beta_1Y*
  as.vector(( rmD125 - coredata(tail(rf,1))))

  date <- lubridate::ymd(index(tail(logR[,"ACWI"],1))) - lubridate::days(5)
  while (is_empty(as.vector(dm$adjClose[date,"ACWI"]))) { date <- date+1}
  rmD5 <- as.vector(dm$adjClose[tD,"ACWI"]) / as.vector(dm$adjClose[date,"ACWI"])-1
  dp$position$'CAPM_return_D5' <- as.vector(coredata(tail(rf,1))) + dp$position$beta_1Y *
    as.vector(( rmD5 - coredata(tail(rf,1))))
    
  date <- lubridate::ymd(index(tail(logR[,"ACWI"],1))) - lubridate::days(30)
  while (is_empty(as.vector(dm$adjClose[date,"ACWI"]))) { date <- date+1}
  rmD23 <- as.vector(dm$adjClose[tD,"ACWI"])/ as.vector(dm$adjClose[date,"ACWI"])-1
  dp$position$'CAPM_return_D23' <- as.vector(coredata(tail(rf,1))) + dp$position$beta_1Y *
    as.vector(( rmD23 - coredata(tail(rf,1))))

 
  dp$position['portfolio','CAPM_return'] <- as.vector(coredata(tail(rf,1))) + dp$position['portfolio','beta_1Y'] *
    (coredata(tail(logR[,"ACWI"],1)) - coredata(tail(rf,1)))

  dp$position$'jensen_alpha' <- dp$position$return_D250 - dp$position$'CAPM_return_D250'
  dp$position$jensen_alpha_D1 <- dp$position$return_D1 - dp$position$CAPM_return_D1
  dp$position$jensen_alpha_D5 <- dp$position$return_D5 - dp$position$CAPM_return_D5
  dp$position$jensen_alpha_D23 <- dp$position$return_D23 - dp$position$CAPM_return_D23
  dp$position$jensen_alpha_D125 <- dp$position$return_D125 - dp$position$CAPM_return_D125
  dp$position$jensen_alpha_D250 <- dp$position$return_D250 - dp$position$CAPM_return_D250
  
  posPort <- which(dp$position$Name=="Portfolio:")
  dp$position$'beta'[posPort] <- replace(dp$position$'beta', is.na(dp$position$'beta'),0) %*% 
    replace(dp$position$SharePortfolio, is.na(dp$position$SharePortfolio),0)
  dp$position$'beta_1Y'[posPort] <- replace(dp$position$'beta_1Y', is.na(dp$position$'beta_1Y'),0) %*% 
    replace(dp$position$SharePortfolio, is.na(dp$position$SharePortfolio),0)
  
  # standard deviations
  
  mergIndex <- dp$position$AlphaVantage %in% names(dm$logRet)
  dp$position[posPort, "std_log_return"]  <- sd(dm$logRet %*% 
   dp$position$SharePortfolio[mergIndex])
  dp$position[posPort, "std_log_return_1Y"] <- sd(dm$logRet[paste(lY,tD,sep='/'),] %*%
    dp$position$SharePortfolio[mergIndex])
  
  dp$position$Category[posPort] <-'Portfolio:'
  dp$position <<- dp$position
  return()
}

# compute and export Risk measures
getRisk <- function(logR, nBoot=100){
  lY <- lubridate::ymd(index(tail(logR[,"ACWI"],1))) - lubridate::years(1)
  tD <- lubridate::ymd(index(tail(logR[,"ACWI"],1)))
  
  dp$position$Value <- replace(dp$position$Value, is.na(dp$position$Value),0)
  weight <- dp$position$Value[-length(dp$position$Value)] / sum(dp$position$Value[-length(dp$position$Value)])
  hVaR.index <- dp$position$AlphaVantage %in% colnames(dm$logRet)
  hVaR.weight <- dp$position$Value[hVaR.index] / sum(dp$position$Value[hVaR.index])
  # check share of portfolio included in the hVaR
  # sum(position$SharePortfolio[position$AlphaVantage %in% colnames(dm$logRet)], na.rm = T)
  # x <- colnames(dm$logRet) %!in%  dp$position$AlphaVantage 
  # colnames(dm$logRet)[x]
  
  #compute portfolio VaR
  dp$position$'VaR_HS_1Y' <- NA
  dp$position[dp$position$Name=="Portfolio:", "VaR_HS_1Y"] <- 
    compVaRPort(logR=dm$logRet, weight= hVaR.weight)
  #compute portfolio ES
  dp$position$ES <- NA
  dp$position[dp$position$Name=="Portfolio:", "ES"] <- 
    compESPort(logR=dm$logRet, weight= hVaR.weight)
  # compute incremental VaR
  dp$position$'incremental_VaR' <- NA
  dp$position[hVaR.index,'incremental_VaR'] <- incVaR(logR=dm$logRet, hVaR.weight= hVaR.weight)
  dp$position$'incremental_VaR_to_Port_VaR' <- NA
  dp$position[hVaR.index,'incremental_VaR_to_Port_VaR'] <- dp$position[hVaR.index,'incremental_VaR'] / 
    unlist(dp$position[dp$position$Name=="Portfolio:", "VaR_HS_1Y"])
  dp$position$'incremental_VaR_per_Value' <- NA
  dp$position[hVaR.index,'incremental_VaR_per_Value'] <- dp$position[hVaR.index,'incremental_VaR'] / dp$position$Value[hVaR.index]
  
  dp$position$'share_Portfolio_VaR_to_share_portfolio' <- NA
  dp$position[hVaR.index,'share_Portfolio_VaR_to_share_portfolio'] <- 
    dp$position[hVaR.index,'incremental_VaR_to_Port_VaR'] /
    dp$position[hVaR.index,'SharePortfolio']
  
  # compute marginal VaR
  dp$position$'marginal_VaR' <- NA
  dp$position[hVaR.index,'marginal_VaR'] <- mVaR(dm$logRet, hVaR.weight, dp$position, hVaR.index) 
  
  #compute individual VaR and ES 
  dp$position$'VaR_normal_std_1Y' <- NA
  dp$position$'VaR_Normal' <- NA
  dp$position$'VaR_HS_1Y' <- NA
  dp$position$'individual_VaR_Bootstrap' <- NA
  for(iAss in colnames(logR)){
    iAssPos <- which(dp$position$AlphaVantage == iAss)
    dp$position[iAssPos, "VaR_normal_std_1Y"] <- -2.33 * sd(logR[paste(lY,tD,sep='/'), iAss])
    dp$position[iAssPos, "VaR_Normal"] <- -2.33 * sd(logR[,iAss])
    dp$position[iAssPos, "VaR_HS_1Y"] <- unlist(quantile(logR[paste(lY,tD,sep='/'), iAss], 0.01))
    dp$position[iAssPos, "individual_VaR_Bootstrap"] <- mean(bootstrap::bootstrap(logR[,iAss], nBoot, theta=quantile, p=0.01)$thetastar)
  }
  
  dp <-
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
  portRisk <- rbind(portRisk, dp$position["portfolio",c("CloseDate","beta_1Y","beta","jensen_alpha","CAPM_return_D250","std_log_return_1Y","VaR bootstrap 05Quantile","VaR_bootstrap","VaR bootstrap 95Quantile",
                                                 "VolaN1_MSGARCH","MSGARCH_prob","ES_MSGARCH", "ES bootstrap 05Quantile","ES bootstrap" ,"ES bootstrap 95Quantile")])
  if(verbose) View(portRisk)
  save(file=paste0(here::here(),"/Data/portRisk.RData"), portRisk)
}


