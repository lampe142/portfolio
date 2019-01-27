
# logR <- dm$logRet
bootRisk <- function(logR, nBoot=100){
  hVaR.index <- dp$position$AlphaVantage %in% colnames(logR)
  port.index <- dp$position$Name=="Portfolio:"
  
  dp$position$Value <- replace(dp$position$Value, is.na(dp$position$Value),0)
  hVaR.weight <- dp$position$Value[hVaR.index] / sum(dp$position$Value[hVaR.index])
  
  bootResults <- bootstrap::bootstrap(1:dim(logR)[1], nBoot, theta=compRiskMes, 
                      weight=hVaR.weight, selPosValue = dp$position$Value[hVaR.index],
                       portValue = sum(dp$position$Value[hVaR.index]), ret=logR)
  
  VaR <-  sapply(1:100,function(x) bootResults$thetastar[[x]]$VaR)
  ES <-  sapply(1:100,function(x) bootResults$thetastar[[x]]$ES)
  incVaR <-  sapply(1:100,function(x) bootResults$thetastar[[x]]$incVaR)
  mVaR <-  sapply(1:100,function(x) bootResults$thetastar[[x]]$mVaR)
  individualVaR <- sapply(1:nBoot, function(x) bootResults$thetastar[[x]]$individualVaR)
  individualES <- sapply(1:nBoot, function(x) bootResults$thetastar[[x]]$individualES)
    
  # save marginal VaR
  dp$risk$`mVaR bootstrap`[hVaR.index] <<- rowMeans(mVaR, na.rm = FALSE, dims = 1)
  dp$risk$`mVaR bootstrap 05Quantile`[hVaR.index] <<- 
    apply(mVaR, 1, quantile, probs = c(0.05),  na.rm = TRUE)
  dp$risk$`mVaR bootstrap 95Quantile`[hVaR.index] <<- 
    apply(mVaR, 1, quantile, probs = c(0.95),  na.rm = TRUE)
  
  # save incremental VaR
  dp$risk$`incVaR bootstrap`[hVaR.index] <<- rowMeans(incVaR, na.rm = FALSE, dims = 1)
  dp$risk$`incVaR bootstrap 05Quantile`[hVaR.index] <<- 
    apply(incVaR, 1, quantile, probs = c(0.05),  na.rm = TRUE)
  dp$risk$`incVaR bootstrap 95Quantile`[hVaR.index] <<- 
    apply(incVaR, 1, quantile, probs = c(0.95),  na.rm = TRUE)
  
  #save individual VaR
  dp$risk$VaR_bootstrap[hVaR.index] <<- rowMeans(individualVaR, na.rm = FALSE, dims = 1) 
  dp$risk$`VaR bootstrap 05Quantile`[hVaR.index] <<- 
    apply(individualVaR, 1, quantile, probs = c(0.05),  na.rm = TRUE)
  dp$risk$`VaR bootstrap 95Quantile`[hVaR.index] <<- 
    apply(individualVaR, 1, quantile, probs = c(0.95),  na.rm = TRUE)
  
  #save portfolio VaR
  dp$risk$VaR_bootstrap[port.index] <<- mean(VaR)
  dp$risk$`VaR bootstrap 05Quantile`[port.index] <<- quantile(VaR, probs = 0.05,na.rm = T)
  dp$risk$`VaR bootstrap 95Quantile`[port.index] <<- quantile(VaR, probs = 0.95,na.rm = T)
  
  # save individual ES
  dp$risk$`ES bootstrap`[hVaR.index] <<- rowMeans(individualES, na.rm = FALSE, dims = 1)
  dp$risk$`ES bootstrap 05Quantile`[hVaR.index] <<- apply(individualES, 1, quantile, probs = c(0.05),  na.rm = TRUE)
  dp$risk$`ES bootstrap 95Quantile`[hVaR.index] <<- apply(individualES, 1, quantile, probs = c(0.95),  na.rm = TRUE)
  # save portfolio ES
  dp$risk$`ES bootstrap`[port.index] <<- mean(ES, na.rm = T)
  dp$risk$`ES bootstrap 05Quantile`[port.index] <<- quantile(ES,probs = c(0.05),na.rm=T)
  dp$risk$`ES bootstrap 95Quantile`[port.index] <<- quantile(ES,probs = c(0.95),na.rm=T)
  dp$risk$`component VaR` <<- dp$risk$`mVaR bootstrap` * dp$risk$SharePortfolio *100
  dp$risk$`VaR Share` <<- dp$risk$`mVaR bootstrap` * dp$risk$SharePortfolio / dp$risk$VaR_bootstrap[port.index]
  # dp$risk$`VaR_bootstrap`[port.index]
  # sum(dp$risk$`component VaR`,na.rm=T)
  
  return()
}

# compRiskMes(1:300, weight=hVaR.weight, selPosValue = position$Value[hVaR.index],
#          portValue = sum(position$Value[hVaR.index]), ret=dm$logRet)
compRiskMes <- function(x, weight=NULL, selPosValue, portValue, level=0.99, ret=dm$logRet){
  # compute VaR
  logR <- ret[x,]
  if(is.null(weight)) {weight <- rep(1/length(logR[1,]), length(logR[1,]))}
  
  # compute portfolio VaR
  VaR <- quantile(logR %*% weight , probs=(1-level))
  
  # compute individual VaR
  individualVaR <- apply(logR,2,function(x) quantile(x, prob=1-level))
  
  # compute individual VaR
  individualES <- apply(logR,2,function(x) mean(x[x < quantile(x , probs=(1-level))]))
  
  # compute Expected shortfall
  ES <- mean((logR %*% weight)[logR %*% weight < VaR])
  
  # compute marginal VaR
  mVaR <- numeric(length(weight))
  
  single_mVaR <- function(i){
    selPosValue2 <- selPosValue
    selPosValue2[i] <- selPosValue[i]+1
    newWeight <- replace(selPosValue2 / portValue, is.na(selPosValue2 / portValue),0)
    return(( quantile(logR %*% newWeight , probs=(1-level), na.rm=T) - VaR) * portValue)
  }
  mVaR <- sapply(X=c(1:length(selPosValue)), FUN=single_mVaR)
  
  # for(i in 1:length(selPosValue)){
  #   selPosValue2 <- selPosValue
  #   selPosValue2[i] <- selPosValue[i]+1
  #   newWeight <- replace(selPosValue2 / portValue, is.na(selPosValue2 / portValue),0)
  #   mVaR[i] <- ( quantile(logR %*% newWeight , probs=(1-level), na.rm=T) - VaR) * portValue
  # }
  
  # compute incremental VaR
  incVaR <- sapply(1:length(weight), FUN=function(x) VaR - quantile(logR %*% replace(weight, weight==weight[x], 0), probs=(1-level)))
  
  # incVaR <- numeric(length(weight))
  # for(i in 1:length(hVaR.weight)){
  #   incVaR[i] <- VaR - quantile(logR %*% replace(weight, weight==weight[i], 0), probs=(1-level))
  # }
  
  return(list(VaR=VaR, ES=ES, mVaR=mVaR, incVaR=incVaR, individualVaR=individualVaR, individualES=individualES))
}