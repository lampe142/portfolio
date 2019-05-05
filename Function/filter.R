
# logR=dm$logRet
filterMSGARCHLogRet <- function(level=0.99, verbose=T){
dp$MSGARCH <- list()
  for (i in names(dm$logRet)){
    dm$logRetFilMSGARCH1 <<- dm$logRet
    spec = MSGARCH::CreateSpec(variance.spec = list(model = c("sGARCH", "sGARCH")), 
                               distribution.spec = list(distribution = c("norm", "norm")))
    dp$MSGARCH[i] <- list(MSGARCH::FitML(spec = spec, data= dm$logRet[,i]))
    #save one step a head vola forecast
    dp$position[i,"VolaN1_MSGARCH"] <-  stats::predict(object=dp$MSGARCH[[i]], nahead=1)$vol
    # get state probability of last date
    dp$position[i, "MSGARCH_prob"] <- tail(State(dp$MSGARCH[[i]])$PredProb,1)
    
    # save VaR and Expected Shortfall
    MS_VaR_ES <- MSGARCH::Risk(object = dp$MSGARCH[[i]], nahead = 1L, alpha=(1-level))
    if(verbose) print(paste(i,c('VaR:','ES:'), round(as.numeric(MS_VaR_ES), digits=4)))
    dp$position[i,"ES_MSGARCH"] <- MS_VaR_ES$ES
    dp$position[i,"ind_VaR_MSGARCH"] <- MS_VaR_ES$VaR
    
    # retuns scaled by forecasted vola divideded by current
    dm$logRetFilMSGARCH1[,i] <- suppressWarnings(as.numeric(dp$position[i, "VolaN1_MSGARCH"]) * 
    (dm$logRet[,i] / MSGARCH::Volatility(dp$MSGARCH[[i]])))
  }
spec = MSGARCH::CreateSpec(variance.spec = list(model = c("sGARCH", "sGARCH")), 
                           distribution.spec = list(distribution = c("norm", "norm")))
dp$MSGARCHPort <- MSGARCH::FitML(spec = spec, data= dm$logRetPort)
#save one step a head vola forecast
dp$position['portfolio',"VolaN1_MSGARCH"] <-  stats::predict(object=dp$MSGARCHPort, nahead=1)$vol
# get state probability of last date
dp$position['portfolio', "MSGARCH_prob"] <- tail(State(dp$MSGARCHPort)$PredProb,1)
MS_VaR_ES_Port <- MSGARCH::Risk(object = dp$MSGARCHPort, nahead = 1L, alpha=(1-level))
if(verbose) print(paste(i,c('VaR:','ES:'), round(as.numeric(MS_VaR_ES_Port), digits=4)))
dp$position['portfolio',"ES_MSGARCH"] <- MS_VaR_ES_Port$ES
dp$position['portfolio',"ind_VaR_MSGARCH"] <- MS_VaR_ES_Port$VaR

dp <<- dp
  return()
}