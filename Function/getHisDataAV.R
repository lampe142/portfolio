getAllCurrentAV <- function(pauseTime=20){
  dp$position[,"curr_return_D1"] <- NA
  repeat{
    missingAss <-  dp$position$AlphaVantage[is.na(dp$position[,"curr_return_D1"])]
    print(paste('Assets missing:',missingAss))
    
    cat(crayon::red(paste('Number of Assets missing:',length(missingAss))))
    cat(crayon::red(paste(' representing :',round(length(missingAss) / 
                                                    length(dp$position$AlphaVantage),2) *100 ,'%')))
    for(iAss in missingAss){
      tryCatch({
        iAssCurr <- getCurrentAV(iAss,verbose = T)
        dp$position[iAss,"curr_return_D1"] <<- log(iAssCurr$close / tail(dp$FX$USEURO,1) / as.numeric(dp$position[iAss,"ClosePrice"]))
        # dp$dp$position[iAss,"curr.Date"] <- iAssCurr$date[[1]]
      }, error = function(e){
        Sys.sleep(pauseTime)
        dp$position[iAss,"curr_return_D1"] <<- NA
      })
    }
    if(rlang::is_empty(missingAss)) break()
  }
  return()
}


getCurrentAV <- function(symbolI='GWPH', verbose=F, os='compact'){
  url2 <- paste('https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&interval=5min&symbol=',symbolI,
                '&datatype=csv&apikey=QYRKHZUIGJT4NB0A&outputsize=',os,sep = '')
  
  x <- unlist(strsplit(rawToChar(httr::GET(url2)$content), ","))
  date <- strsplit(x[6][1], split='\n')[[1]][2]
  close <- as.numeric(x[10])
  if(verbose) print(paste(symbolI,' date: ',date,' price:', close))
  return(list(date=date, close=close))
}

addTodp <- function(downAssets, pauseTime=20){
  dp1 <- list()
  dp2 <- list()
  dp1$avAdjClose <- getPortData(pauseTime=20, itCycle=10, test=F, 
                                downAssets = downAssets)
  
  # download 2. try
  repeat{
    missingAss = downAssets[downAssets
      %!in% attributes(dp1$avAdjClose)$names]
    print(paste('Assets missing:',missingAss))
    
    cat(crayon::red(paste('Number of Assets missing:',length(missingAss))))
    cat(crayon::red(paste(' representing :',round(length(missingAss) / 
                                                    length(dp$position$AlphaVantage),2) *100 ,'%')))
    
    dp2$avAdjClose <- getPortData(pauseTime=25, itCycle=10, test=F, downAssets = missingAss)
    # merging both downloads
    dp1$avAdjClose <- append(dp1$avAdjClose, dp2$avAdjClose)
    if(rlang::is_empty(missingAss)) break()
  }
  dp$avAdjClose <<- append(dp$avAdjClose, dp1$avAdjClose)
  return()
}



getAllData <- function(pauseTime=20, FXsource='ECB'){
#  x <- getFX(source = 'ECB'); tail(x$USEURO)
  dp$FX <<- getFX(source = FXsource)

  dp1 <- list()
  dp2 <- list()
  dp1$avAdjClose <- getPortData(pauseTime=20, itCycle=10, test=F, 
    downAssets =dp$position$AlphaVantage[-length(dp$position$AlphaVantage)])
  
  iDownload <- 0
  # download 2. try
  repeat{
    missingAss = dp$position$AlphaVantage[
      dp$position$AlphaVantage[-length(dp$position$AlphaVantage)]
      %!in% attributes(dp1$avAdjClose)$names]
    print(paste('Assets missing:',paste(missingAss, collapse = ' ')))
    
    cat(crayon::red(paste('Number of Assets missing:',length(missingAss))))
    cat(crayon::red(paste(' representing :',round(length(missingAss) / 
      length(dp$position$AlphaVantage),2) *100 ,'%')))
    cat(crayon::bgMagenta(paste('Number Downloads runs:',iDownload)))
    iDownload <- iDownload + 1
    
    dp2$avAdjClose <- getPortData(pauseTime=25, itCycle=10, test=F, downAssets = missingAss)
    # merging both downloads
    dp1$avAdjClose <- append(dp1$avAdjClose, dp2$avAdjClose)
    if(rlang::is_empty(missingAss) | (iDownload > 50)) break()
  }
  dp$vola <<- getVola()
  dp$avAdjClose <<- dp1$avAdjClose
  return()
}

getHisDataAV <- function(symbolI='EEM', verbose=F, os='compact'){
  url2 <- paste('https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=',symbolI,
                '&apikey=QYRKHZUIGJT4NB0A&outputsize=',os,sep = '')
  x <- unlist(strsplit(rawToChar(httr::GET(url2)$content), ","))
  x3 <-  gsub(pattern = "\n", "",gsub(pattern = "\"", "",x[6:length(x)]))
  x4 <- reshape2::colsplit(string=x3, pattern =":", names=c("trt", "time"))
  
  dates <- as.Date(gsub(" ","", x4[seq(from=9, to=nrow(x4), by=8),1]))
  dates <- c(dates[1]+1, dates)
  closeAdj <- as.numeric(as.character(x4[seq(from=5, to=nrow(x4), by=8),2]))
  ts <- xts::xts(x=closeAdj, order.by = dates)
  
  if(verbose){ tail(ts) }
  return(ts)
}

getPortData <- function(pauseTime=15, itCycle=10, test=F, downAssets=NULL, verbose=F){
  assetsID <- dp$position
  avAdjClose <- list()
  
  os ='full'
  i=0
  dErr <- c()
# loop over all the positions to download the data
for (iAss in na.omit(downAssets)){
  i <- (i+1)
  if(verbose){
    print(paste(round(i/length(na.omit(downAssets)) *100,1),'% downloaded'))
    print(paste('AV Download asset: ',iAss,sep=''))
  }

# try download
  tryCatch({
    evalString <- paste0('avAdjClose$',iAss,' <- getHisDataAV(iAss, os=os)')
    eval(parse(text = evalString))
  }, error = function(e){
    
    if(verbose){
      print(paste0('Download ',iAss,' resulted in an Error'))
      print(paste('pause for',pauseTime,'sec. because error'))
    }

    Sys.sleep(pauseTime)
    # second donwload
    tryCatch({
      evalString <- paste0('avAdjClose$',iAss,' <- getHisDataAV(iAss, os=os)')
      eval(parse(text = evalString))
      if(verbose) print(paste0('2. Download ',iAss,' resulted in an success'))
    }, error = function(e){
      if(verbose) print(paste0('2. Download ',iAss,' resulted in an Error'))
    })
  })
  if(iAss=='ORHOF'){
    # try download
    iAss2 <- 'CNNRF'
    avAdjClose2 <- list()
    tryCatch({
      evalString <- paste0('avAdjClose2$',iAss2,' <- getHisDataAV(iAss2, os=os)')
      eval(parse(text = evalString))
    }, error = function(e){
      
      if(verbose){
        print(paste0('Download ',iAss2,' resulted in an Error'))
        print(paste('pause for',pauseTime,'sec. because error'))
      }
      
      Sys.sleep(pauseTime)
      # second donwload
      tryCatch({
        evalString <- paste0('avAdjClose2$',iAss2,' <- getHisDataAV(iAss2, os=os)')
        eval(parse(text = evalString))
        if(verbose) print(paste0('2. Download ',iAss2,' resulted in an success'))
      }, error = function(e){
        if(verbose) print(paste0('2. Download ',iAss2,' resulted in an Error'))
      })
    })
    avAdjClose$ORHOF <- rbind(avAdjClose$ORHOF, avAdjClose2$CNNRF)
  }
}
 return(avAdjClose)
}





################################################################################
# export
updatePositionRisk <- function(){
  dp$position$return_D1 <- NA
  dp$position$return_D5 <- NA  
  dp$position$return_D23 <- NA
  dp$position$return_D125 <- NA
  dp$position$return_D250 <- NA
  dp$position$ab_return_D1 <- NA
  dp$position$ab_return_D5 <- NA
  dp$position$ab_return_D23 <- NA
  dp$position$ab_return_D125 <- NA
  dp$position$ab_return_D250 <- NA
  
  for(iAss in names(dp$avAdjClose)){
    iAssPos <- which(dp$position$AlphaVantage == iAss)
    avAdjCloseEA <- eval(parse(text= paste("dp$avAdjClose$",iAss, sep=''))) / dp$FX$USEURO
    dp$position$CloseDate[iAssPos] <- as.Date(time(tail(avAdjCloseEA,1)))
    dp$position$ClosePrice[iAssPos] <- tail(avAdjCloseEA, n=1)
    avAdjlogRetEA <- diff(log(avAdjCloseEA), lag=1)[-1]
    lDate <- index(tail(avAdjCloseEA, n=1))
  
    dp$position$'return_D1'[iAssPos] <- as.numeric(tail(avAdjCloseEA[paste((lDate-1),lDate,sep='/')],1))/
      as.numeric(head(avAdjCloseEA[paste((lDate-1),lDate,sep='/')],1)) -1
    
    dp$position$'return_D5'[iAssPos] <- as.numeric(tail(avAdjCloseEA[paste((lDate-5),lDate,sep='/')],1))/
      as.numeric(head(avAdjCloseEA[paste((lDate-5),lDate,sep='/')],1)) -1
    
    dp$position$'return_D23'[iAssPos] <-
    as.numeric(tail(avAdjCloseEA[paste((lDate-30),lDate,sep='/')],1))/
      as.numeric(head(avAdjCloseEA[paste((lDate-30),lDate,sep='/')],1)) -1
    
    dp$position$'return_D125'[iAssPos] <- 
      as.numeric(tail(avAdjCloseEA[paste((lDate-365/2),lDate,sep='/')],1))/
      as.numeric(head(avAdjCloseEA[paste((lDate-365/2),lDate,sep='/')],1)) -1
    
    dp$position$'return_D250'[iAssPos] <- 
      as.numeric(tail(avAdjCloseEA[paste((lDate-365),lDate,sep='/')],1))/
      as.numeric(head(avAdjCloseEA[paste((lDate-365),lDate,sep='/')],1)) -1
  }
  dp$position$Value <- dp$position$Volume * dp$position$ClosePrice
  dp$position$ab_return_D1 <- dp$position$return_D1 * dp$position$Value / (1 + dp$position$return_D1)
  dp$position$ab_return_D5 <- dp$position$return_D5 * dp$position$Value / (1 + dp$position$return_D5)
  dp$position$ab_return_D23 <- dp$position$return_D23 * dp$position$Value / (1 + dp$position$return_D5)
  dp$position$ab_return_D125 <- dp$position$return_D125 * dp$position$Value / (1 + dp$position$return_D5)
  dp$position$ab_return_D250 <- dp$position$return_D250 * dp$position$Value / (1 + dp$position$return_D5)
  
  
  dp$position$SharePortfolio <- dp$position$Value / sum(dp$position$Value, na.rm =T)
  
  posPort <- which(dp$position$Name=="Portfolio:")
  dp$position$Value[posPort] <- sum(dp$position$Value[1:(posPort-1)], na.rm = T)
  dp$position$CloseDate[posPort] <- max(dp$position$CloseDate[-posPort])
  
  dp$position$'return_D1'[posPort] <- sum(dp$position$SharePortfolio[!is.na(dp$position$SharePortfolio)] * dp$position$'return_D1'[!is.na(dp$position$SharePortfolio)])
  dp$position$'return_D5'[posPort] <- sum(dp$position$SharePortfolio[!is.na(dp$position$SharePortfolio)] * dp$position$'return_D5'[!is.na(dp$position$SharePortfolio)])
  dp$position$'return_D23'[posPort] <- sum(dp$position$SharePortfolio[!is.na(dp$position$SharePortfolio)] * dp$position$'return_D23'[!is.na(dp$position$SharePortfolio)])
  dp$position$'return_D125'[posPort] <- sum(dp$position$SharePortfolio[!is.na(dp$position$SharePortfolio)] * dp$position$'return_D125'[!is.na(dp$position$SharePortfolio)])
  dp$position$'return_D250'[posPort] <- sum(dp$position$SharePortfolio[!is.na(dp$position$SharePortfolio)] * dp$position$'return_D250'[!is.na(dp$position$SharePortfolio)])
  dp$position$'ab_return_D1'[posPort] <- sum(dp$position$'ab_return_D1'[-posPort], na.rm = T)
  dp$position$Category[posPort] <-'Portfolio:'
  dp$position$CloseDate <- as.Date(dp$position$CloseDate)
  
  dp <<- dp
  return()
}

################################################################################
# merges log Returns with sufficient data
# fx = dp$FX$USEURO
# avAdjClose = dp$avAdjClose
mergeCloseRet <- function(fx = dp$FX$USEURO, avAdjClose=dp$avAdjClose, 
  thres=list(dataMinLength=250, nLastDays=5, nLastDaysWind=10)){
  if(is.null(thres))thres <- list(dataMinLength=400, nLastDays=5, nLastDaysWind=10)
  #check that of the last days is available
  sel <- logical(length(attributes(avAdjClose)$names))
  
  dp$position$AlphaVantage %in% attributes(avAdjClose)$names
  # check sufficient data is available
  for (iAss in 1:length(attributes(avAdjClose)$names)){
    sel[iAss] <- length(avAdjClose[[iAss]]) > thres$dataMinLength &
                 time(tail(avAdjClose[[iAss]],1)) > Sys.Date()-thres$nLastDays
  }
  samAss <- attributes(avAdjClose)$names
  avAdjCloseMer <- avAdjClose[[attributes(avAdjClose)$names[1]]]
  # merging close Prices
  for (iAss in attributes(avAdjClose)$names[sel]){
  #   print(iAss)
  #   print(tail(avAdjClose[[iAss]],1))
    avAdjCloseMer <- xts::merge.xts(avAdjCloseMer, avAdjClose[[iAss]], join='outer')
  }
  avAdjCloseMer <- avAdjCloseMer[,-1]
  avAdjCloseMer <-  xts::last(avAdjCloseMer, n=250*3)
  avAdjCloseMer <- xts(missForest::missForest(data.matrix(as.data.frame(avAdjCloseMer)))$ximp, order.by = index(avAdjCloseMer))

  colnames(avAdjCloseMer) <- attributes(avAdjClose)$names[sel]
  # adjusting for FX
  avAdjCloseMerFX = lapply(avAdjCloseMer, FUN = function(x) x/fx) 
  avAdjCloseMerFX <- do.call(cbind, avAdjCloseMerFX)
  colnames(avAdjCloseMerFX) <-  colnames(avAdjCloseMer)
  # computing log returns
  logReMer <- diff(log(avAdjCloseMerFX), lag=1)[-1]
  reMer <- diff(avAdjCloseMerFX, lag=1)[-1]
  # saving in the main list
  dm$adjClose <- avAdjCloseMerFX
  dm$ret <- reMer
  dm$logRet <- logReMer
  dm$assTicker <- colnames(logReMer)
  dm$assDate <- zoo::index(logReMer)
  
  dp$position$SharePortfolio <- replace(dp$position$SharePortfolio, is.na(dp$position$SharePortfolio),0)
  names(dm$adjClose)
  dp$position$SharePortfolio[names(dm$adjClose)]
  dm$adjClosePort <- xts(dm$adjClose %*%  dp$position$SharePortfolio[c(sel,F)], index(dm$adjClose[,1]))
  dm$logRetPort <- diff(log(dm$adjClosePort ), lag=1)[-1]
  
  dm <<- dm
  return()
}

################################################################################
# get volatility info
getVola <- function(verbose=F){
  
  volaTicker <- c('VIX','RVX')
  dp1 <- list()
  dp2 <- list()
  repeat{
    missingTicker <- volaTicker[volaTicker %!in% attributes(dp1)$names]
    print(paste('Assets missing:',missingTicker))
    
    if(verbose){
      cat(crayon::red(paste('Number of Assets missing:',length(missingTicker))))
      cat(crayon::red(paste(' representing :',round(length(missingTicker) / 
                                                      length(volaTicker),2) *100 ,'%')))
    }
    dp2 <- getPortData(pauseTime=25, itCycle=10, test=F, downAssets = missingTicker,verbose = T)
    # merging both downloads
    dp1 <- append(dp1, dp2)
    if(rlang::is_empty(missingTicker)) break()
  }
  return(dp1)
}
