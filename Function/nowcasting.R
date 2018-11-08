# functions necessary for nowcasting

transStationary <- function(data=nc$fed, spec=nc$fed.spec){
  transData <- data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)))
  colnames(transData) <- colnames(data)
  row.names(spec) <- spec$SeriesID
  transData$Date <- data$Date
  for (iSpec in spec$SeriesID){
    if(spec$Transformation[spec$SeriesID == iSpec] == 'chg'){
      if(spec$Frequency[spec$SeriesID == iSpec]=='q'){
        transData[!is.na(data[,iSpec]), iSpec] <- c(NA, diff(na.omit(as.matrix(data[,iSpec])), lag=1))
      } else if(spec$Frequency[spec$SeriesID == iSpec]=='m'){
        transData[-1, iSpec] <- diff(as.matrix(data[,iSpec]), lag=1)
      }
    } else if(spec$Transformation[spec$SeriesID == iSpec] == 'pchg'){
      if(spec$Frequency[spec$SeriesID == iSpec]=='q'){
        transData[!is.na(data[,iSpec]), iSpec] <- c(NA, diff(log(na.omit(as.matrix(data[,iSpec]))), lag=1))
      } else if(spec$Frequency[spec$SeriesID == iSpec]=='m'){
        transData[-1, iSpec] <- diff(log(as.matrix(data[,iSpec])), lag=1)
      }
    }else if(spec$Transformation[spec$SeriesID == iSpec] == 'lin'){
      transData[,iSpec] <- data[,iSpec]
    }
  }
  return(transData)
}