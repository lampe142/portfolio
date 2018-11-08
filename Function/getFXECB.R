getFX <- function(sDate = Sys.Date() - 3*365,
                        eDate = Sys.Date(),
                        source='FRED'){
  if(source=='ECB'){    ## Currencies
    # EUROCAD
    #  filter1 <- list(startPeriod = "2015-01-10", endPeriod = "2018-01-16", detail = 'full')
    #  EUROCAD <- get_data("EXR.M.CAD.EUR.SP00.E", filter1);
    #  EUROCAD <- xts(x=EUROCAD$obsvalue, order.by = as.Date(EUROCAD$obstime))
    # USEURO
    filter2 <- list(startPeriod = as.character(sDate),
                    endPeriod = as.character(eDate), detail = 'full')
    # US Dollar for many assets
    USEURO <- ecb::get_data("EXR.D.USD.EUR.SP00.A", filter2);
    USEURO <- xts(x=USEURO$obsvalue, order.by = as.Date(USEURO$obstime))
    # Hong Kong Dollar for Indonesia ETF position
    HKDEURO <- ecb::get_data("EXR.D.HKD.EUR.SP00.A", filter2);
    HKDEURO <- xts(x=HKDEURO$obsvalue, order.by = as.Date(HKDEURO$obstime))
    # Mexican Peso for Chile, France, India
    MXNEURO <- ecb::get_data("EXR.D.MXN.EUR.SP00.A", filter2);
    MXNEURO <- xts(x=MXNEURO$obsvalue, order.by = as.Date(MXNEURO$obstime))
    # CANNAROYALTY CORP. REGISTERED SHARES O.N.
    CADEURO <- ecb::get_data("EXR.D.CAD.EUR.SP00.A", filter2);
    CADEURO <- xts(x=CADEURO$obsvalue, order.by = as.Date(CADEURO$obstime))
    # ROBO GLOBAL(R)ROBOTICS AND AUTOMATION GO UCITS ETF
    CHFEURO <- ecb::get_data("EXR.D.CHF.EUR.SP00.A", filter2);
    CHFEURO <- xts(x=CHFEURO$obsvalue, order.by = as.Date(CHFEURO$obstime))

    key <- "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_1Y"
    rf1y <- ecb::get_data("YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_1Y", filter2);
    rf1y <- xts(x=rf1y$obsvalue/100, order.by = as.Date(rf1y$obstime))
    
    retList <- list(rf1y=rf1y, USEURO=USEURO, HKDEURO=HKDEURO, MXNEURO=MXNEURO, CADEURO=CADEURO, CHFEURO=CHFEURO)

    save(USEURO, HKDEURO, MXNEURO, CADEURO, CHFEURO, file = "Data/ECBcurrData.RData")
  } else if(source=='ECB_load'){
    load(file = "Data/ECBcurrData.RData", verbose=TRUE)
  }else if(source=='FRED'){
    raw <- suppressWarnings(na.omit(alfred::get_alfred_series("DEXUSEU", "test",
                                           observation_start = sDate, observation_end = eDate,
                                           realtime_start = eDate, realtime_end = eDate)))
    USEURO <- xts::xts(x=raw$test, order.by = as.Date(raw$date))
    raw <- suppressWarnings(na.omit(alfred::get_alfred_series("EUR12MD156N", "test",
                                             observation_start = sDate, observation_end = eDate,
                                             realtime_start = eDate, realtime_end = eDate)))
    rf1y <- xts::xts(x=raw$test, order.by = as.Date(raw$date))
    retList <- list(rf1y=rf1y, USEURO=USEURO)
  }
  
  return(retList)
}