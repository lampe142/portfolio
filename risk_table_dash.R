## full risk table
dp$risk %>% subset(,select=names(dp$risk)[c(-1,-25,-26,-29,-30,-32,-33,-39,-40-42)]) %>% 
  plyr::arrange(!SharePortfolio)  -> risk.Table 

risk.Table <- replace(risk.Table, is.na(risk.Table),0)

datatable(risk.Table, options=list(pageLength = 100)) %>%
  formatRound('Value', digits = 0, interval = 3, mark = " ")%>%
  formatPercentage('SharePortfolio', 1) %>%
  formatStyle('SharePortfolio', background = color_from_middle(risk.Table$SharePortfolio,'pink','lightblue')) %>%
  formatRound('Beta', digits = 2, interval = 3, mark = " ")%>%
  formatStyle('Beta', background = color_from_middle(risk.Table$Beta,'pink','pink')) %>%
  formatRound('Beta1Y', digits = 2, interval = 3, mark = " ")%>%
  formatStyle('Beta1Y', background = color_from_middle(risk.Table$Beta1Y,'pink','pink')) %>%
  formatPercentage('250D.logReturn', 1) %>%
  formatStyle('250D.logReturn', background = color_from_middle(risk.Table$`250D.logReturn`,'pink','pink')) %>%
  formatPercentage('250D.CAPM.Return', 1) %>%
  formatStyle('250D.CAPM.Return', background = color_from_middle(risk.Table$`250D.CAPM.Return`,'pink','pink')) %>%
  formatStyle('Jensen.Alpha', background = color_from_middle(risk.Table$Jensen.Alpha,'pink','lightblue')) %>%
  formatRound('std_log_returns', digits = 3, interval = 3, mark = " ")%>%
  formatStyle('std_log_returns', background = color_from_middle(risk.Table$std_log_returns,'pink','pink')) %>%
  formatRound('std_log_returns_1Y', digits = 3, interval = 3, mark = " ")%>%
  formatStyle('std_log_returns_1Y', background = color_from_middle(risk.Table$std_log_returns_1Y,'pink','pink')) %>%
  formatPercentage('VaR Normal std1Y', 1) %>%
  formatStyle('VaR Normal std1Y', background = color_from_middle(risk.Table$`VaR Normal std1Y`,'pink','pink')) %>%
  formatPercentage('VaR Normal', 1) %>%
  formatStyle('VaR Normal', background = color_from_middle(risk.Table$`VaR Normal`,'pink','pink')) %>%
  formatPercentage('VaR HS 1Y', 1) %>%
  formatStyle('VaR HS 1Y', background = color_from_middle(risk.Table$`VaR HS 1Y`,'pink','pink')) %>%
  formatPercentage('individual VaR Bootstrap', 1) %>%
  formatStyle('individual VaR Bootstrap', background = color_from_middle(risk.Table$`individual VaR Bootstrap`,'pink','pink')) %>%
  formatPercentage('incremental VaR', 1) %>%
  formatStyle('incremental VaR', background = color_from_middle(risk.Table$`incremental VaR`,'pink','pink')) %>%
  formatPercentage('incremental VaR to Port VaR (share Portfolio VaR)', 1) %>%
  formatStyle('incremental VaR to Port VaR (share Portfolio VaR)', background = color_from_middle(risk.Table$`incremental VaR to Port VaR (share Portfolio VaR)`,'pink','pink')) %>%
  
  
  
  
  formatPercentage('individual VaR Bootstrap', 2) %>%
  formatStyle('individual VaR Bootstrap', background = color_from_middle(risk.Table$`individual VaR Bootstrap`,'pink','pink')) %>%
  formatPercentage('component_VaR', 2) %>%
  formatStyle('component_VaR', background = color_from_middle(risk.Table$component_VaR,'pink','pink')) %>%
  formatPercentage('VaR_Share', 2) %>%
  formatStyle('VaR_Share', background = color_from_middle(risk.Table$VaR_Share,'pink','pink')) -> risk.Table
risk.Table