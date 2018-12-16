```{r include = TRUE, echo=FALSE}
dp$risk %>% subset((dp$risk$SharePortfolio>0.001 &
                      !is.na(dp$risk$SharePortfolio)) | dp$risk$Name == 'Portfolio:', 
                   select=c("Category",'Name',
                            'SharePortfolio',
                            'Beta',
                            'Jensen.Alpha',
                            'std_log_returns_1Y',
                            'individual VaR Bootstrap',
                            'component_VaR')) %>% 
  #           'VaR_Share')) %>% 
  plyr::arrange(!SharePortfolio) %>% 
  #  mutate(SharePortfolio = round(SharePortfolio,digits = 3)) %>% 
  mutate(Beta = round(Beta,digits = 3)) %>% 
  mutate(Jensen.Alpha = round(Jensen.Alpha,digits = 3)) %>% 
  mutate(std_log_returns_1Y = round(std_log_returns_1Y,digits = 3) * 100) %>% 
  dplyr::rename(vola_1Y = std_log_returns_1Y) %>% 
  mutate(component_VaR = round(component_VaR ,digits = 3)) %>% 
  mutate(`individual VaR Bootstrap` = round(`individual VaR Bootstrap`,digits = 3)) -> risk.Table 

risk.Table <- replace(risk.Table, is.na(risk.Table),0)


datatable(risk.Table,
          extensions = 'Buttons', options = list(pageLength = 100,
                                                 dom = 'Bfrtip',
                                                 buttons = list('copy', 'csv', 'excel', 'pdf', 'print',
                                                                list(extend = 'colvis',visible=F, targets= 0, columns = c(3:dim(risk.Table)[2])))
          )) %>%
  formatPercentage('SharePortfolio', 1) %>%
  formatStyle('SharePortfolio', background = color_from_middle(risk.Table$SharePortfolio,'pink','lightblue')) %>%
  formatStyle('Beta', background = color_from_middle(risk.Table$Beta,'pink','pink')) %>%
  formatStyle('Jensen.Alpha', background = color_from_middle(risk.Table$Jensen.Alpha,'pink','lightblue')) %>%
  formatStyle('vola_1Y', background = color_from_middle(risk.Table$vola_1Y,'pink','pink')) %>%
  formatPercentage('individual VaR Bootstrap', 2) %>%
  formatStyle('individual VaR Bootstrap', background = color_from_middle(risk.Table$`individual VaR Bootstrap`,'pink','pink')) %>%
  formatPercentage('component_VaR', 2) %>%
  formatStyle('component_VaR', background = color_from_middle(risk.Table$component_VaR,'pink','pink'))-> risk.Table
#  formatPercentage('VaR_Share', 2) %>%
# formatStyle('VaR_Share', background = color_from_middle(risk.Table$VaR_Share,'pink','pink')) 
risk.Table
```