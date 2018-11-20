# utility function that are useful 

view <- function(table=dp$risk){
  View(table %>% subset(
  !is.na(table$SharePortfolio) & table$SharePortfolio >0.001 | 
    table$Name=="Portfolio:"), title = 'open Positions')
  return(NULL)
}

'%!in%' <- function(x,y)!('%in%'(x,y))

exportToExcleOpen <- function(){
  wb <- openxlsx::loadWorkbook(file = fileName)
  openxlsx::writeData(wb, sheet='Risk', x=dp$risk[5:ncol(dp$risk)], startCol = 5, startRow = 2,  colNames =F)
  openxlsx::writeData(wb, sheet='Position', x=dp$position[6:ncol(dp$position)], startCol = 6, startRow = 2,  colNames =F)
  openxlsx::saveWorkbook(wb, fileName, overwrite = TRUE)
  system('open Port.xlsx -a "Microsoft Excel"')
  return(NULL)
}

sign_formatter <- formatter("span", 
                            style = x ~ style(color = ifelse(x > 0, "green", 
                                                             ifelse(x < 0, "red", "black"))))
sign_formatter(c(-1, 0, 1))


color_from_middle <- function (data, color1,color2) 
{
  max_val=max(abs(data))
  JS(sprintf("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'",
             max_val,color1,max_val,color1,color2,color2,max_val,max_val))
} 

  