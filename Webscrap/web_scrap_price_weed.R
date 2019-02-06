
setwd(here::here())
source('profileRep.R')
#source('send_email_profileRep.R')

US.all.States <- read.csv("Data/price_weed_states.csv", header = F)
load(file="Data/can_price.Rda")

# can.price <- data.frame(matrix(nrow = 1, ncol = 5))
# names(can.price) <- c('date', 'state', 'quality', 'price', 'sample.size')
  
for(iState in US.all.States$V1){
  paste0('http://www.priceofweed.com/prices/United-States/',iState,'.html') -> url
  #Reading the HTML code from the website
  read_html(url) %>% html_nodes(".avg_box , td") %>% html_text() -> text
  iRow <- nrow(can.price)
  if(iRow!=1)  iRow <- nrow(can.price) + 1

  #cbind(can.price, c(Sys.Date(), iState, text[6], text[5], text[7]))
  if(text[6]=="$N/A") text[6] <- NA
  can.price[iRow,'date'] <- Sys.Date()
  can.price[iRow,'state'] <- iState
  can.price[iRow,'quality'] <- text[5]
  can.price[iRow,'price']  <- as.numeric(gsub("\\$",'',text[6])) / 28.3495
  can.price[iRow,'sample.size'] <- as.numeric(text[7])
  iRow <-  iRow + 1
  can.price[iRow,'date'] <- Sys.Date()
  can.price[iRow,'state'] <- iState
  can.price[iRow,'quality'] <- text[8]
  can.price[iRow,'price']  <- as.numeric(gsub("\\$",'',text[9])) / 28.3495
  can.price[iRow,'sample.size'] <- as.numeric(text[10])
}

CA.all.States <- read.csv("Data/price_weed_states_ca.csv", header = F)
for(iState in CA.all.States$V1){
  paste0('http://www.priceofweed.com/prices/Canada/',iState,'.html') -> url
  #Reading the HTML code from the website
  read_html(url) %>% html_nodes(".avg_box , td") %>% html_text() -> text
  iRow <- nrow(can.price)
  if(iRow!=1)  iRow <- nrow(can.price) + 1
  
  #cbind(can.price, c(Sys.Date(), iState, text[6], text[5], text[7]))
  if(text[6]=="$N/A") text[6] <- NA
  can.price[iRow,'date'] <- Sys.Date()
  can.price[iRow,'state'] <- iState
  can.price[iRow,'quality'] <- text[5]
  can.price[iRow,'price']  <- as.numeric(gsub("\\$",'',text[6])) / 28.3495
  can.price[iRow,'sample.size'] <- as.numeric(text[7])
  iRow <-  iRow + 1
  can.price[iRow,'date'] <- Sys.Date()
  can.price[iRow,'state'] <- iState
  can.price[iRow,'quality'] <- text[8]
  can.price[iRow,'price']  <- as.numeric(gsub("\\$",'',text[9])) / 28.3495
  can.price[iRow,'sample.size'] <- as.numeric(text[10])
}
country <- read.csv("Data/Untitled.csv", header = F)
for(iState in country$V1){
  paste0('http://www.priceofweed.com/prices/',iState,'.html') -> url
  #Reading the HTML code from the website
  read_html(url) %>% html_nodes(".avg_box , td") %>% html_text() -> text
  iRow <- nrow(can.price)
  if(iRow!=1)  iRow <- nrow(can.price) + 1
  
  #cbind(can.price, c(Sys.Date(), iState, text[6], text[5], text[7]))
  if(text[6]=="$N/A") text[6] <- NA
  can.price[iRow,'date'] <- Sys.Date()
  can.price[iRow,'state'] <- iState
  can.price[iRow,'quality'] <- text[5]
  can.price[iRow,'price']  <- as.numeric(gsub("\\$",'',text[6])) / 28.3495
  can.price[iRow,'sample.size'] <- as.numeric(text[7])
  iRow <-  iRow + 1
  can.price[iRow,'date'] <- Sys.Date()
  can.price[iRow,'state'] <- iState
  can.price[iRow,'quality'] <- text[8]
  can.price[iRow,'price']  <- as.numeric(gsub("\\$",'',text[9])) / 28.3495
  can.price[iRow,'sample.size'] <- as.numeric(text[10])
}
can.price$date <- as.Date(can.price$date)
# View(can.price)
save(can.price,file="Data/can_price.Rda")
load(file="Data/can_price.Rda")
Sys.Date()-1
can.priceRound <-  can.price
can.priceRound$price <- round(can.price$price,4)

# can.priceRound <- can.priceRound[!can.priceRound$date=='2019-01-28',]

can.priceRound[!duplicated(can.priceRound[,c(-4,-5)]),] %>%
subset(state %in% country$V1 & quality =='High Quality',
              #         (date=='Sys.Date()'| date=='Sys.Date()-1'), 
                     select=c('date','state','price')) %>%
reshape::cast(date~state) -> priceCanT

priceCanT %>% tableHTML::tableHTML(caption ='price of weed') -> priceWeedBP

xts(order.by = priceCanT$date, x = priceCanT[,-1]) -> priceCanTimeS
names(priceCanTimeS) <- names(priceCanT)[-1]

sapply(2:7, function(x) quantmod::Delt(x1=priceCanT[,x], type = 'log', k=1))[-1,] %>%
xts(order.by = priceCanT$date[-1]) -> lgCannaTimes
colnames(lgCannaTimes) <-  names(priceCanT[-1])

round(lgCannaTimes*10000,2) %>% tableHTML::tableHTML(theme = "scientific", 
caption ='daily log return of cannabis price in pb') -> pcPriceWeedBP


dygraphs::dygraph(priceCanTimeS, main = "", width = 1800) %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyRangeSelector(height = 20) -> priceCanTimeSPlot


  html_bod <- paste0("<p> cannabis prices succes scrapped from priceofweed.coml. </p>", priceWeedBP,
                     "<p> cannabis prices succes scrapped from priceofweed.coml. </p>", pcPriceWeedBP)
  gmailr::mime() %>%
    gmailr::to("maxlampe@posteo.de") %>%
    gmailr::from("lampe142@googlemail.com")%>%
    gmailr::subject(paste('Cannabis price', Sys.Date()))%>%
    gmailr::html_body(html_bod)%>%
    gmailr::send_message()

  
  
  