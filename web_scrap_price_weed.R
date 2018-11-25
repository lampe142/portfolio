
# install.packages("rvest")
#Loading the rvest package
library('rvest')

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

