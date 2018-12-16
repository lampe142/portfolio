install.packages("rvest")
#Loading the rvest package
library('rvest')

#Specifying the url for desired website to be scrapped
url <- 'https://tradingeconomics.com/'
#Reading the HTML code from the website
read_html(url) %>% html_nodes("tr~ tr+ tr a , th a") %>% html_text() -> text
View(text)

US.all.States <- read.csv("Data/price_weed_states.csv", header = F)
load(can.price, file="Data/can_price.Rda")


