library(usmap)
library(USAboundaries)
library(ggplot2)

state_codes$state_name  
can.price$state <- gsub("-", " ", can.price$state)
can.price %>% subset(date=='2018-11-20' & quality == 'High Quality', select=c("state","price"))%>%
merge(y = state_codes,by.x='state',  by.y = "state_name", all.x = TRUE,no.dups = TRUE,) %>%
subset(!is.na(state_abbr),select=c("state_abbr","price")) -> us.can.price.high
names(us.can.price.high) <- c("state",'price')

usmap::plot_usmap(data=us.can.price.high,values = "price", lines = "black") +
scale_fill_continuous(
  low = "red", high = "green", name = "weed price", label = scales::comma
) + theme(legend.position = "right")

state_codes$state_name  
can.price$state <- gsub("-", " ", can.price$state)
can.price %>% subset(date=='2018-11-20' & quality == 'Medium Quality', select=c("state","price"))%>%
  merge(y = state_codes,by.x='state',  by.y = "state_name", all.x = TRUE,no.dups = TRUE) %>%
  subset(!is.na(state_abbr),select=c("state_abbr","price")) -> us.can.price.high
names(us.can.price.high) <- c("state",'price')

usmap::plot_usmap(data=us.can.price.high,values = "price", lines = "black") +
  scale_fill_continuous(
    low = "red", high = "green", name = "weed price", label = scales::comma
  ) + theme(legend.position = "right")
