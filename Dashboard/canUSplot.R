# creating the plots
load(file="Data/can_price.Rda")

can.price$state <- gsub("-", " ", can.price$state)
can.price %>% subset(date=='2018-11-20' & quality == 'High Quality', select=c("state","price"))%>%
merge(y = state_codes,by.x='state',  by.y = "state_name", all.x = TRUE,no.dups = TRUE) %>%
subset(!is.na(state_abbr),select=c("state_abbr","price")) -> us.can.price.high
names(us.can.price.high) <- c("state",'price')

usmap::plot_usmap(data=us.can.price.high,values = "price", lines = "black") +
scale_fill_continuous(
  low = "red", high = "green", name = "weed price $/Oz.", label = scales::comma
) + theme(legend.position = "right") + labs(title = paste(Sys.Date(), 'Cannabis prices high quality'))
ggsave("Dashboard/us.can.price.high.png")

can.price %>% subset(date=='2018-11-20' & quality == 'Medium Quality', select=c("state","price"))%>%
  merge(y = state_codes,by.x='state',  by.y = "state_name", all.x = TRUE,no.dups = TRUE) %>%
  subset(!is.na(state_abbr),select=c("state_abbr","price")) -> us.can.price.medium
names(us.can.price.medium) <- c("state",'price')


usmap::plot_usmap(data=us.can.price.medium,values = "price", lines = "black") +
  scale_fill_continuous(low = "red", high = "green", name = "weed price $/Oz.", label = scales::comma) +
  theme(legend.position = "right") + labs(title = paste(Sys.Date(), 'Cannabis prices medium quality'))
ggsave("Dashboard/us.can.price.medium.png")

####### return
can.price$state <- gsub("-", " ", can.price$state)

can.price %>% subset((date==head(can.price$date,1) | date==tail(can.price$date,1)) & quality == 'High Quality', select=c("date","state","price"))%>%
  arrange(desc(date)) %>%
  group_by(state) %>% 
  mutate(pct_change = (price/lead(price) - 1) * 100) %>%
  subset( date==tail(can.price$date,1)) %>%
  merge(y = state_codes,by.x='state',  by.y = "state_name", all.x = TRUE,no.dups = TRUE) %>%
  subset(!is.na(state_abbr),select=c("state_abbr","pct_change")) -> us.can.pct_change.high
names(us.can.pct_change.high) <- c("state",'pct_change')


pct.title <-  paste('Cannabis high quality price change in % from',head(can.price$date,1)  , 'to ',tail(can.price$date,1))
usmap::plot_usmap(data=us.can.pct_change.high,values = "pct_change", lines = "black") +
  scale_fill_continuous(low = "red", high = "green", name = "weed price pct_change", label = scales::comma) +
  theme(legend.position = "right") + labs(title = pct.title)
ggsave("DashboardOutput/us.can.pct_change.high.png")


gmailr::mime() %>%
  gmailr::to("maxlampe@posteo.de") %>%
  gmailr::from("lampe142@googlemail.com")%>%
  gmailr::subject(paste('Cannabis price', Sys.Date()))%>%
  gmailr::attach_file(filename = 'DashboardOutput/us.can.pct_change.high.png')%>%
  gmailr::send_message()