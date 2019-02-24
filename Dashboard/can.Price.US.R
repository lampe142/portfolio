
load(file="Data/can_price.Rda")
# Load the ggplot2 and maps packages
dataset2 <- readxl::read_excel(path='Data/State-Data.xlsx') 
names(dataset2) <- c("StateName","state","Abb","Latitude","Longitude")
#str(dataset2)

## prepare data set
# Load the mapping data into a dataframe called states_map
states_map <- map_data("state")
can.price %>% subset(date==tail(can.price$date,1) & quality == 'High Quality', select=c("state","price"))-> dataset
dataset$state <- gsub("-", " ", tolower(dataset$state))
dataset <- merge(x = dataset, dataset2, by.x = 'state', by.y = 'state')[,-3]

can.price %>% subset((date==head(can.price$date,1) | date==tail(can.price$date,1)) & quality == 'High Quality', select=c("date","state","price"))%>%
  arrange(desc(date)) %>%
  group_by(state) %>% 
  mutate(pct_change = (price/lead(price) - 1) * 100) %>%
  subset( date==tail(can.price$date,1)) %>%
  merge(y = state_codes,by.x='state',  by.y = "state_name", all.x = TRUE,no.dups = TRUE) %>%
  subset(!is.na(state_abbr),select=c("state_abbr","pct_change")) -> us.can.pct_change.high
names(us.can.pct_change.high) <- c("state",'pct_change')
us.can.pct_change.high$pct_change <- round(us.can.pct_change.high$pct_change,2)

dataset <- merge(x = dataset, us.can.pct_change.high, by.x = 'Abb', by.y = 'state',  all.x = T )
dataset %>% subset(Abb!= 'HI' & Abb != 'AK') ->dataset

# set the colors depending on the price changes
dataset$Color <-  "Dark Green"
dataset$Color[dataset$pct_change<0] <- "Dark Red"
dataset$Color[is.na(dataset$pct_change)] <- "azure4"
dataset$pct_change[is.na(dataset$pct_change)] <- 0

# str(dataset)
# View(dataset)


titelCan <- paste("Cannabis Prices and returns from",head(can.price$date,1), 'to', tail(can.price$date,1)) 
# Start ggplot2 by sending it the dataset and setting the map_id variable to state
ggplot(dataset, aes(map_id = state)) +
  geom_map(map = states_map, aes(fill=price), show.legend = T) +
  labs(x = NULL, y = NULL,
       title =titelCan )  +
  scale_fill_gradient2(name = "# Sightings", midpoint = price,
                       low = "blue", mid = "purple",
                       high = "red") +
  geom_label( aes(x=Longitude, y=Latitude), colour="white", 
              fill=dataset$Color,label=dataset$pct_change, size=3) +
  expand_limits(x = states_map$long, y = states_map$lat) +   # define the x and y limits for the map
  scale_fill_gradient( low = "dark grey", high = "#115a9e") +   # define the color gradient for the state images
  labs(x=NULL, y=NULL) +   # remove all x and y axis labels
  theme_classic() +   # remove all grid lines
  theme(axis.line=element_blank(),  # remove other elements of the graph
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

ggsave(filename="DashboardOutput/us.states.png")
