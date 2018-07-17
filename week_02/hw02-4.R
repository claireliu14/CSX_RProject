library(ggmap)
library(mapproj)
map <- get_map(location = '台北市', zoom = 11, maptype = "roadmap")
ggmap(map)

library(maps)

data(us.cities)
mycountry <- unique(txhousing$city)
tx.cities <- subset(us.cities, country.etc == "TX")
tx.cities$city <- unlist(strsplit(tx.cities$name, " TX"))
m.txhousing <- subset(txhousing, year == 2000 & city %in% tx.cities$city)
temp <- tx.cities[tx.cities$city %in% m.txhousing$city, c("lat", "long")]
temp <- temp[rep(seq_len(nrow(temp)), each = 12), ]
m.txhousing.geo <- cbind(m.txhousing, temp)
head(m.txhousing.geo)

ggplot(m.txhousing.geo, aes(x = long, y = lat, size = sales, colour = cut(median, 5))) + 
  borders("county", "texas", colour = "grey70") +
  geom_point(alpha=.5) + 
  facet_wrap(~month) +  
  ggtitle("Housing market for populous cities in Texas (2000)") + 
  scale_colour_discrete(name  = "Median price") + 
  scale_size_continuous(name  = "Number of Sales") 

tx_center <- as.numeric(geocode("Texas"))
txMap <- ggmap(get_map(location = tx_center, zoom=6), extent="normal")
tx.cities.all <- subset(us.cities, country.etc == "TX")
txMap + geom_point(aes(x=long, y=lat, size = pop), col = "orange", 
                   data = tx.cities.all, alpha=0.4) +
  ggtitle("Population of Texas cities")