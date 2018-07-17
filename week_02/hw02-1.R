library(ggplot2)
library(dplyr)
airpassengers_df <- data.frame(year = c(floor(time(AirPassengers) + .01)),
                               month = c(cycle(AirPassengers)),
                               data = c(AirPassengers))
head(airpassengers_df)
ggplot(airpassengers_df, aes(x=month, y=data)) + geom_line() + facet_wrap(~ year)

ggplot(data = txhousing, aes(x = median)) + geom_histogram()

ggplot(txhousing, aes(x=sales, y=median)) + geom_point()

mData <- txhousing %>%
  filter(year == 2000) %>%
  group_by(city) %>%
  summarize(totalSales = sum(sales, na.rm=TRUE)) %>%
  filter(totalSales >= 10000)
ggplot(mData, aes(x=reorder(city, -totalSales), y=totalSales)) + geom_bar(stat = "identity")

worldphone_df <- as.data.frame(as.table(WorldPhones))
names(worldphone_df) <- c("year", "area", "freq")
ggplot(worldphone_df, aes(x=year, y=freq, color=area, group=area)) + geom_line() + geom_point()

install.packages("GGally")
install.packages("memisc")
library(GGally) 
library(scales)
library(memisc) 

set.seed(20022012)
txhousing.samp <- subset(txhousing, select = -c(year,city))
txhousing.samp <- txhousing.samp[sample(1:length(txhousing$sales), 8000), ]
ggpairs(txhousing.samp,
        lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))