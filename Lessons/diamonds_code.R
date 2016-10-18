data("diamonds")

library(dplyr)

diamonds_by_clarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n())

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

clarity_bar=ggplot(aes(x=clarity, y=mean_price), data=diamonds_mp_by_clarity) + geom_bar(stat="identity")

color_bar=ggplot(aes(x=color, y=mean_price), data=diamonds_mp_by_color) + geom_bar(stat="identity")

grid.arrange(clarity_bar,color_bar)

?grid.arrange


diamonds
str(diamonds)
diamonds$color
?diamonds
?scale_x_continuous

ggplot(diamonds, aes(x=x, y=price), binwidth = 10) + geom_point() + scale_x_continuous(breaks=seq(50,70,2))

cor.test(diamonds$x,diamonds$price)
cor.test(diamonds$y,diamonds$price)
cor.test(diamonds$z,diamonds$price)

ggplot(aes(x=depth,y=price),data=diamonds) + geom_point(alpha=1/100) + scale_x_continuous(breaks=seq(50,70,2))

cor.test(diamonds$depth, diamonds$price)

?quantile

ggplot(aes(x=carat, y=price), data=diamonds) + 
  geom_point() + 
  coord_cartesian(xlim=c(0,quantile(x=diamonds$carat,probs=0.99))) 

diamonds$volume = diamonds$x * diamonds$y * diamonds$z

ggplot(aes(x=volume, y=price), data=d_scrub) + 
  geom_point(alpha=1/50) + 
  coord_cartesian(xlim=c(0,quantile(x=diamonds$volume,probs=0.99))) +
  geom_smooth()
  

d_scrub = subset(diamonds, volume!=0 & volume <= 800)
?subset

cor.test(d_scrub$volume, d_scrub$price)


quantile(x=diamonds$volume, probs=0.99)

ggplot(diamonds, aes(x=carat, y=price), binwidth = 10) + geom_boxplot() + facet_wrap(~color, scales = 'free')

ggplot(diamonds, aes(carat), binwidth = 0.01) + geom_freqpoly() + coord_cartesian(xlim=seq(0,5,0.1), ylim=seq(0,2000,100))

by(diamonds$price, diamonds$color, summary)

summary(diamonds$color)# + coord_cartesian(xlim = 2500)


cheap <- subset(diamonds,diamonds$price<500)
vcheap <- subset(diamonds, diamonds$price<250)
vcheap <- subset(diamonds, diamonds$price>=15000)
