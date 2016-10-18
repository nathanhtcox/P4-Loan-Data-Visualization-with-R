install.packages('GGally')
library(GGally)

?ggpairs


data("diamonds")

diamonds <- transform(diamonds, volume=x*y*z)

diamonds <- transform(diamonds, price_per_carat = carat/price)

ggplot(aes(x=cut, y=price_per_carat), data=diamonds) + geom_jitter(aes(color=color)) +
  facet_wrap(~clarity)

ggplot(aes(x=volume, y=price), data=diamonds) + 
  geom_point(aes(color=clarity)) + 
  scale_y_log10() + 
  coord_cartesian(xlim=c(0,quantile(diamonds$volume, probs=0.99)))

?quantile

quantile(diamonds$volume, probs=0.99)

c(0,quantile(diamonds$volume, probs=0.99))

?scale_y_log10
