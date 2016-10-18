library(ggplot2)
library(dplyr)
install.packages('reshape2')
install.packages('tidyr')
library(tidyr)



pf = read.csv("pseudo_facebook.tsv", sep = "\t")

pf$year_joined=as.integer(2014-(pf$tenure/365))

pf$year_joined.bucket = cut(x=pf$year_joined,breaks=c(2004,2009,2011,2012,2014))

pf.with_tenure = subset(pf, tenure>0)

pf.with_tenure$friending_rate = pf.with_tenure$friend_count / pf.with_tenure$tenure

pf.with_tenure$friend_init_per_day = pf.with_tenure$friendships_initiated / pf.with_tenure$tenure

ggplot(aes(x=tenure, y=friend_init_per_day), data=pf.with_tenure) + geom_smooth(aes(color=year_joined.bucket))

summary(pf.with_tenure$friending_rate)

ggplot(aes(x=age, y=median_friend_count), data=pf.fc_by_age_gender) + 
  geom_line(aes(color=year_joined.bucket)) +
  geom_line(stat="summary", fun.y = mean, linetype=2)

pf.fc_by_age_gender <- pf %>%
  filter(!is.na(gender)) %>%
  group_by(age,gender,year_joined.bucket) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n=n()) %>%
  ungroup() %>%
  arrange(age)

?spread

a=subset(pf.fc_by_age_gender, -median_friend_count)

pf.fc_by_age_gender.wide <- pf.fc_by_age_gender %>%
  select(-median_friend_count, -n) %>%
  spread(gender, mean_friend_count)

?geom_hline

ggplot(aes(x=age, y=female/male), data=pf.fc_by_age_gender.wide) + geom_line() + geom_hline(yintercept=1,linetype=2)

ggplot(aes(x=age, y=mean_friend_count), data=pf.fc_by_age_gender) + geom_line(aes(color=gender), stat='summary', fun.y=median)

