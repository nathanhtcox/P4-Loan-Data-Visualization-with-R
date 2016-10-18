install.packages('dplyr')
library(gridExtra)
library(ggplot2)
library(dplyr)

list.files()

?read.csv

age_groups <- group_by(pf, age)

pf.fc_by_age <- summarise(age_groups,
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())

ggplot(aes(x=age, y=friend_count_mean), data=pf.fc_by_age) + geom_line()


pf = read.csv("pseudo_facebook.tsv", sep = "\t")

pf$age_with_months <- pf$age + (12-pf$dob_month)/12

head(pf$age_with_months,20)

age_groups <- group_by(pf, age_with_months)

pf.fc_by_age_months <- summarise(age_groups,
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())

head(pf.fc_by_age_months)

ggplot(aes(x=age_with_months, y=friend_count_mean), data=pf.fc_by_age_months) + geom_line() + coord_cartesian(xlim=c(13,71),ylim=c(0,500))

ggplot(aes(x=age, y=friend_count), data=pf) + xlim(13,90) + 
  geom_point(alpha = 1/20, color='orange', position = position_jitter(h=0)) + 
  coord_trans(y='sqrt', limy=c(1,5000)) +
  geom_line(stat="summary", fun.y = mean) + 
  geom_line(stat="summary", fun.y = quantile(pf$friend_count, probs = 0.1), color='blue', linetype=2)

ggplot(aes(x=www_likes_received, y=likes_received), data=pf) +
  geom_point(alpha=1/20, color='orange') +
  coord_cartesian(xlim=c(0,10000), ylim=c(0,10000))

cor.test(x=pf$www_likes_received, y=pf$likes_received, method=c('pearson'))

cor.test(x=pf$age, y=pf$friend_count, method=c('pearson'))

ggplot(aes(x=age, y=friendships_initiated), data=pf) +
  geom_jitter(alpha=1/20)

str(pf)

p = qplot(x = friend_count, data = subset(pf, !is.na(gender)))

p

#p1 = qplot(x = friend_count, data = subset(pf, !is.na(gender))) +

p1 = p +  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50))

p2 = p + scale_x_log10()

p3 = p + scale_x_sqrt()

grid.arrange(p1,p2,p3)


qplot(x=gender, y=friendships_initiated, data=subset(pf, !is.na(gender)), geom = 'boxplot') + 
  coord_cartesian(ylim=c(0,200))

summary(pf$www_likes)
by(pf$www_likes, pf$gender, sum)

pf$mobile_check_in = ifelse(pf$mobile_likes > 0, 1, 0)

sum(pf$mobile_check_in)/nrow(pf)

install.packages('alr3')
library(alr3)
data(Mitchell)
?Mitchell
head(Mitchell, 20)

ggplot(aes(x=Month, y=Temp), data=Mitchell) + geom_point()

Mitchell$moy <- Mitchell$Month%%12
  
ggplot(aes(x=Month, y=Temp), data=Mitchell) + geom_point() + scale_x_discrete(breaks=seq(0,203,12))

cor.test(Mitchell$Month, Mitchell$Temp)
