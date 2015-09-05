getwd()
setwd('/Users/James/Dropbox/Projects/da/eda/pset3')
library(ggplot2)
library(ggthemes)
theme_set(theme_economist(12))

data(diamonds)
summary(diamonds)
?diamonds
head(diamonds$color)

# histogram of the price of all the diamonds in the diamond data set.
ggplot(aes(x=price), data=diamonds) +
  geom_histogram(color='black', fill='#099DD9') +
  xlab('Price ($USD)') +
  ylab('Number of diamonds for that price')

summary(diamonds$price)
sum(diamonds$price < 500)
sum(diamonds$price < 250)
sum(diamonds$price >= 15000)

# exploring the largest peak
ggplot(aes(x=price), data=diamonds) +
  geom_histogram(color='#099DD9', binwidth=1) +
  scale_x_continuous(limits=c(500,1000),
                     breaks=seq(500,1000,50)) +
  xlab('Price ($USD)') +
  ylab('Number of diamonds for that price')

ggsave('priceHistogram.png')

# Break out the histogram of diamond prices by cut.
ggplot(aes(x=price), data=diamonds) +
  geom_histogram(color='black', fill='#099DD9') +
  facet_wrap(~cut) +
  xlab('Price ($USD)') +
  ylab('Number of diamonds for given price and cut')

# Price stats by cut
by(diamonds$price, diamonds$cut, max)
by(diamonds$price, diamonds$cut, min)
by(diamonds$price, diamonds$cut, median)

# Scale y-axis for each cut
ggplot(aes(x=price), data=diamonds) +
  geom_histogram(color='black', fill='#099DD9') +
  facet_wrap(~cut, scales='free_y') +
  xlab('Price ($USD)') +
  ylab('Number of diamonds for given price and cut')

# Price per carat faceted by cut
ggplot(aes(x=price/carat), data=diamonds) +
  geom_histogram(color='black', fill='#099DD9', binwidth=0.05) +
  scale_x_log10() +
  facet_wrap(~cut, scales='free_y') +
  xlab('Price per carat') +
  ylab('Number of diamonds for given carat and cut')

# Price boxplots by cut, clarity, and color
library('gridExtra')
p1 <- ggplot(aes(x=factor(cut), y=price), data=diamonds) +
  geom_boxplot()
p2 <- ggplot(aes(x=factor(clarity), y=price), data=diamonds) +
  geom_boxplot()
p3 <- ggplot(aes(x=factor(color), y=price), data=diamonds) +
  geom_boxplot()
grid.arrange(p1, p2, p3)

by(diamonds$price, diamonds$color, summary)
by(diamonds$price, diamonds$color, IQR)

# Boxplot of price per carat across different colors
ggplot(aes(x=factor(color), y=price/carat), data=diamonds) +
  geom_boxplot()

# Carat Frequency Polygon
ggplot(aes(x=carat), data=diamonds) +
  geom_freqpoly(binwidth=.01) +
  scale_x_continuous(limits=c(0,2),
                     breaks=seq(0,2,.1)) +
  scale_y_continuous(limits=c(0,3000),
                     breaks=seq(500,3000,500))
