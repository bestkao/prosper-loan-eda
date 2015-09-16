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
library(gridExtra)
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


#-------------------------------------------------------------------------------
## Data Wrangling with R

# extra datasets
# install.packages('devtools')
# devtools::install_github('rstudio/EDAWR')
library(EDAWR)
?storms
?cases
?pollution
?tb

storms$storm
storms$wind
storms$pressure
storms$date

cases$country
names(cases)[-1]
unlist(cases[1:3, 2:4])

storms$pressure / storms$wind

# Reshaping layouts of tables
# install.packages('tidyr')
library(tidyr)
?gather
?spread

# make observations from variables
cases
gather(cases, 'year', 'n', 2:4)

# make variables from observations
pollution
spread(pollution, 'size', 'amount')

# split columns
storms
storms2 <- separate(storms, date, c('year', 'month', 'day'), sep='-')
storms2

# merge columns
unite(storms2, 'date', year, month, day, sep='-')

# install.packages('dplyr')
library(dplyr)
?select
?filter
?arrange
?mutate
?summarise
?group_by

# install.packages('nycflights13')
library(nycflights13)
?airlines
?airports
?flights
?planes
?weather

# extract existing variables
storms
select(storms, storm, pressure)
select(storms, -storm)
select(storms, wind:date)

# extract existing observations
filter(storms, wind >= 50)
filter(storms, wind >= 50, storm %in% c('Alberto', 'Alex', 'Allison'))

# derive new variables (from existing variables)
mutate(storms, ratio=pressure/wind, inverse=ratio^-1)

# change the unit of analysis
pollution %>% summarize(median=median(amount), variance=var(amount))
pollution %>% summarize(mean=mean(amount), sum=sum(amount), n=n())

# sort
arrange(storms, wind)
arrange(storms, desc(wind))
arrange(storms, wind, date) # by wind, then by date

# Pipe operator
tb
select(tb, child:elderly)
tb %>% select(child:elderly)
filter(storms, wind >= 50)
storms %>% filter(wind >= 50)
storms %>% filter(wind >= 50) %>% select(storm, pressure)
storms %>% mutate(ratio=pressure/wind) %>% select(storm, ratio)

# Unit of Analysis
pollution %>% group_by(city) %>% summarize(mean=mean(amount),
                                           sum=sum(amount), n=n())
pollution %>% group_by(size) %>% summarize(mean=mean(amount))
pollution %>% ungroup()

tb
tb %>% group_by(country, year)
# Cases by country, year
tb %>% group_by(country, year) %>%
  summarise(cases=sum(child, adult, elderly, na.rm=TRUE))
# Cases by country
tb %>% group_by(country, year) %>%
  summarise(cases=sum(child, adult, elderly, na.rm=TRUE)) %>%
  summarise(cases=sum(cases, na.rm=TRUE))
# Total cases
tb %>% group_by(country, year) %>%
  summarise(cases=sum(child, adult, elderly, na.rm=TRUE)) %>%
  summarise(cases=sum(cases, na.rm=TRUE)) %>%
  summarise(cases=sum(cases, na.rm=TRUE))

# Joining data
y
z
bind_cols(y, z)
bind_rows(y, z)
union(y, z)
intersect(y, z)
setdiff(y, z)

songs
artists
left_join(songs, artists, by='name')

songs2
artists2
left_join(songs2, artists2, by=c('first', 'last'))

left_join(songs, artists, by='name')
inner_join(songs, artists, by='name')
semi_join(songs, artists, by='name')
anti_join(songs, artists, by='name')
#-------------------------------------------------------------------------------


## Wrangling Gapminder Data

# install.packages('XLConnect', dependencies=TRUE)
library(XLConnect)
pg <- readWorksheetFromFile('patent-grants.xlsx',
                            sheet=1, header=TRUE)

# convert colnames to numeric years
names(pg)[2:20] <- as.numeric(substring(names(pg)[2:20], 2))
names(pg)[1] <- 'country'
# gather by counts by year
pgByYear <- gather(pg, 'year', 'count', 2:20)
# convert patent counts to numerics
pgByYear$count <- as.numeric(pgByYear$count)


## Investigating the Gapminder data

# Histogram of Patents Granted by Year
ggplot(aes(x=year, y=count), data=pgByYear) +
  geom_histogram(color='black', fill='#099DD9', stat='identity') +
  scale_x_discrete(breaks=seq(1984, 2002, 2)) +
  xlab('Year') +
  ylab('Number of Patents')
ggsave('patent-histogram.png')

# Histogram of Patents Granted by Year Faceted by Country
ggplot(aes(x=year, y=count), data=pgByYear) +
  facet_wrap(~country) +
  geom_histogram(color='black', fill='#099DD9', stat='identity') +
  scale_x_discrete(breaks=seq(1984, 2002, 2))
ggsave('country-patent-histogram.png')

# Frequency Polygon of Patents Granted by Year Faceted by Country
ggplot(aes(x=year, y=count, group=country, color=country), data=pgByYear) +
  geom_freqpoly(stat='identity') +
  scale_x_discrete(breaks=seq(1984, 2002, 2)) +
  xlab('Year') +
  ylab('Number of Patents')
ggsave('country-patent-frequency-polygon.png')
#-------------------------------------------------------------------------------


## Anonymized Facebook Birthdays For My Friends
bd <- read.csv('friends-birthdays.csv')
bd$DOB <- as.Date(as.character(bd$DOB), '%Y%m%d')
bd <- separate(bd, DOB, c('year', 'month', 'day'), sep='-')

# How many people share your birthday? Do you know them?
# (Reserve time with them or save money to buy them a gift!)
nrow(bd[bd$month=='01' & bd$day=='28',])
# 1 Person

# Which month contains the most number of birthdays?
# How many birthdays are in each month?
bd %>% group_by(month) %>%
  summarise(cases=length(month))
# Jan  = 28 (most birthdays)
# Feb  = 15
# Mar  = 21
# Apr  = 24
# May  = 20
# June = 22
# July = 18
# Aug  = 17
# Sept = 22
# Oct  = 19
# Nov  = 17
# Dec  = 21

# Which day of the year has the most number of birthdays?
days <- bd %>% group_by(month, day) %>%
  summarise(cases=length(month))
days[days$cases==max(days$cases),]
# The following eigh days have highest number of birthdays (3):
# 01-23
# 01-31
# 02-20
# 04-19
# 06-08
# 06-23
# 07-09
# 08-26

# Do you have at least 365 friends that have birthdays on everyday
# of the year?
days
# Nope, ie. no birthday on 01-07