setwd('/Users/James/Dropbox/Projects/da/eda/pset5')
library(ggplot2)
library(ggthemes)
theme_set(theme_fivethirtyeight(12))
data(diamonds)

# histogram of diamond prices,
# faceted by color, colored by cut
ggplot(diamonds, aes(price, fill = cut)) +
  geom_histogram() +
  facet_wrap(~color) +
  scale_x_log10() +
  scale_fill_brewer(type = 'qual')


# scatterplot of diamond prices vs table, colored by cut
ggplot(diamonds, aes(table, price, color = cut)) +
  geom_point() +
  scale_x_continuous(limits = c(50, 80),
                     breaks = seq(50, 80, 2)) +
  scale_fill_brewer(type = 'qual')

summary(subset(diamonds, cut == 'Ideal'))
summary(subset(diamonds, cut == 'Premium'))


# scatterplot of diamond price vs volume (x * y * z),
# colored by clarity
diamonds <- transform(diamonds, volume = x * y * z)
ggplot(diamonds, aes(volume, price, color = clarity)) +
  geom_point() +
  xlim(0, quantile(diamonds$volume, .99)) +
  scale_y_log10() +
  scale_fill_brewer(type = 'div')


# create new variable 'prop_initiated' in pseudo-facebook dataset,
# containing proportion of friendships initiated over friend count
pf <- read.delim('pseudo_facebook.tsv')
pf <- transform(pf, prop_initiated = friendships_initiated / friend_count)


# line graph of the median proportion of friendships initiated vs. tenure
# colored by bins of year_joined
pf$year_joined <- floor(2014 - pf$tenure / 365)
pf$year_joined.bucket <- cut(pf$year_joined,
                             breaks=c(2004, 2009, 2011, 2012, 2014))

ggplot(subset(pf, !is.na(year_joined.bucket)),
       aes(tenure, prop_initiated)) +
  geom_line(aes(color = year_joined.bucket), stat ='summary', fun.y = median)


# Smoothed version
ggplot(subset(pf, !is.na(year_joined.bucket)),
       aes(tenure, prop_initiated)) +
  geom_line(aes(color = year_joined.bucket), stat ='summary', fun.y = median) +
  geom_smooth()

# mean of proportion of friendships initiated
# for group with largest proportion of friendships initiated
summary(subset(pf, year_joined.bucket == '(2012,2014]')$prop_initiated)

# scatterplot of price/carat ratio of diamonds
ggplot(diamonds, aes(cut, price / carat, color = color)) +
  geom_jitter() +
  facet_wrap(~clarity) +
  scale_color_brewer(type = 'div')

#-------------------------------------------------------------------------------


## Wrangling Gapminder Data

# install.packages('XLConnect', dependencies=TRUE)
library(XLConnect)
gdp <- readWorksheetFromFile('gdp-per-capita-growth.xlsx',
                            sheet=1, header=TRUE)

# convert colnames to numeric years
names(gdp)[2:51] <- as.numeric(substring(names(gdp)[2:51], 2))
names(gdp)[1] <- 'country'
# gather by growths by year
library(tidyr)
gdpByYear <- gather(gdp, 'year', 'growth', 2:51)
# convert growths to numerics
library(dplyr)
gdpByYear <- gdpByYear %>% mutate(growth = as.numeric(growth))


# fastest growing countries by average gdp/capita growth
gdpByYear <- gdpByYear %>%
  filter(!is.na(growth)) %>%
  group_by(country) %>%
  summarize(avg_growth = mean(growth)) %>%
  arrange(desc(avg_growth))

# scatterplot of avg growth vs country
ggplot(gdpByYear, aes(country, avg_growth)) +
  geom_point()
