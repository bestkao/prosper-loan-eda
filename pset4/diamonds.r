setwd('~/Dropbox/Projects/da/eda/lesson4/')
library(ggthemes)
theme_set(theme_fivethirtyeight(12))

# scatterplot of price ($USD) vs x (length in mm)
ggplot(aes(x, price), data=diamonds) + geom_point()

# correlations
with(diamonds, cor.test(price, x)) # 0.88
with(diamonds, cor.test(price, y)) # 0.87
with(diamonds, cor.test(price, z)) # 0.86

# scatterplot of price ($USD) vs depth (in mm)
ggplot(aes(depth, price), data=diamonds) + geom_point()

# same scatterplot with transparency, rescaled x-axis breaks
ggplot(aes(depth, price), data=diamonds) + 
  geom_point(alpha=0.01) +
  scale_x_continuous(breaks=seq(0,79,2))

# compute correlation coefficient
with(diamonds, cor.test(depth, price)) # -0.01 (no correlation)

# scatterplot of price vs carat, omitting the top 1%
ggplot(aes(carat, price), data=diamonds) +
  geom_point() +
  xlim(0, quantile(diamonds$carat, .99)) +
  ylim(0, quantile(diamonds$price, .99))

# scatterplot of price vs volume
diamonds$volume <- with(diamonds, x*y*z)
ggplot(aes(volume, price), data=diamonds) +
  geom_point()

library(plyr)
count(diamonds$volume == 0) # 20 diamonds with volume == 0

# Correlation coefficient between reasonable diamond volumes and price
with(subset(diamonds, volume != 0 & volume < 800),
     cor.test(volume, price)) # 0.92

# Adjustments - price vs. volume w/linear model smoother
ggplot(aes(volume, price),
       data=subset(diamonds, volume != 0 & volume < 800)) +
  geom_point(alpha=0.1) +
  geom_smooth(method='lm', color='#CC2127')

# Create new diamondsByClarity dataframe
detach("package:plyr", unload=TRUE) # to avoid conflict w/dplyr's count()
library(dplyr)

diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarize(mean_price=mean(price),
            median_price=median(price),
            min_price=min(price),
            max_price=max(price),
            n=n())
diamondsByClarity

# Bar charts of mean price
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

library(gridExtra)
p1 <- ggplot(aes(clarity, mean_price), data=diamonds_mp_by_clarity) +
  geom_bar(stat='identity')
p2 <- ggplot(aes(color, mean_price), data=diamonds_mp_by_color) +
  geom_bar(stat='identity')
grid.arrange(p1, p2)
# Diamonds with less clarity and worse colors
# seem to be surprisingly more expensive.
