getwd()
setwd('~/Dropbox/Projects/da/eda/lesson2/')

statesInfo <- read.csv('stateData.csv')
View(statesInfo)

stateSubset <- subset(statesInfo, state.region == 1)
head(stateSubset, 2)
dim(stateSubset)

stateSubsetBracket <- statesInfo[statesInfo$state.region == 1, ]
head(stateSubsetBracket, 2)
dim(stateSubsetBracket)

# states with illiteracy rates <= 0.5%
subset(statesInfo, illiteracy <= 0.5)
# states with hs grad rates >= 50%
statesInfo[statesInfo$highSchoolGrad >= 50, ]

# factor variables
reddit <- read.csv('reddit.csv')
str(reddit)
table(reddit$employment.status)
summary(reddit)

# ordered factors
levels(reddit$age.range)
library(ggplot2)
qplot(data=reddit, x=age.range)
qplot(data=reddit, x=income.range)
reddit$age.range <- ordered(reddit$age.range,
                            levels=c('Under 18', '18-24', '25-34',
                                     '35-44', '45-54', '55-64', '65 or'))
qplot(data=reddit, x=age.range)
reddit$income.range <- factor(reddit$income.range,
                              levels=c('Under $20,000', '$20,000 - $29,999',
                                       '$30,000 - $39,999', '$40,000 - $49,999',
                                       '$50,000 - $69,999', '$70,000 - $99,999',
                                       '$100,000 - $149,999', '$150,000 or more'),
                              ordered = T)
qplot(data=reddit, x=income.range)
