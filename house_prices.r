library(stringtools)
library(dplyr)
library(reshape2)
library(tidyr)

setwd("D:/lehre/makro")

dat <- read.csv("D:/lehre/makro/house prices real oecd.csv", stringsAsFactors=FALSE)
colnames(dat)[1] <- "period"

# Convert to long format
d = gather(dat,key = "region",value=price,-period)


d$year = as.numeric(substring(d$period,1,4))
d$quarter = as.numeric(str.right.of(d$period,":"))

d = arrange(d, year, quarter, region)

# take average price over years
d = summarise(group_by(d, year,region),
      price = mean(price)
    ) %>% ungroup

d = arrange(d,region, year)

d = mutate(group_by(d, region),
      lprice=  lag(price),
      dprice = price-lprice,
      gprice = (price-lprice)/lprice
    ) %>% ungroup
unique(d$region)

df = filter(d, region=="JPN")
# Autocorrelation of real house price differences
x = na.omit(diff(df$price))
acf(x)

ar1.diff = function(x) {
  dx = diff(x)
  lag_dx = lag(dx)
  cor(dx,lag_dx,use = "complete.obs")
}

# All first order auto correlations of differences in house prices
ac.df = summarise(group_by(d, region), dprice.ar1 = ar1.diff(price))
as.data.frame(ac.df)

acf
as.numeric(acf(x, lag.max=1, plot=FALSE)[[1]])

library(ggplot2)
ggplot(data=d, aes(x=year, y = gprice, color=region)) + geom_line(size=1.1) + geom_point() + facet_wrap(~region) + geom_hline(yintercept=0, colour="black")

ggplot(data=d, aes(x=year, y = price, color=region)) + geom_line(size=1.1) + facet_wrap(~region) + geom_hline(yintercept=100, colour="black")


plot(df$year,df$dprice)
