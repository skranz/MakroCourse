T = 100000
T.year = 1000

mu = 0.002 / T.year

logr = rnorm(T,mu,0.01)
logx = cumsum(logr)
x = exp(logx)

x = cumsum(runif(T,-1,1))
df = data.frame(t=1:T,x=x)
df$year = ceiling(df$t/T.year)
library(dplyr)
sdf = summarise(group_by(df,year), mx = mean(x))

plot(sdf$year, sdf$mx,type="l")
plot(sdf$year, log(sdf$mx),type="l")


plot(x,type="l")


