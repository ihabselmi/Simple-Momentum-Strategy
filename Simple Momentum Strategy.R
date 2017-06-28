library(reshape2)
library(data.table)
library(tseries)
library(dplyr)
library(zoo)
library(ggplot2)


MoM= fread("sample.csv")
head(MoM)
MoM = select(MoM, date, year, month, tic, ret )%>%
  mutate(ret = as.numeric(ret), cum_ret = 0) %>%
  arrange(tic, year, month)
head(MoM)
for(i in 2:12)
{
  MoM = mutate(group_by(MoM, tic),
               cum_ret = (1 + cum_ret) * (1 + lag(ret,i)) - 1)
};

MoM
MoM = na.omit(MoM)%>%arrange(year, month)
MoM = transform(MoM, MoM_rank = ave(cum_ret, year, month, FUN = function(x) ntile(x,10)))

winner = filter(MoM, MoM_rank == 10) %>%
  select(date,ret)
loser = filter(MoM, MoM_rank ==1)%>%
  select(date, ret)
MoM_ret = inner_join(winner, loser, by = "date")%>%
  mutate(MoM = ret.x - ret.y)
mean(MoM_ret$MoM)
sd(MoM_ret$MoM)
MoM_ret = mutate(MoM_ret, cum_ret = 1+MoM, cum_ret = cumprod(cum_ret))

qplot(as.Date(MoM_ret$date), MoM_ret$cum_ret, xlab = "Date", ylab = "Dollar Value", color= I("red"), geom = "line", main = "If you invest 1 dollar in this portfolio since inception")+theme_grey()

