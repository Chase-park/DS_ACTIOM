
### install packages  ###
#install.packages('tidyverse')

library(tidyverse)



### load data ###
setwd('/Users/chsae/Desktop')

raw=read_csv('melonchart_pre (UTF-8).csv')

View(raw)

tail(raw)



## new variable: weeknum
for (i in seq(2011,2018,1)){
  for(j in seq(1,12,1))
  print(i)
}


raw$weeknum=1
for (i in 1:(nrow(raw)-1)){
  if (raw$week[i]!=raw$week[i+1]){
    raw$weeknum[(i+1):nrow(raw)]=raw$weeknum[i]+1
  }
}


## group by songs
id=raw$id

raw %>%
  arrange(id)

##  rmse
rmse=
  raw %>%
  group_by(id,title)%>%
  summarise(rmse=sqrt(mean(lm(rank~weeknum)$residuals^2)),count=sum(rank/rank))%>%
  arrange(desc(rmse)) %>%
  filter(count>10)


rmse




## plot function

rp= function(id1){
  
  a=raw[which(raw$id==id1),]
  
  plot(a$weeknum,a$rank,
      ylim=c(100,1),
      xlab='Week Number',
      ylab='Ranking',
      main= paste(raw[(which(raw$id==id1)),]$title[1],'-',raw[(which(raw$id==id1)),]$artist[1]),
      family="AppleGothic",
      col='blue',
      pch=16)
  
  coef=lm(a$rank~a$weeknum)$coef
  
  abline(coef[1],coef[2],col='red')
  
}  




rp(3973781)


## 
