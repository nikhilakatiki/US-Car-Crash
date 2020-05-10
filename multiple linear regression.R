
library(leaps)

prodata<-read.csv("Dataset2014.csv")
head(prodata)
sum(is.na(prodata))
prodata=na.omit(prodata)
dim(prodata)
str(prodata)

lm.all<- lm(Deaths~.-State,data=prodata)
lm.all
summary(lm.all)
plot(prodata)

windows(width =7, height = 9)
par(mfrow = c(2,2))
plot(lm.all,las=1)
plot(lm.all, 4)

newdata<-prodata[-c(41,44,10,33,9,5)]

regfit.full = regsubsets(Deaths~. -State, data=newdata,nvmax =6, method = "backward")
summary(regfit.full)
bwselect <-summary(regfit.full)
names(bwselect)
bwselect$adjr2

lm.bward <- lm(Deaths~Vmiles+Crashes, data=newdata)
summary(lm.bward)

windows(width =7, height = 9)
par(mfrow = c(2,2))
plot(lm.bward,las=1)

##Cook's Distance plot for checking outliers 
plot(lm.bward, 4)

sigma(lm.bward)/mean(newdata$Deaths)
  
confint(lm.bward)




