data2 <- read.delim("C:/Users/Deepika Chandrababu/Desktop/Course Materials/2122/MOSR/assignment2/StatisticsMWO/Question2_4.txt",header=TRUE,stringsAsFactors=T)
head(data2)
data2$Failure[data2$Failure=="FALSE"]<- 0
data2$Failure[data2$Failure=="TRUE"] <- 1
your_model <- glm( formula=Failure ~ Temperature+CPU, data= data2, family = binomial)
summary(your_model)
library(oddsratio)
odds.ratio(your_model)


odds<-table(data2$CPU,data2$Failure)
ood<-table(data2$Temperature,data2$Failure)
odds
ood
fisher.test(odds)
fisher.test(ood)
hist(data2$CPU)
hist(data2$Temperature)
hist(data2$Failure)
##normality test
shapiro.test(data2$Failure)

isFALSE(data2$Failure)
Temperature<-data2$Temperature
CPU <- data2$CPU
Failure <- data2$Failure
plot(Temperature, Failure)
abline(glm(Failure ~ Temperature+CPU), lwd = 3)
points(Temperature, your_model$fitted+1, pch=1, col="red")

points(CPU, your_model$fitted+1, pch=1, col="green")

XT = xtabs(~ Failure + CPU, data=data2)

XT

library(ggplot2)
ggplot(data2, aes(x=CPU, y=Temperature, fill=Failure)) + geom_boxplot()

library(plyr)

cdat <- ddply(data2, "CPU", summarise, rating.median=median(Temperature))
rating.median=median(data2$Temperature)

# Density plots with median
ggplot(data2, aes(x=Temperature, fill=Failure)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.median,  colour=CPU),
             linetype="dashed", size=1)

##lessthan 10 percent
fail<-data2$Failure[data2$Temperature == 125]
fail
length(fail)
temp<- data2$Temperature[data2$Temperature == 125]
length(temp)
cpu<- data2$CPU[data2$Temperature == 125]
cpu
length(cpu)
median(temp)
median(fail)
dat <- data.frame(fail,temp,cpu)
dat
ggplot(dat, aes(x=cpu, y=temp, fill=fail)) + geom_boxplot()

rating.median=median(y)


