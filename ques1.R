data <- read.delim("C:/Users/Deepika Chandrababu/Desktop/Course Materials/2122/MOSR/assignment2/StatisticsMWO/Question1_3.txt",header=TRUE)
data
boxplot(data$Tracker1,data$Tracker2,ylab="Performance", main = "Comparing the algorithms using BoxPlot",
        ylim = c(-10, 10),col=c('darkslategray4','darkorchid4'),names=c('Tracker1','Tracker2'),notch=TRUE)


x <- cumsum(data$Tracker1)
y <- cumsum(data$Tracker2)

plot(ecdf(x), 
     xlim = range(c(x, y)), 
     col = "red",
     xlab = "Stock values of Tracker1 & 2",
     ylab = "Performance",
     main = "Comparing the algorithms using cumsum")
plot(ecdf(y), 
     add = TRUE, 
     lty = "dashed",
     col = "darkblue") 
res <- wilcox.test(data$Tracker1, data$Tracker2,paired=FALSE, alternative="two.sided", 
                   conf.level=0.95 )
res
res$p.value

install.packages("effsize")
library(effsize)
data
Z=-100
N=10000
r = abs(Z)/sqrt(N)
r
res = cliff.delta(data$Tracker1, data$Tracker2)
print(res)
18158194/ 10000*10000
library(ggplot2)
mdata <- data.frame(data$Tracker1, data$Tracker2)
mdata
ggplot(mdata, aes(data.Tracker1,data.Tracker2 + geom_boxplot()+
                 scale_y_continuous(trans=pseudo_log_trans(),
                 breaks=c(-10,-1,0,1,10),
                 limits=c(-10,10))))

