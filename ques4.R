data4<-read.delim("C:/Users/Deepika Chandrababu/Desktop/Course Materials/2122/MOSR/assignment2/StatisticsMWO/Question4_1.txt",header=TRUE,stringsAsFactors=T)
head(data4)
data4$Static_Analysis[data4$Static_Analysis=="yes"] <- 0
data4$Static_Analysis[data4$Static_Analysis=="no"] <-1
qqnorm(data4$Times, pch = 1, frame = FALSE)
qqline(data4$Times, col = "steelblue", lwd = 2)

yes<-data4$Times[data4$Static_Analysis=="yes"] 
no<-data4$Times[data4$Static_Analysis=="no"]
length(yes)
length(no)
#p>o.5, the data is normal
shapiro.test(data4$Times)
t.test(yes,no,var.equal = TRUE)
t.test(yes,no)

#################Power and significance
library(effsize)
mean(yes)
mean(no)
sd(yes)
sd(no)
Sd1<-3.368151*3.368151
Sd1
sd2<-6.095536*6.095536
sd2
sdpooled<- sqrt(11.34444+37.15556)/2
sdpooled
cohen <- (31.4-25.3)/3.482097
cohen 

library(pwr)
analysis <- pwr.t.test(d = 1.8, sig.level = 0.05, power = 0.8)
#She needs to observe about a 1000 students.
analysis
plot(analysis)

subset1 <- yes[-1]; subno1 <- no[-1]
subset2 <- yes[-2]; subno2 <- no[-2]
subset3 <- yes[-3]; subno3 <- no[-3]
subset4 <- yes[-4]; subno4 <- no[-4]
subset5 <- yes[-5]; subno5 <- no[-5]
subset6 <- yes[-6]; subno6 <- no[-6]
subset7 <- yes[-7]; subno7 <- no[-7]
subset8 <- yes[-8]; subno8 <- no[-8]
subset9 <- yes[-9]; subno9 <- no[-9]
subset10 <- yes[-10]; subno10 <- no[-10]
subsampleyes1 <- yes[-1]; subsampleno1 <- no[-1]
subsampleyes2 <- yes[-2]; subsampleno2 <- no[-2]
subsampleyes3 <- yes[-3]; subsampleno3 <- no[-3]
subsampleyes4 <- yes[-4]; subsampleno4 <- no[-4]
subsampleyes5 <- yes[-5]; subsampleno5 <- no[-5]
subsampleyes6 <- yes[-6]; subsampleno6 <- no[-6]
subsampleyes7 <- yes[-7]; subsampleno7 <- no[-7]
subsampleyes8 <- yes[-8]; subsampleno8 <- no[-8]
subsampleyes9 <- yes[-9]; subsampleno9 <- no[-9]
subsampleyes10 <- yes[-10]; subsampleno10 <- no[-10]



d1<-sd(subsampleyes10);d2<-sd(subsampleno10)
Sd1<-d1*d1;sd2<-d2*d2
#Sd1#sd2
sdpooled<- sqrt(Sd1+sd2)/2
sdpooled
m1<-mean(subsampleyes10);m2<-mean(subsampleno10)
t.test(subsampleyes10,subsampleno10,var.equal = TRUE)
#cohen d=(m2-m1)/sdpooled
cohen <- (m2-m1)/sdpooled
cohen 

library(pwr)
analysis <- pwr.t.test(d =2.2, sig.level = 0.05, power = 0.8)
#She needs to observe about a 1000 students.
analysis
