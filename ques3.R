data3<-read.table("C:/Users/Deepika Chandrababu/Desktop/Course Materials/2122/MOSR/assignment2/StatisticsMWO/Question3_1.txt",header=TRUE)
head(data3)

dat <- read.table("C:/Users/Deepika Chandrababu/Desktop/Course Materials/2122/MOSR/assignment2/StatisticsMWO/Question3_1.txt",header=TRUE)
dat
library(rcompanion)


data3$Interfaces = factor(data3$Interfaces,
                      levels=unique(data3$Interfaces))


### Create a new variable which is the likert scores as an ordered factor

data3$Responses.f = factor(data3$Responses.f,
                       ordered = TRUE)
#contingency table
XT = xtabs( ~ Interfaces + Responses,data = data3)
XT
prop.table(XT,margin = 1)

library(lattice)

histogram(~ Responses.f | Interfaces,
          data=data3,
          layout=c(1,3)      #  columns and rows of individual plots
)
kruskal.test( formula=Responses~Interfaces, data=data3)

library(FSA)

data3$Interfaces = factor(data3$Interfaces,
                      levels=c("type1", "type2", "type3"))

levels(data3$Interfaces)
as.numeric(data3$Interfaces)
data3$Responses.f<- as.numeric(data3$Responses.f)
DT = dunnTest(Responses.f ~ Interfaces,
              data=data3,
              method="bh")      # Adjusts p-values for multiple comparisons;
DT
PT = DT$res

PT

library(rcompanion)

cldList(P.adj ~ Comparison,
        data = DT,
        threshold = 0.05)

PT = pairwise.wilcox.test(data3$Responses.f,
                          data3$Interfaces,
                          p.adjust.method="fdr")
PT = DT$res

PT

library(rcompanion)

cldList(P.adj ~ Comparison,
        data = PT,
        threshold = 0.05)
data3$Responses.f
PT


Sum = groupwiseMedian(Responses.f ~ Interfaces,
                      data       = data3,
                      conf       = 0.95,
                      R          = 5000,
                      percentile = TRUE,
                      bca        = FALSE,
                      digits     = 3)

Sum
X     = 1:3
Y     = Sum$Percentile.upper + 0.2
Label = c("1", "2", "3")
library(ggplot2)

ggplot(Sum,                ### The data frame to use.
       aes(x = Interfaces,
           y = Median)) +
  geom_errorbar(aes(ymin = Percentile.lower,
                    ymax = Percentile.upper),
                width = 0.05,
                size  = 0.5) +
  geom_point(shape = 15,
             size  = 4) +
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) +
  
  ylab("Responses score") +
  
  annotate("text",
           x = X,
           y = Y,
           label = Label)
#effect size

eta = (43.837-3+1)/(300-3)
eta


#eta2[H] =(H - k + 1)/(n - k)
#where H is the value obtained in the Kruskal-Wallis test; k 
#is the number of groups; n is the total number of observations 