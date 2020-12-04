mammalsleep <- read_csv("C:/Users/dalit/DataScience/data/mammalsleep.csv")

summary (mammalsleep)
mammalsleep$species <- factor(mammalsleep$species)
summary (mammalsleep$species)
mammalsleep$pi <- factor(mammalsleep$pi)
mammalsleep$sei <- factor(mammalsleep$sei)
mammalsleep$odi <- factor(mammalsleep$odi)

cor(mammalsleep$bw, mammalsleep$brw, method="spearman")
cor.test(mammalsleep$bw, mammalsleep$sws, method="spearman", na.rm=T)

cor.test(mammalsleep$bw, mammalsleep$ts, method="spearman", na.rm=T)
cor.test(mammalsleep$bw, mammalsleep$mls, method="spearman", na.rm=T)
cor.test(mammalsleep$brw, mammalsleep$mls, method="spearman", na.rm=T)
cor.test(mammalsleep$brw, mammalsleep$gt, method="spearman", na.rm=T)
cor.test(mammalsleep$mls, mammalsleep$gt, method="spearman", na.rm=T)
library(Hmisc)
rcorr(as.matrix(mammalsleep[,3:9]),type="spearman")

t.test(mammalsleep$ts ,mammalsleep$ps, paired = TRUE)

mammalsleep$higrisk <- ifelse(mammalsleep$odi==4 | mammalsleep$odi==5,1,0)
table(mammalsleep$higrisk)
library(dplyr)

low <-mammalsleep %>% filter(higrisk==0) %>% select(mls)
hi <- mammalsleep %>% filter(higrisk==1) %>% select(mls)
t.test(low$mls, hi$mls, paired=FALSE)
chisq.test(mammalsleep$odi,mammalsleep$sei)
table(odi=mammalsleep$odi,sei=mammalsleep$sei)

##Graphs

pie(mammalsleep$higrisk)
library(ggplot2)
pie(table(mammalsleep$higrisk),lables=c("High","Low"),main="Pie Chart of Highrisk",radius=1.5)

barplot(table(mammalsleep$higrisk))
    
ggplot(data=mammalsleep)+
  geom_bar(aes(x=higrisk))
    
ggplot(data=mammalsleep)+
  geom_bar(aes(x=odi,group=factor(higrisk),color=factor(higrisk), fill="blue"))

ggplot(data=mammalsleep)+
  geom_bar(aes(x=odi,group=factor(higrisk),color=factor(higrisk), fill="yellow"))+
  scale_fill_manual("legend", values = "yellow", aesthetics = "fill"  )


ggplot(data=mammalsleep)+
  geom_bar(x="odi",group=factor(higrisk),width=0.5,color="blue", fill="white") +
  scale_fill_manual("legend", values = c("A" = "black", "B" = "orange", "C" = "blue"))


plot(mammalsleep$bw)

plot(mammalsleep$bw, ylim=c(0,2000))

plot(mammalsleep$gt)

lines(mammalsleep$bw)

plot(mammalsleep$bw ~ mammalsleep$brw, xlim=c(0,1000), ylim=c(0,700))

plot(log(mammalsleep$bw+1) ~ log(mammalsleep$brw+1))   

hist(mammalsleep$ts)
boxplot(mammalsleep$ts)

hist(mammalsleep$ts, breaks = 40)
boxplot(log(mammalsleep$ts))

boxplot(log(mammalsleep$bw)~mammalsleep$odi)

a1<-boxplot(mammalsleep$bw)
data(a1)
a1

ggplot (data=mammalsleep)+
  geom_density(aes(x=log(bw), group=odi, color=odi))

pairs(mammalsleep[,3:13])

library(corrgram)
corrgram(mammalsleep[,2:13])
corrgram(mammalsleep[,2:13], upper.panel=panel.pie)

mcor <- rcorr(as.matrix(mammalsleep[,3:9]),type="spearman")
heatmap(mcor$r)
