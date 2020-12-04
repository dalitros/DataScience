
mammalsleep <- read_csv("DataScience/data/mammalsleep.csv")

summary(mammalsleep)

mammalsleep$species <- factor(mammalsleep$species)
summary(mammalsleep$species)

mammalsleep$pi <- factor(mammalsleep$pi)
mammalsleep$sei <- factor(mammalsleep$sei)
mammalsleep$odi <- factor(mammalsleep$odi)

cor(mammalsleep$bw, mammalsleep$brw, method = "spearman")
cor.test(mammalsleep$bw, mammalsleep$brw, method = "spearman")

cor.test(mammalsleep$bw, mammalsleep$sws, method = "spearman",na.rm=T)
cor.test(mammalsleep$bw, mammalsleep$ts, method = "spearman",na.rm=T)
cor.test(mammalsleep$bw, mammalsleep$mls, method = "spearman",na.rm=T)
cor.test(mammalsleep$brw, mammalsleep$mls, method = "spearman",na.rm=T)
cor.test(mammalsleep$bw, mammalsleep$gt, method = "spearman",na.rm=T)
cor.test(mammalsleep$mls, mammalsleep$gt, method = "spearman",na.rm=T)
cor.test(mammalsleep$mls, as.numeric(mammalsleep$odi), method = "spearman",na.rm=T)

library(Hmisc)
rcorr(as.matrix(mammalsleep[,3:9]),type = "spearman")

t.test(mammalsleep$ts, mammalsleep$ps,paired = TRUE)

mammalsleep$higrisk <- ifelse(mammalsleep$odi==4 | mammalsleep$odi==5,1,0)

table(mammalsleep$higrisk)

library(dplyr)

low <- mammalsleep %>% filter(higrisk ==0) %>% select(mls)
hi <- mammalsleep %>% filter(higrisk ==1) %>% select(mls)

t.test(low$mls, hi$mls, paired = FALSE)

chisq.test(mammalsleep$odi, mammalsleep$sei)
table(odi=mammalsleep$odi, sei=mammalsleep$sei)

####################

pie(mammalsleep$higrisk)
pie(table(mammalsleep$higrisk),labels = c("High","Low"),
    main="Pie Chart of Highrisk",radius = 1.5)

barplot(table(mammalsleep$higrisk))

library(ggplot2)

ggplot(data=mammalsleep) +
  geom_bar(aes(x=higrisk))

ggplot(data=mammalsleep) +
  geom_bar(aes(x=odi, group=factor(higrisk), color=factor(higrisk)))

plot(mammalsleep$bw,ylim=c(0,1000))
plot(mammalsleep$gt)
lines(mammalsleep$gt)

plot(mammalsleep$bw ~ mammalsleep$brw, xlim=c(0,1000), ylim=c(0,700))
plot(log(mammalsleep$bw+1) ~ log(mammalsleep$brw+1))

hist(mammalsleep$ts)
boxplot(mammalsleep$ts)

hist(mammalsleep$bw,breaks = 40)
boxplot(mammalsleep$bw)

hist(log(mammalsleep$bw))
boxplot(log(mammalsleep$bw))

boxplot(log(mammalsleep$bw) ~ mammalsleep$odi)

a1 <- boxplot(mammalsleep$bw)
a1$out

ggplot(data=mammalsleep) +
  geom_density(aes(x=log(bw), group=odi,color=odi))

pairs(mammalsleep[,3:13])

library(corrgram)
corrgram(mammalsleep[,2:13],upper.panel = panel.pie)

mcor <- rcorr(as.matrix(mammalsleep[,3:9]),type = "spearman")

heatmap(mcor$r)
heatmap(mcor$P)
