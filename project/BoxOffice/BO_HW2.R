library(DBI)
library(odbc)
library(Hmisc)
library(devtools)
library(ggplot2)
library(corrgram)
library(dplyr)

####################### Fix the type of the columns:
movie_ff$runtime <- as.numeric(movie_ff$runtime)
movie_ff$revenue <- as.numeric(movie_ff$revenue)
movie_ff$actors_cnt <- as.numeric(movie_ff$actors_cnt)
movie_ff$producers_cnt <- as.numeric(movie_ff$producers_cnt)
movie_ff$actor1_prev_revenue <- as.numeric(movie_ff$actor1_prev_revenue)
movie_ff$depart_Camera <- as.numeric(movie_ff$depart_Camera)
movie_ff$depart_Directing <- as.numeric(movie_ff$depart_Directing)
movie_ff$depart_Production <- as.numeric(movie_ff$depart_Production)
movie_ff$depart_Writing <- as.numeric(movie_ff$depart_Writing)
movie_ff$depart_Crew_female <- as.numeric(movie_ff$depart_Crew_female)
movie_ff$depart_Editing_female <- as.numeric(movie_ff$depart_Editing_female)
movie_ff$depart_Sound_female <- as.numeric(movie_ff$depart_Sound_female)
movie_ff$original_language <- factor(movie_ff$original_language)

movie_ff$budget <- as.numeric(movie_ff$budget)
movie_ff$release_month <- factor(movie_ff$release_month)
movie_ff$seasonality <- factor(movie_ff$seasonality)
movie_ff$countries_cnt <- as.numeric(movie_ff$countries_cnt)
movie_ff$actor0_prev_revenue <- as.numeric(movie_ff$actor0_prev_revenue)
movie_ff$depart_Art <- as.numeric(movie_ff$depart_Art)
movie_ff$depart_Custom_Mkup <- as.numeric(movie_ff$depart_Custom_Mkup)
movie_ff$depart_Lighting <- as.numeric(movie_ff$depart_Lighting)
movie_ff$depart_Visual_Effects <- as.numeric(movie_ff$depart_Visual_Effects)
movie_ff$depart_Camera_female <- as.numeric(movie_ff$depart_Camera_female)
movie_ff$depart_Directing_female <- as.numeric(movie_ff$depart_Directing_female)
movie_ff$depart_Production_female <- as.numeric(movie_ff$depart_Production_female)
movie_ff$depart_Writing_female <- as.numeric(movie_ff$depart_Writing_female)


####################### Analyze budget:
summary(movie_ff$budget)
table(movie_ff$budget)
sum(is.na(movie_ff$budget))
sd(movie_ff$budget, na.rm = TRUE)
nrow(table(movie_ff$budget))
max(table(movie_ff$budget))
calculate_mode(movie_ff$budget)
hist(movie_ff$budget, breaks = 50, xlim=c(0,3e08),  ylim=c(0,4000), na.rm = TRUE)
boxplot(movie_ff$budget, na.rm = TRUE)
plot(movie_ff$budget)
outliers <- boxplot(movie_ff$budget, plot=FALSE)$out
outliers
###correlation function
plot(movie_ff$revenue~movie_ff$budget, xlim=c(0,1.5e09),  ylim=c(0,1.5e09))
mod1 <- lm(movie_ff$revenue~movie_ff$budget)
pred1<-predict(mod1)
abline(reg=mod1, col="red")
summary(mod1)
cor.test(movie_ff$budget,movie_ff$revenue,method="spearman")
##log transformation in order to see the lower values
hist(log(movie_ff$budget), breaks = 50, na.rm = TRUE)
##relationship between budget and original_language
plot(movie_ff$budget ~ movie_ff$original_language)
#### calculate the IQR and outliers:
Q <- quantile(movie_ff$budget, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$budget, probs= .75, na.rm = TRUE) - quantile(movie_ff$budget3, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$budget, plot=FALSE)$out
outliers
#### check distribution without outliers
movie_ff$budget2 <- movie_ff$budget
movie_ff$budget2[movie_ff$budget2 < lower | movie_ff$budget2 > higher] <- NA
summary(movie_ff$budget2)
hist(movie_ff$budget2, breaks = 50, xlim=c(0,3e08),  ylim=c(0,4000), na.rm = TRUE)
hist(log(movie_ff$budget2), breaks = 50, xlim=c(0,20), na.rm = TRUE)
boxplot(movie_ff$budget2,  ylim=c(0,3e08),na.rm = TRUE)
###correlation function after NA removal 
plot(movie_ff$budget2 ~ movie_ff$revenue, xlim=c(0,1.5e09),  ylim=c(0,5e08))
mod12 <- lm(movie_ff$budget2 ~ movie_ff$revenue)
pred2<-predict(mod2)
abline(reg=mod2, col="red")
summary(mod2)
cor.test(movie_ff$budget2,movie_ff$revenue,method="spearman")




#### chanage 0 to NA
movie_ff$budget3 <- movie_ff$budget
movie_ff$budget3[movie_ff$budget3==0] <- NA
movie_ff$budget3

summary(movie_ff$budget3)
hist(movie_ff$budget3, breaks = 50, na.rm = TRUE)
boxplot(movie_ff$budget3, na.rm = TRUE)
hist(log(movie_ff$budget3), breaks = 50, na.rm = TRUE)
plot(movie_ff$budget3)
ggplot(data=movie_ff)+
  geom_smooth(mapping=
                aes(x=movie_ff$budget3,
                    y=revenue))

plot(log(movie_ff$budget3) ~ movie_ff$original_language, na.rm = TRUE)
plot(movie_ff$budget3 ~ movie_ff$revenue)
mod3 <- lm(movie_ff$budget3 ~ movie_ff$revenue)
pred3<-predict(mod3)
abline(reg=mod3, col="red")
summary(mod3)
cor.test(movie_ff$budget3,movie_ff$revenue,method="spearman")


#### calculate the IQR and outliers:
Q <- quantile(movie_ff$budget3, probs=c(.25, .75), na.rm = TRUE)
iqr<-quantile(movie_ff$budget3, probs= .75, na.rm = TRUE) - quantile(movie_ff$budget3, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$budget3, plot=FALSE)$out
outliers

#### check distribution without outliers
movie_ff$budget4 <- movie_ff$budget3
movie_ff$budget4[movie_ff$budget4 < lower | movie_ff$budget4 > higher] <- NA
summary(movie_ff$budget4)
hist(movie_ff$budget4, breaks = 50, na.rm = TRUE)
hist(log(movie_ff$budget4), breaks = 50, na.rm = TRUE)
boxplot(movie_ff$budget4, na.rm = TRUE)
ggplot(data=movie_ff)+
  geom_smooth(mapping=
                aes(x=movie_ff$budget4,
                    y=revenue))

plot(movie_ff$budget4 ~ movie_ff$revenue)
mod4 <- lm(movie_ff$budget4 ~ movie_ff$revenue)
pred4<-predict(mod4)
abline(reg=mod4, col="red")
summary(mod4)
cor.test(movie_ff$budget4,movie_ff$revenue,method="spearman")


movie_ff$budget <- as.numeric(movie_ff$budget)

Q <- quantile(movie_ff$actor2_movies_5y_cnt, probs=c(.25, .75), na.rm = TRUE)
iqr <- quantile(movie_ff$actor2_movies_5y_cnt, probs= .75, na.rm = TRUE) - quantile(movie_ff$actor2_movies_5y_cnt, probs= .25, na.rm = TRUE)
lower<-Q[1]-1.5*iqr
higher<-Q[2]+1.5*iqr
outliers <- boxplot(movie_ff$actor2_movies_5y_cnt, plot=FALSE)$out

#### check distribution without outliers
movie_ff$actor2_movies_5y_cnt2 <- movie_ff$actor2_movies_5y_cnt
movie_ff$actor2_movies_5y_cnt2[movie_ff$actor2_movies_5y_cnt2 < lower | movie_ff$actor2_movies_5y_cnt2 > higher] <- NA
hist(movie_ff$actor2_movies_5y_cnt2, breaks = 50, na.rm = TRUE)
movie_ff$actor2_movies_5y_cnt2 <- as.numeric(movie_ff$actor2_movies_5y_cnt2)
summary(as.numeric(movie_ff$actor2_movies_5y_cnt2))

ggplot(data = movie_ff)+ 
  geom_smooth(mapping = 
    aes(x=movie_ff$actor2_movies_5y_cnt2,
        y=revenue), na.rm = TRUE)
plot(movie_ff$revenue ~ movie_ff$actor2_movies_5y_cnt2)

ggplot(data=movie_ff)+
  geom_smooth(mapping=
                aes(x=movie_ff$budget4,
                    y=revenue))











#