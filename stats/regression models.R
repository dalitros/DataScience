library(dplyr)
library(mechkar)
library(car)
library(tidyr)
library(tidyverse)

############################################
##  Linear Regression
############################################

############################
### Univariate Analysis 
############################

### cars

data(cars)

hist(cars$speed)
hist(cars$dist)

###### Model
mod1 <- lm(cars$dist ~ cars$speed)
pred1 <- predict(mod1)

##### plot the model. abline produce straight line. 
##### abline reg take the best
plot(cars$dist ~ cars$speed)
abline(reg=mod1, col="red")
abline(h=50, col="green")
abline(v=15, col="green")

###comparison of the prediction to the plot
plot(pred1 ~ cars$dist)

### linear regression function. 
yhat <- funtion(x) {
 return((3.9324 * x) + -17.5791)
}
yhat(4)

mod1$coefficients[2]

##### summary
summary(mod1)

Table2(mod1)

############################
### Multivariate Analysis 
############################

data(mtcars)

### model
mod2 <- lm(mpg ~ wt,data=mtcars)
pred2 <- predict(mod2)

plot(mtcars$mpg ~ mtcars$wt)
abline(reg=mod2, col="red")

plot(pred2 ~ mtcars$mpg)

summary(mod2)

######

### model with two variables
mod3 <- lm(mpg ~ wt + cyl,data=mtcars)
summary(mod3)
  
Table2(mod3)

### model with three variables
mod4 <- lm(mpg ~ wt + cyl + hp,data=mtcars)
summary(mod4)

Table2(mod4)

vif(mod4)

### multivariable model
mod5 <- lm(mpg ~ . ,data=mtcars)
summary(mod5)

plot(mtcars$cyl ~ mtcars$hp)

Table2(mod5)


### Colinearity test
vif(mod5)

### Plot all vs all
pairs(mtcars)

######################

bodyfat <- read.csv("C:/Users/dalit/DataScience/data/bodyfat.csv", sep="")
head(bodyfat)


bfmod1 <- lm(body_fat_pct ~ ., data=bodyfat)
pred_bf <- predict(bfmod1)
summary(bfmod1)

plot(pred_bf ~ bodyfat$body_fat_pct)
abline(reg=pred_bf, col="red")

vif(bfmod1)
summary(pred_bf)
####name all the names
nm <- (bfmod1)
###the difference between two vec
nm <- setdiff(nm,c("wrist","knee"))
as.character.numeric_version(nm)
data.frame(nm)
bfmod1 <- lm(body_fat_pct ~ ., data=bodyfat[,nm])

############################################
##   Logistic Regression
############################################

titanic <- TitanicSurvival
summary(titanic)

titanic <- titanic %>% filter(is.na(age)==FALSE)

#titanic$age <- ifelse(is.na(titanic$age)==TRUE,30,titanic$age)

### logistic regression model

mod6 <- glm(survived ~ age, data=titanic, family = "binomial")
summary(mod6)

pred6 <- predict(mod6,type="response")

hist(pred6)

summary(pred6)

table(surv=titanic$survived, pred=ifelse(pred6 >= 0.5,1,0))
table(surv=titanic$survived, pred=ifelse(pred6 >= 0.4,1,0))

########

mod7 <- glm(survived ~ age + sex, data=titanic, family = "binomial")
summary(mod7)

pred7 <- predict(mod7,type="response")

hist(pred7)

summary(pred7)

table(surv=titanic$survived, pred=ifelse(pred7 >= 0.5,1,0))

########

mod8 <- glm(survived ~ age + sex + passengerClass, data=titanic, family = "binomial")
summary(mod8)

pred8 <- predict(mod8,type="response")

hist(pred8)

summary(pred8)

table(surv=titanic$survived, pred=ifelse(pred8 >= 0.5,1,0))

Table2(mod8)

Table2.forestplot(mod8)

##########


happy <- read_csv("C:/Users/dalit/DataScience/data/SomervilleHappinessSurvey2015.csv")
head(happy)
summary(happy)

mod9 <-glm(D~., data=happy, family="binomial")
summary(mod9)
mod9

exp(mod9$coefficients)

mod9b <- step(mod9, direction = "forward")
summary(mod9b)

mod9c <- step(mod9, direction = "backward")
summary(mod9c)

mod9d <- step(mod9, direction = 'both')
summary(mod9d)
############################################
## Poisson Regression
############################################

df <- warpbreaks
summary(df)
head(df)
hist(df$breaks)

mod10 <- glm(breaks ~ ., data=df, family = "poisson")
summary(mod10)

pred10 <- predict(mod10,type="response")
hist(pred10)

plot(df$breaks ~ pred10)
##log transforming the data make it normal. 
hist(log(df$breaks, breaks=10))

mechkar::Table2(mod10)


