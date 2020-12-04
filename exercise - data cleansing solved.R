
library(dplyr)
library(mechkar)
source("C:/Users/dalit/DataScience/R/mechkar.R")

df <- read.csv("C:/Users/dalit/DataScience/data/bank-full.csv",sep = ";")
head(df)
df

df2 <- read.csv("C:/Users/dalit/DataScience/data/bank.csv",sep = ";")

### to prevent problems in some functions, we changed the name of the 
### variable 'y' to 'category' and will drop the first one

df$category <- df$y
df$y <- NULL

### additionally, we convert all character variables to factor, as needed for mechkar

for(v in names(df)) {
  if(is.character(df[[v]])==TRUE) {
    df[[v]] <- factor(df[[v]])
  }
}

####################
###   Functions  ###
####################

outlierMatrix <- function(data,threshold=1.5) {
  vn <- names(data)
  outdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    if(is.numeric(data[[v]])) {
      outlow <- quantile(data[[v]],probs = 0.25,na.rm = T) 
      outhigh <- quantile(data[[v]],probs = 0.75, na.rm = T)
      irq_level <- (outhigh - outlow) * threshold
      outlow <- outlow - irq_level
      outhigh <- outhigh +  irq_level
      mv <- ifelse(data[[v]] < outlow | data[[v]] > outhigh, 1, 0)
      outdata[v] <- mv
    } else {
      mv <- rep(0,nrow(data))
    }
  }
  outdata$row1 <- NULL
  return(outdata)
}


missingMatrix <- function(data) {
  vn <- names(data)
  missdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    mv <- ifelse(is.na(data[[v]]),1,0)
    missdata[v] <- mv
  }
  missdata$row1 <- NULL
  return(missdata)
}


############
## 1) Summarize the dataset
############

summary(df)

############
## 2) Analyze the data using statistical analysis (Table1)
############

## a simple table with statistics for the whole dataset
tab1 <- Table1(data=df)
tab1

## a table with statistics for the whole dataset + stratified differences 
## among categories of one categorical variable (in this case, the outcome)
tab2 <- Table1(data=df, y="category")

############
## 3) Explore the data using graphics (exploreData / Sweetviz)
############

## EDA for all the variables in the dataframe
exploreData(data=df)

## EDA for all the variables in the dataframe and stratified by the outcome variable
exploreData(data=df, x=names(df), y="category")


############
## 4) Create a correlation matrix 
############

## to make the correlation matrix we need to select the numeric variables. 
## We use a loop for this

numvar <- NULL

for(v in names(df)) {
  if(is.numeric(df[[v]])==TRUE) {
    numvar <- c(numvar, v)
  }  
}

numvar

library(Hmisc)

cormat <- rcorr(as.matrix(df[,numvar]))

## correlation matrix
cormat$r

## p-values
round(cormat$P,3)

############
## 5) Plot the correlation using a correlation plot
############

## option 1:
heatmap(cormat$r)

## option 2 (a better one):
library(corrplot)
corrplot(cormat$r)
## customized
corrplot(cormat$r,type = "upper",method = "color")

############
## 6) Which variables have outliers?
############

## see the EDA generated with the exploreData to find the vars with outliers
## all numeric variables (out of day) seem to have outliers. Let's check them

## we drop the variable day
numvar <- setdiff(numvar,"day")
numvar

## lets create a graph with a grid of 2x3
par(mfrow=c(3,2))

## lets try with boxplots
for(v in numvar) {
  boxplot(df[[v]])
}

## lets check with scatter plots
for(v in numvar) {
  scatter.smooth(df[[v]] ~ seq(1,nrow(df)),xlab="index",ylab=v)
}

par(mfrow=c(1,1))

### there is a very strange behaviour on the data. Lets check the data 
### more in depth. We will generate an outlier's matrix (see functions section)

out1 <- outlierMatrix(df[,numvar])
head(out1)

## now we will check if there is some correlation among the outlier rows
corrplot(rcorr(as.matrix(out1))$r,type = "upper",method = "color")

## the unique high correlation is found between pdays and previous.
## we will get the values that are considered as outliers for each variables 
## and check if there is any relationship with the outcome variable.

## a) check the if the presence of outliers is related to the outcome
table(out1$age, df$category)
## outcome == yes in %
table(out1$age,df$category)[,2]/table(out1$age)*100

## its evident that for age there is affecting the outliers
## show histogram of age by outlier
agedf <- data.frame(age=df$age,age_out=out1$age)

library(ggplot2)
ggplot(agedf, aes(x=age, group=age_out, color=age_out)) +
  geom_density()

### unpaired t-test
t <- t.test(x=agedf$age[which(agedf$age_out==0)],y=agedf$age[which(agedf$age_out==1)],
       paired = FALSE)
t
t$p.value

## the p-value is significant => outliers are related with age !

## we can check this way on all the variables in an iterative way 
## and print the results into a pdf file

ttst <- NULL
pdf(file="eda - outliers.pdf")
for(v in numvar) {
  print(v)
  outdf <- data.frame(var=df[[v]],var_out=factor(out1[[v]]))
  g <- ggplot(outdf, aes(x=var, group=var_out, color=var_out)) +
    geom_density()
  plot(g)
  t <- t.test(x=outdf$var[which(outdf$var_out==0)],y=outdf$var[which(outdf$var_out==1)],
         paired = FALSE)
  print(t)
  ttst <- rbind(ttst, cbind(v,t$p.value))
}
## close the pdf
dev.off()
ttst

## as the results point to that that all the variables are affecting outliers, we assume
## we can't remove them

## we have to think tranforming the variables
hist(df$age)
hist(log(df$age)) # ***
hist(sqrt(df$age))

hist(df$balance)
hist(log(df$balance)) # ***
hist(sqrt(df$balance))

hist(df$duration)
hist(log(df$duration+1)) # ***
hist(sqrt(df$balance))

hist(df$campaign)
hist(log(df$campaign+1))
hist(sqrt(df$campaign))
hist(log(df$campaign+1)^1.3) # ***

hist(df$pdays)
hist(log(df$pdays+1)) 
hist(sqrt(df$balance))
hist(log(df$pdays+1)^2) # ***

hist(df$previous)
hist(log(df$previous+1)) 
hist(sqrt(df$previous))
hist(log(df$previous^2)) # ***

## some variables require some custom transformation to convert them
## to an approximate known distribution - we need to use a bit our
## imagination and try to find the best bet !

############
## 7) Create a missing matrix. Which variables have more missing values? Are there rows with many missings?
############

## even that this database seems to have no missings, there are many variables that have 
## have unknown variables: 

summary(df)
# education, contact, poutcome
# pdays has also a -1 that is very suspiceous...

plot(df$pdays ~ df$poutcome)
plot(df$pdays ~ df$contact)
plot(df$pdays ~ df$education)

df %>% group_by(poutcome) %>% 
  summarise(pdays_min=min(pdays),pdays_max=max(pdays),pdays_q99=quantile(pdays,0.99))

## as we can see, the pdays value -1 is highly related to the unknown value of poutcome
## thus, it can be used for missing values

## For the analysis, we have to conver these values to missing, the missingness of the
## categorical variables will be used for determining the missing of the numeric values,
## but we will do not need to impute them


df2 <- df
df2$poutcome <- ifelse(df2$poutcome == "unknown",NA,df2$poutcome)
df2$contact <- ifelse(df2$contact == "unknown",NA,df2$contact)
df2$poutcome <- ifelse(df2$poutcome == "unknown",NA,df2$poutcome)
df2$poutcome <- ifelse(df2$poutcome == "unknown",NA,df2$poutcome)
df2$pdays <- ifelse(df2$pdays == -1,NA,df2$poutcome)

summary(df2)

mss <- missingMatrix(df2)
summary(mss)

getMissingness(df2)

## both varibales, pdays and poutcome, have more than 80% of missingness. 
## We will not impute them. Even so, lets analyze the missing mechanism

### plot the matrix
library(naniar)
vis_miss(df2)

## As we can appreciate from the plot, pdays and poutcome missingness is highly correlated
table(mss$pdays, mss$poutcome)


#############
## 8) Use logistic regression to check if the missingness is related to any other variable
#############


df3 <- df2
df3$pday_na <- mss$pdays
df3$pdays <- NULL
## we will also need to remove the poutcome variable
df3$poutcome <- NULL


mod1 <- glm(pday_na ~. , data=df3, family="binomial")
summary(mod1)

## we found two variables tha may explain the missingness of the pdays and poutcome
## variables: campaign and the October category of month.
## So, in this case, both missing values can be considered as MNAR

## now lets check the contacts (just for fun, even we know it is a category and we will 
## not need to impute it)

df3 <- df2
df3$contact_na <- mss$contact
df3$poutcome <- NULL
df3$pdays <- NULL
df3$contact <- NULL
## we will also need to remove the poutcome variable


mod1 <- glm(contact_na ~. , data=df3, family="binomial")
summary(mod1)

## there are lots of variables that show significant influence on missingness
## for this variables. Thus, the mechanism is also considered to be MNAR

