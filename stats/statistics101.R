###########################################
####  
###########################################

# Data
x1 <- mtcars$cyl
x2 <- mtcars$mpg
x3 <- mtcars$wt
x4 <- mtcars$qsec

# Central Tendency Measures

### create a function that shows the most frequent element in a vector. 
### If there are more than one element that are the most frequent and has the same count, return all of them

MODE <- function(x) {
  freq <- table(x)
  Xmo <- names(freq[1])
    return(Xmo)
}

### create a function that shows the median of a given vector. 

MEDIAN <- function(x) {
  x <- x[order(x)]
  idx <- length(x)/2
  Xme <- (x[floor(idx)] + x[ceiling(idx)])/2
  return(Xme)
}

### create a function that shows the mean (average) of a given vector. 

AVG <- function(x) {
  Xmn <- sum(x)/length(x)
    return(Xmn)
}

#  Dispersion Measures

### Create a function that shows the range and in parenthesis the min-max values.

RANGE <- function(x) {
  rng <- paste(max(x)-min(x)," (",min(x),",",max(x),")",sep="")
    return(rng)
}

### Create a function that shows the Interquartile range (IQR) and in parenthesis the 25%-75% values.
### You can check the help for the 'quantile' function.

IQR <- function(x) {
  q25 <- quantile(x,probs = 0.25)
  q75 <- quantile(x,probs = 0.75)
  iqr <- paste(q75-q25, " (",q25," , ",q75,")",sep="")
  return(iqr)
}

### create a function that shows the variabce of a given vector. 

VAR <- function(x) {
  mu <- AVG(x)
  N <- length(x) 
  xdiff <- sum((x-mu)^2)
  Xvar <- xdiff/(N-1)
    return(Xvar)
}

### create a function that shows the standard deviation of a given vector. 

StdDev <- function(x) {
  mu <- AVG(x)
  N <- length(x) 
  xdiff <- sum((x-mu)^2)
  sigma <- sqrt(xdiff/(N-1))
  return(sigma)
}

### create a function that shows the standard error of a given vector. 

StdErr <- function(x) {
  N <- length(x)
  Xerr <- StdDev(x)/sqrt(N)
  return(Xerr)
}


### create a function that shows the confidence interval of a given proportion
### 
CIprop <- function(prop,alpha=0.05) {
  z <- qnorm(1 - (alpha/2))
  ci <- z * (prop*(1-prop))
  ci <- paste(prop," (",prop-ci,",",prop+ci,")",sep="")
  return(ci)
}

### create a function that shows the confidence interval of the mean for a given vector
### for a given alpha, we can calculate the critical value z using this formula:
###   z <- qnorm(1 - (alpha/2))

CImu <- function(x,alpha=0.05) {
  z <- qnorm(1 - (alpha/2))
  ci <- z * StdErr(x)
  ci <- round(ci,3)
  ci <- paste(AVG(x)," (",AVG(x)-ci,",",AVG(x)+ci,")",sep="")
  return(ci)
}

# Similarity Measures

### create a function that calculates the Pearson correlation statistic between two vectors

Pearson.corr <- function(x,y) {
  pcor <- cov(x,y)/(StdDev(x)*StdDev(y))
  return(pcor)
}

### create a function that calculates the Spearman correlation statistic between two vectors

Spearman.corr <- function(x,y) {
  require(dplyr)
  df <- data.frame(x=x,y=y)
  df <- df %>% arrange(x) 
  df$ox <- seq(1,nrow(df),1)
  df <- df %>% arrange(y) 
  df$oy <- seq(1,nrow(df),1)
  aggx <- df %>% group_by(x) %>% summarise(mox=mean(ox))
  aggy <- df %>% group_by(y) %>% summarise(moy=mean(oy))
  df <- inner_join(df, aggx)
  df <- inner_join(df, aggy)
  rho <- Pearson.corr(df$mox,df$moy)
  return(rho)
}

# Comparison Measures

### create a function that calculates the Chi-Square statistic between two vectors

df <- data.frame(color=c("Blue","Brown","Green","Orange","Red","Yellow"),
                 dist=c(0.24,0.13,0.16,0.20,0.13,0.14),
                 observed=c(481,371,483,544,372,369))

df$expected <- sum(df$observed) * df$dist


CHISQR <- function(obs,exp,alpha=0.05) {
  x2 <- sum((obs-exp)^2/exp)
  df <- length(obs)-1
  cv <- pchisq(1 - (alpha/2),df=df)
  ## results
  txt <- paste("Chi-square.test = ",round(x2,4)," - df = ", df," - Critical value = ",round(cv,4),". The difference is ",sep="")
  ## If the t-value is grater than the critical value we reject the null hypothesis = significant difference
  res <- ifelse(x2 > cv,paste(txt,"significant"),paste(txt,"non-significant"))
  return(res)
}

CHISQR(df$observed,df$expected)
chisq.test(x=df$observed,p=df$dist)



# Difference Measures

### create a function that calculates the t-test statistic between two vectors
TTEST <- function(x,y,alpha=0.05) {
  ## calculate the t-value
  tt <- (mean(x)-mean(y))/sqrt(((VAR(x)/length(x)))+(VAR(y)/length(y)))
  ## calculate the degrees of freedom
  df <- length(x)+length(y)-2
  ## calculate the critical value for the selected alpha and degrees of freedom
  tdist <- qt(1 - (alpha/2),df=df)
  ## results
  txt <- paste("t.test = ",round(tt,4)," - df = ", df," - Critical value = ",round(tdist,4),". The difference is ",sep="")
  ## If the t-value is grater than the critical value we reject the null hypothesis = significant difference
  res <- ifelse(tt > tdist,paste(txt,"significant"),paste(txt,"non-significant"))
  return(res)
}

age1 <- c(40,30,35,48,29,33,42,36,41,30)
age2 <- c(22,34,26,33,41,25,31,29,36,30)

TTEST(age1,age2)
t.test(age1,age2,var.equal=T)
