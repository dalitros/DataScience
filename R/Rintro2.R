##############################################################
##############  TCDS - R intro part 2   ######################
##############################################################


#############################################
#############  Internal functions
#############################################

### create a dataframe 
#df <- data.frame(model=as.character(),price=as.numeric(),mpg=as.integer())

df <- data.frame(model=c("2018 Toyota Camry Hybrid","2018 Ford Fusion Hybrid","2018 Toyota Prius",
                         "2018 Hyundai Ioniq","2018 Kia Optima Hybrid","2018 Ford C-Max Hybrid"))
df$price <- c(32400, 37370, 30565, 28300, 35210, 27275)
df$mpg <- c(52, 42, 52, 58, 43, 40)

df
summary(df)

#####################
###  Aggregation
#####################
min(df$price)
max(df$price)
mean(df$price)
sd(df$price)
median(df$price)
quantile(df$price,c(0.1,0.25,0.75,0.9))
sum(df$price)

########################
### Concatenation
########################

a <- c(10, 25, 8, 33, 12, 4, 28, 9, 11, 22)
b <- c(20, 30, 33, 28, 34, 21, 26, 29, 20, 25)

### bind rows
c <- rbind(a,b)
c
class(c)
str(c)
dim(c)
rownames(c)

### bind columns
d <- c("Yes","No","No","Yes","Yes","Yes","No","No","Yes","No")

e <- cbind(a,b,d)
e
class(e)
str(e)
dim(e)
colnames(e)

#### add a new column
df
df2 <- cbind(df, available=c(TRUE,FALSE,TRUE,TRUE,TRUE,FALSE))
df2
class(df2)
str(df2)
summary(df2)
names(df2)

## alternative way
df2$available <- c(TRUE,FALSE,TRUE,TRUE,TRUE,FALSE)

### concatenate strings
s1 <- paste("I","love","data","science")
s1
class(s1)
str(s1)
summary(s1)

s2 <- paste("I","love","data","science",sep="_")
s2
class(s2)
str(s2)
summary(s2)


s3 <- c("I","love","data","science")

s4 <- paste(s1,collapse=" ")
s4
class(s4)
str(s4)
summary(s4)

s5 <- paste(s1,collapse="+")
s5
class(s5)
str(s5)

s6 <- paste0("I","love","data","science")
s6
class(s6)
str(s6)

###################################
### Tranformation
###################################

### functions to transform existing variables
### In data science we use very widely the following transformations:

d1 <- c(1,2,3,4,5,6)
d2 <- d1^2 ## quadratic
d2
d1^3  ## cubic
sqrt(d2)  ## square root

d3 <- log(d1) ## natural (neperian) logarithm
d3
exp(d3)
log10(d1) ## decimal logarithm

d4 <- c(-5,-4,-3,-2,-1, 0, 1, 2, 3, 4, 5)
sign(d4)
abs(d4)

### geometric transformations
sin(c(10,30,90,180))
cos(c(10,30,90,180))
tan(c(10,30,90,180))

### other
pi <- 3.1415927
round(pi)
round(pi,1)
round(pi,2)
round(pi,4)

ceiling(pi)
floor(pi)

##############################################################
###   SYSTEM FUNCTIONS
##############################################################

ls()  ## show all the available objects existing in the environment
rm("C")  ## remove the C object

getwd()  ## get the working directory
setwd("/home/karpati/Rintro/")  ## set the working directory to the specified path

dt <- Sys.time()  ## get system date-time

dt
format(dt,format="%d-%m-%Y")  ## date format: '22-10-2014'
format(dt,format="%m/%d/%Y")  ## date format: '10/22/2014'
format(dt,format="%H:%M")     ## time format: '17:05'
format(dt,format="%I:%M %p")  ## time format: '05:05 PM'

timestamp()
date()

### Sequences
1:10
seq(1,10,2)
rep(0.5, 10)
length(c(1,2,3,4,5,6,7,8,9,10))

### randomization
sample(c(1,2,3,4,5,6,7,8,9,10),3)
for (n in 1:4) {
  print(sample(c(1,2,3,4,5,6,7,8,9,10),3))
}

set.seed(123)
for (n in 1:4) {
  print(sample(c(1,2,3,4,5,6,7,8,9,10),3))
}

set.seed(123)
for (n in 1:4) {
  print(sample(c(1,2,3,4,5,6,7,8,9,10),3))
}

runif(n=10, min=0, max=1)  ## 10 random numbers between 0 and 1
runif(n=5, min=-3, max=3)  ## 3 random numbers between -3 and 3

## normal distribution
rnorm(n=10, mean=5, sd=2)

## binomial distribution
rbinom(n=10, size=1, prob=0.4)

################################################################
######  Conditional expressions - return a boolean response
################################################################

a <- TRUE
b <- TRUE
c <- FALSE
d <- FALSE

a & b
a & c
c & d

a | b
a | c
c | d

a + b
a + c
c + d

a * b
a * c
c * d

a == b
a == c
c == d

cities = c('Jerusalem', 'Tel Aviv', 'Haifa')

'Jerusalem' %in% cities
'Holon' %in% cities

!('Jerusalem' %in% cities)  ## not in
!('Holon' %in% cities)      ## not in

############################################
### Apply and friends
############################################

### apply: apply a function recursively on a matrix or array
### MARGIN: how to apply the functions. 1=by rows, 2=by columns
### FUN: the function to apply. 
m1 <- matrix(c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5)),ncol=5,byrow=T)
m1
apply(X = m1, MARGIN = 1, FUN = sum)
apply(X = m1, MARGIN = 2, FUN = sum)


############
#### lapply: apply a function recursively on a list.. the result will be a list
############
m1 <- matrix(c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5)),ncol=5,byrow=T)
m2 <- matrix(c(2,1,2,2,5,3,5,4,5,5,5,1),ncol=3,byrow=T)
m3 <- matrix(c(8,8,8,8,6,6,6,6,4,4,4,4),ncol=4)
l1 <- list(m1=m1,m2=m2,m3=m3)
l1


lapply(l1, sum)
### We can use the selection operator `[` for extracting values at the same position
### Get the element of the third column of each element of a list 
lapply(l1, "[",,3)  

### Get the element of the first row of each element of a list 
lapply(l1, "[",1,)  

###########
### sapply: like lapply, but tries to simplify the output
### 
###########
l1

### We can use the selection operator `[` for extracting values at the same position
### the following extract the first value of each element in the list 
sapply(l1, "[",1,simplify = F)

### This will extract matrices containing the three first rows and columns of each element 
sapply(l1, "[",1:3,1:3,simplify = F)

### Simplify true will make the same, but joining the output into one unique matrix
sapply(l1, "[",1:3,1:3,simplify = T)

###########
### mapply: "multivariate" apply - apply a function to multiple objects at a time
###########
m1 <- matrix(c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5)),ncol=5,byrow=F)
m1

### We can generate the same matrix using mapply
m2 <- mapply(rep,1:5,5)
m2

m1 == m2

### generate a list of vectors containing letters repeated 4 times each 
mapply(rep, LETTERS[1:6], 4, SIMPLIFY = FALSE)

### what will this generate?
mapply(rep, LETTERS[1:6], 6:1, SIMPLIFY = FALSE)

#####################################################################
############  Programming with R
#####################################################################

##########################
### Conditional 
##########################

###  if (condition) { ... } 
x <- 0
y <- 0
if( x > 1 ) { y = 3 }
c(x,y)

###  if (condition) { ... } else { ... }
x <- 5
y <- 0
if( x > 5 ) { y = 10 } else { y = 5 }
c(x,y)

###  ifelse(condition, value1, value2) 
x <- 5
y <- ifelse(x > 5, 10, 5)
c(x,y)

###################################
### Recursive
###################################

### for(var in sequence) { ... }
y <- 1
for(x in 1:10) {
  y <- y + x
  print(c(x,y))
}
y

### exit prematurely from the for loop
for (x in 1:1000) {
  print(x)
  if(x==20) {
    break
  }
}

### while(condition) { ... }
y <- 10
x <- 1
while(x < 5) {
  y <- y * x
  x <- x + 1
  print(c(x,y))
}



### permanent recursion
x <- 0
while(TRUE) {
  x <- x + 1
  print(x)
}
## how can we prevent it from running infinitely? 

## repeat works like the last while example.
x <- 0
repeat {   
  x <- x + 1
  if (x == 1) {
    print("We are just beginning")
  } else if (x < 10) {
    print(paste("Counting...",x))
  } else {
    print("We are done !!!")
    break
  }
}


###########################################
###  Graphs in R
###########################################

x <- c(1,2,3,4,5,6,7,8)
y <- x + 15

plot(x, y) 

plot(x ~ y)  ## what happen with the axes?
lines(x ~ y)

plot(x, y, type='o') ## points and lines
plot(x, y, type='p') ## points
plot(x, y, type='l') ## lines
plot(x, y, type='b') ## both points and dotted lines
plot(x, y, type='c') ## dotted lines
plot(x, y, type='s') ## steped lines
plot(x, y, type='n') ## no dots or lines

###### adding horizontal and verical lines to a graph
plot(iris$Sepal.Length ~ iris$Sepal.Width)
## lets add to the graph a vertical line at the mean septal lenght
abline(h=mean(iris$Sepal.Length), col="red")
## and an horizontal line at the mean septal with
abline(v=mean(iris$Sepal.Width), col="red")

##############################
###### plot parameters
##############################

### defining the x and y limits
plot(x=NULL, xlim=c(1,10), ylim=c(1,11))

### line width
y <- 1 
for(n in seq(0.5,5,0.5)) {
  abline(h=y, lwd=n)
  y <- y + 1
}
### write the values of the line width 0.2 points over the line
text(x=rep(3,9),y = seq(1.2,10.2,1),labels = paste("lwd=",seq(0.5,5,0.5)))


### line type (6 types)
plot(x=NULL, xlim=c(1,7), ylim=c(1,7), xlab="X", ylab="Y")
y <- 1 
for(n in 1:6) {
  abline(h=y, lty=n)
  y <- y + 1
}
### write the values of the line width 0.2 points over the line
text(x=rep(3,6),y = seq(1.2,6.2,1),labels = paste("lty=",1:6))

### point symbols: there are 35, 25 accessible by numbers (1-25) and 10 accessible by 
### symbols ('*','+','-','.','|','%','#','o','O','0')

plot(x=NULL, xlim=c(1,5), ylim=c(0,6), xlab="X", ylab="Y")
for(x in 1:5) {
  for(y in 1:5) {
    p <- y+(5*(x-1))
    points(x,y,pch=p)
    text(x,y+0.2,labels=paste("pch=",p),cex=0.7)
  }
}


q <- c('*','+','-','.','|','%','#','o','O','0')
plot(x=NULL, xlim=c(0,3), ylim=c(0,6), xlab="X", ylab="Y")
for(x in 1:2) {
  for(y in 1:5) {
    p <- y+(2*(x-1))
    points(x,y,pch=q[p])
    text(x,y+0.2,labels=paste("pch=",q[p]),cex=0.7)
  }
}

#### symbols from 21 to 25 may be drawn in different colors:

cl <- 2:5
gr <- 21:25
plot(x=NULL, xlim=c(1,4), ylim=c(0,5), xlab="X", ylab="Y")
for(x in 1:4) {
  for(y in 1:4) {
    p <- y+(2*(x-1))
    points(x,y,pch=gr[x],bg=cl[y],cex=2)
    text(x,y+0.2,labels=paste("pch=",q[p]),cex=0.7)
  }
}

### barplot
barplot(df$mpg)

### histograms and boxplots
x <- rnorm(400, mean=40, sd=15)
hist(x)
y <- rbinom(1:400, 2, 0.3)
summary(y)
table(y)
boxplot(x ~ y)

t1 <- table(v2)
t1
pie(t1)

scatter.smooth(x)

### adding color to a plot
mycol <- ifelse(x >44,"red","blue")
scatter.smooth(x, col=mycol)

### adding a title and axis labels
scatter.smooth(x, 
               col=mycol, 
               main="Scatter plot", 
               xlab="Individuals", 
               ylab="Frequency", 
               ylim=c(-50,100))

legend(x="bottomright",
       fill=c("red","blue"), 
       col=c("red","blue") ,
       legend=c(">44","<=44"),
       cex=0.5,
       horiz = T)

### ploting many graphics at once
par(mfrow=c(2,2))
cl <- as.numeric(iris$Species)
plot(iris$Sepal.Length, main="Septal Lenght",col=cl)
plot(iris$Sepal.Width, main="Septal Width",col=cl)
plot(iris$Petal.Length, main="Petal Lenght",col=cl)
plot(iris$Petal.Width, main="Petal Width",col=cl)
par(mfrow=c(1,1))

### plotting time-series
j <- JohnsonJohnson
j
plot(j)


 
