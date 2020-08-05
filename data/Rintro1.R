##############################################################
##############  TCDS - R intro part 1   ######################
##############################################################

##########################
### R objects
##########################

### assign a number to a variable (object)
a <- 10

### assign a string to a variable (object)
b <- "Hello world"

### assign a boolean (logical) to a variable (object)
c <- TRUE
C <- FALSE

c
C

### NOTE: in R object names are case sensititve !!!
c1 <- true
c1 <- True

### assign a float to a variable (object)
pi <- 3.14159265359

### check the type of object
class(a)
class(b)
class(c)
class(pi)

### check for the structure of the data
str(a)
str(b)
str(c)
str(pi)

#######################
### Vectors
#######################

### generate a vector of numbers
v1 <- c(1,2,3,4,5,6)
min(v1)
max(v1)
mean(v1)
summary(v1)

### generate a vector of characters
v2 <- c("Yes","No","No","Yes","No","Yes","No","No")
summary(v2)
table(v2)

### generating a vector with existing objects
v3 <- c(a,b,c)
v3
summary(v3)
table(v3)

### adding an object to an existing vector
v4 <- c(v3,pi)
v4
table(v4)

### checking the class and structure of vectors
class(v1)
str(v1)

class(v4)
str(v4)

### generate a logical vector
v5 <- c(TRUE, FALSE, FALSE, TRUE, FALSE)

### Factors
v2
v2a <- factor(v2)
v2a
str(v2a)
summary(v2a)

### generate a factor and define the levels
v6 <- c(1,3,2,3,2,4,1,2,1,1,1,2,1,4,3,4,2,4)
v6f <- factor(v6, levels=c(1,2,3,4), labels=c("None","Low","Medium","High"))

summary(v6)
summary(v6f)

v6
v6f

### changing the order of the categories in a factor
v6f <- factor(v6f, levels=c("Low","Medium","High","None"))
summary(v6f)

v6f <- factor(v6f, levels=c(4,1,2,3))
summary(v6f)

### generating sequences
# numeric sequence
v7 <- 12:24
v7

# numeric sequence incremented by a specific number
v8 <- seq(from=10, to=100, by=5)
v8
v8 <- seq(10,100,5)
v8

# string sequence
v9 <- letters
v10 <- LETTERS
v9[5]
v10[1:3]

length(v10)
v10[seq(1,26,3)]
v10[seq(1,length(v10),3)]

### operations on numerical vectors
a
a * 2
a - 5
a / 2
a^2
sqrt(a)
log(a)
log10(a)

a * a

a
a - c(2,4)  ### cycling


############################
########### Matrices
############################

## create matrix - input data by row
m1 <- matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), nrow=4, ncol=4, byrow=TRUE)
dim(m1)
nrow(m1)
ncol(m1)
m1

## create matrix - input data by column
m1 <- matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), nrow=4, ncol=4, byrow=FALSE)
dim(m1)
m1

## define only the number of rows
m1 <- matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), nrow=4)
dim(m1)
m1

## define only the number of columns
m1 <- matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), ncol=4)
dim(m1)
m1

?matrix

### column and row operations
m1
colSums(m1)
rowSums(m1)

colMeans(m1)
rowMeans(m1)

## Transposing a matrix
m1
t(m1)

## Addition/Substraction of matrices
m2 <- matrix(c(1,2,3,4), ncol=1)
m2
dim(m2)

m3 <- matrix(c(2,2,2,2,2,2,2,2), nrow=4)
m3
dim(m3)

m1 + m1
m1 + t(m1)
m1 - t(m1)

m1 + m2 ## doesn't work
m1 + as.vector(m2) ## doesn't work


## Multiplication/Division
## m1 [4x4], m2 [1x4], m3 [4x2]
m1 * m2  ## doesn't work
m1 %*% m2 ## this works 
crossprod(m1,m2)  ## 

m1 %*% m3
m2 %*% m3


###### Special matrices
### unit matrix
matrix(1,ncol=5,nrow=8)

### Zero matrix
matrix(0,ncol=5,nrow=8)

### Diagonal matrix
s <- matrix(c(4,4,-2,2,6,2,2,8,4),3,3)
s

diag(s)
diag(diag(s))

### Identity matrix
I <- diag(c(1,1,1))
I

### Inverse of a matix
s
solve(s)
s %*% solve(s)

##########################
### Arrays
##########################

### Matrices can represent bidimentional objects. 
### To represent multidimentional object we use arrays
a1 <- array(1:24, c(3,4,2))
a1
class(a1)
str(a1)
summary(a1)

### Operations on arrays are like operations on matrices
a1 * 2
a1^2

a2 <- array(2, c(4,2,2))
a2

a1 + a2  ## (3x4x2) + (4x2x2) ### non-conformable
a1 * a2  ## (3x4x2) * (4x2x2) ### non-conformable
a1 %*% a2  ## (3x4x2) %*% (4x2x2) ### non-conformable

a3 <- array(3, c(3,4,2))
a3

a1 + a3  ## (3x4x2) + (3x4x2)
a1 * a3  ## (3x4x2) + (3x4x2)
a1 %*% a3 ### result in a (1x1) matrix

a4 <- array(rnorm(n=300,mean=0.5,sd=0.15), dim = c(10,10,3))
a4

a5 <- array(c(rep(0,100),rep(1,100),rep(0,100)), dim = c(10,10,3))
a5

a4 * a5

(a4 * a5) 

##########################
### Lists
##########################

### simple list
l1 <- list(1:5)
l1
str(l1)
class(l1)
summary(l1)

l2 <- list(letters[1:10])
l2

l3 <- list(c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE))
l3

### list of vectors (named):
l4 <- list(numlst=l1, chrlst=l2, boollst=l3)
l4
class(l4)
str(l4)
summary(l4)
names(l4)

l4[1]
l4$numlst

l4[2]
l4[[2]]
l4$chrlst

### retrieving a value from an object in a list
l4[[2]][[1]][2]   # this will return the character "c" from the second object (chrlst)

### list of lists
l5 <- list(list(model="2018 Toyota Camry Hybrid", price=32400, mpg=52),
           list(model="2018 Ford Fusion Hybrid", price=37370 , mpg=42),
           list(model="2018 Toyota Prius", price=30565 , mpg=52),
           list(model="2018 Hyundai Ioniq",price=28300, mpg=58),
           list(model="2018 Kia Optima Hybrid",price=35210, mpg=43),
           list(model="2018 Ford C-Max Hybrid",price=27275, mpg=40)
      )

l5
class(l5)
str(l5)
summary(l5)

l5$model
l5[1]
l5[[1]]
l5[[2]]
l5[[3]]

l5[[3]]$model
l5[[3]]$price
l5[[3]]$mpg

### list with multiple data types:
l6 <- list(
  vector_list=v5,
  list_list=l5,
  matrix_list=m3,
  n=6  
)
l6
class(l6)
summary(l6)
class(l6$matrix_list)
str(l6)

l6$vector_list
l6$matrix_list
l6$n




##########################
### DATA FRAMES
##########################
### A data.frame is a collection of vectors
### Vectors are arranged by colums
### The length of each vector has to be the same

a <- c(1,2,3,4,5,6,7,8)
b <- c("a","b","c","d","e","f","g","h")
c <- c(TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE)
d <- c(24.233,1.45,10,83.62234,333.02,0.001,2.1,0.1001)
df <- data.frame(a,b,c,d)

class(df)
str(df)
summary(df)

### Subseting a data.frame
df[3,]
df[,3]

df$c
df$b

df.false <- subset(df, c==FALSE)

### changing the values of some rows in a variable based on a condition
df
df$a[which(df$c==TRUE)] <- 0
df

### deleting parts of the data frame
df$c <- NULL
df[,-3]

df[-5,]

#########################################
### Data type conversion
#########################################

### converting numbers to strings
a <- 10
class(a)

b <- as.character(a)
class(b)

a <- 1:10
a
class(a)

b <- as.character(a)
b
class(b)

### transforming characters to numbers
c <- as.numeric(b)
c
class(c)

a <- c("a","b","3","d","e")
a
b <- as.numeric(a)
b

### converting date strings into date objects
a <- c('2015-04-22',"2017/03/06","16/03/2011")
a
b <- as.Date(a)
b

### missing values
c <- NA
is.na(c)

### NULL deletes a column in a data frame 
df <- mtcars
df$mpg <- NULL

set.seed(4)
names.vec <- c('Avi', 'Ben', 'Gad', 'Dan', 'Harel', 'Vered', 'Zelig')
ages.vec <- sample(25:35, size=7, replace=T)
is.married.vec <- sample(c(T, F), size=7, replace=T)
cities = c('Jerusalem', 'Tel Aviv', 'Haifa')
city.vec <- sample(cities, size=7, replace=T, prob=c(0.6, 0.3, 0.1))
has.pet.vec <- sample(c(T, F), size=7, replace=T, prob=c(0.35, 0.65))

guys <- data.frame(name = names.vec,
                   age = ages.vec,
                   is.married = is.married.vec,
                   city = city.vec,
                   has.pet = has.pet.vec,
                   stringsAsFactors = F)

print(guys)
summary(guys)
str(guys)
 
