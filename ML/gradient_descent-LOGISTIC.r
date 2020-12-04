loss_function <- function(y,pred) { 
    return(-sum((y*log(pred))+((1-y)*log(1-pred)))/length(y) )
}

sigma <- function(z) {
   yhat <- 1/(1+exp(-z))
   return(yhat)
}

### The gradient descent requires two values: learning rate and number of iterations.

gradientDesc <- function(x, y, learn_rate=1e-6, max_iter=1e3) {
  #plot(x, y, col = "blue", pch = 20)
  conv_threshold <- 1e-4
  n <- length(y)
  m <- runif(1, 0, 1)
  c <- runif(1, 0, 1)
  yhat <- sigma(m * x + c)
  cost <- loss_function(y,yhat)
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    m_new <- m - learn_rate * (sum((yhat - y) * x))
    c_new <- c - learn_rate * (sum(yhat - y))
    m <- m_new
    c <- c_new
    yhat <- sigma(m * x + c)
    cost_new <- loss_function(y,yhat)
    if(cost - cost_new <= conv_threshold) {
      #abline(c, m) 
      converged = T
      print(paste("Optimal intercept:", c, "Optimal slope:", m))
      return(data.frame(w=m,b=c,loss=cost_new))
    }
    iterations = iterations + 1
    if(iterations > max_iter) { 
      #abline(c, m) 
      converged = T
      print(paste("Optimal intercept:", c, "Optimal slope:", m))
      return(data.frame(w=m,b=c,loss=cost_new))
    }
  }
}




# Run the function 
gradientDesc(mtcars$wt, mtcars$am, 0.0001, 2500000)


mod <- glm(mtcars$am ~ mtcars$wt,family="binomial")
mod$coefficients

library(dplyr)
library(ggplot2)

## iterating with the learning rate and number of iterations
res <- NULL

learning_rate <- c(1e-1,1e-2,1e-3,1e-4,1e-5,1e-6)
number_iterations <- c(1e2,1e3,1e4,1e5,1e6,1e7)
for (iter in number_iterations) {
    for (lr in learning_rate) {
        gd <- gradientDesc(mtcars$wt, mtcars$am, lr, iter)
        res <- rbind(res, data.frame(cbind(number_iterations=iter,learning_rate=lr, gd)))
    }
}


res <- res %>% arrange(learning_rate,number_iterations)
res

options(repr.plot.width = 17, repr.plot.height = 10)
ggplot(data=res) +
  geom_line(aes(x=learning_rate, y=loss, color=factor(number_iterations), group=factor(number_iterations))) +
  scale_x_reverse()

options(repr.plot.width = 17, repr.plot.height = 10)
ggplot(data=res) +
  geom_line(aes(x=log10(learning_rate), y=loss, color=factor(number_iterations), group=factor(number_iterations))) +
  scale_x_reverse()

res <- res %>% arrange(loss)
res

gradientDesc(mtcars$wt, mtcars$am, learn_rate=1e-02,max_iter=1e+05)

predict <- function(x,w,b,cutoff=0.5) {
    prob <- sigma((x*w)+b)
    yhat <- ifelse(prob>=cutoff,1,0)
    return(data.frame(prob=prob, yhat=yhat))
}

pred <- predict(mtcars$wt, -4.02397, 12.04037)
cbind(pred, y=mtcars$am)

## contingency table
table(yhat=pred$yhat, y=mtcars$am )

loss_function <- function(y,pred) {
    ll <- ifelse(y==0,(1-y)*log(1-pred),y*log(pred))
    return(-sum(ll)/length(y))
}



loss_function(y,yyhat)

sigma <- function(z) {
   yhat <- 1/(1+exp(-z))
   return(yhat)
}

gradientDesc <- function(X, y, learn_rate=1e-6, max_iter=1e3, normalize=FALSE) {
  conv_threshold <- 1e-4
  n <- length(y)
  m <- ncol(X)
  ## add the intercept
  X <- as.matrix(cbind(b=rep(1,n),X))
  W <- as.matrix(runif(m+1, 0, 1))
  ## normalize X
  if (normalize==TRUE) {
      X <- X %*% diag(1/colSums(X))
  }
  ## recover intercept
  X[,1] <- 1
  yhat <- sigma(X %*% W)
  cost <- loss_function(y,yhat)
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    W_new <- W - t(learn_rate * (t(yhat-y) %*%  X))
    W <- W_new
    yhat <- sigma(X %*% W)
    cost_new <- loss_function(y,yhat)
    if(iterations > max_iter) { 
      converged = T
      print(paste("Optimal weights:", W))
      return(list(w=W,loss=cost_new))
    }
    iterations = iterations + 1
  }
}


X <- mtcars[,1:5]
y <- mtcars$am

gradientDesc(X,y,learn_rate=1e-3, max_iter=1e3)

mod1 <- glm(mtcars$am ~., data= mtcars[,1:5],family="binomial")
mod1$coefficients

## iterating with the learning rate and number of iterations
res <- NULL

learning_rate <- c(1e-1,1e-2,1e-3,1e-4,1e-5,1e-6)
number_iterations <- c(1e2,1e3,1e4,1e5,1e6,1e7)
for (iter in number_iterations) {
    for (lr in learning_rate) {
        gd <- gradientDesc(X, y, lr, iter)$loss
        res <- rbind(res, data.frame(cbind(number_iterations=iter,learning_rate=lr, gd)))
    }
}


res %>% filter(is.infinite(gd)==F) %>% arrange(gd)

# BEST: iter=1e+07 , lr=1e-04
gradientDesc(X,y,learn_rate=1e-04, max_iter=1e+07)

mod1$coefficients

predgd <- mtcars %>% 
  mutate(z = -142.2363606 + (mpg*4.3812627) + (cyl*6.8023036) + (disp*-0.3054370) + (hp*0.4429151) + (drat*3.1235744)) %>%
  mutate(yhat = sigma(z)) %>%
  select(yhat)
cbind(round(predgd,5),y)
table(y=y, pred=ifelse(predgd>=0.5,1,0))

#rm(predict)
pred1 <- predict(mod1,type="response")
table(y=y, pred=ifelse(pred1 >= 0.5,1,0))
