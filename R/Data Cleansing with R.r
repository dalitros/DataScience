library(dplyr)
library(ggplot2)
if(!require(ggExtra)) {install.packages("ggExtra");require(ggExtra)}
if(!require(naniar)) {install.packages("naniar");require(naniar)}
if(!require(mvoutlier)) {install.packages("mvoutlier");require(mvoutlier)}
if(!require(MissMech)) {install.packages("MissMech");require(MissMech)}
if(!require(mice)) {install.packages("mice");require(mice)}
if(!require(MatchIt)) {install.packages("MatchIt");require(MatchIt)}

library(readr)
data <- read_csv("C:/Users/dalit/DataScience/data/mammalsleep.csv")

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

minmax <- function(x) {
    return(((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))))
}

getMissingness <- function (data, getRows = FALSE) {
    require(dplyr)
    l <- nrow(data)
    vn <- names(data)
    nadf <- data
    cnt <- NULL
    miss <- function(x) return(sum(is.na(x)))
    for (n in vn) {
        nadf[[n]] <- ifelse(is.na(nadf[[n]]) == T, 1, 0)
        cnt <- rbind(cnt, data.frame(n, sum(nadf[[n]])))
    }
    names(cnt) <- c("var", "na.count")
    cnt$rate <- round((cnt$na.count/nrow(nadf)) * 100, 1)
    nadf$na.cnt <- 0
    nadf$na.cnt <- rowSums(nadf)
    cnt <- cnt %>% dplyr::arrange(desc(na.count)) %>% dplyr::filter(na.count > 
        0)
    totmiss <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::tally()
    idx <- NULL
    msg <- (paste("This dataset has ", as.character(totmiss), 
        " (", as.character(round(totmiss/nrow(data) * 100, 1)), 
        "%)", " complete rows. Original data has ", nrow(data), 
        " rows.", sep = ""))
    if (getRows == TRUE & totmiss != 0) {
        nadf$rn <- seq_len(nrow(data))
        idx <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::select(rn)
    }
    print(list(head(cnt, n = 10), msg))
    return(list(missingness = cnt, message = msg, rows = idx$rn))
}

### load data

animals = read.csv("C:/Users/dalit/DataScience/data/mammalsleep.csv")
animals$X <- NULL
head(animals)

## Using IRQ for catching univariate outliers (1.5 x IRQ)
head(outlierMatrix(animals,threshold=1.5))

## Using IRQ for catching univariate outliers (1.5 x IRQ)
head(outlierMatrix(animals,threshold=2.0))

## Visual determination of univariate outliers using boxplots 
options(repr.plot.width = 8, repr.plot.height = 8)
par(mfrow=c(4,3))
for(v in names(animals[,2:8])) {
    boxplot(animals[[v]],main=v)
}
par(mfrow=c(1,1))


## Visual determination of univariate outliers using scatter plots 
options(repr.plot.width = 8, repr.plot.height = 8)
par(mfrow=c(4,3))
for(v in names(animals[,2:8])) {
    scatter.smooth(animals[[v]] ~ animals$species, main=v, xlab="animals",ylab=v, family="symmetric",
                  lpars =list(col = "red", lwd = 2, lty = 2))
}
par(mfrow=c(1,1))


## dbscan
library(dbscan)
# normalize the numbers
animals.norm <- sapply(animals[,2:ncol(animals)], minmax)

head(animals.norm)

mm <- getMissingness(animals, getRows = T)
head(mm$rows)

mod <- dbscan(animals.norm[mm$rows,], eps=0.5, minPts = 4)
table(mod$cluster)

options(repr.plot.width = 8, repr.plot.height = 8)
pairs(animals[,2:ncol(animals)], col=ifelse(mod$cluster==0,2,1))


options(repr.plot.width = 4, repr.plot.height = 4)
res <-chisq.plot(animals.norm[mm$rows,2:6],ask=F)
res$outliers

## Distance-Distance plot for outlier detection
options(repr.plot.width = 4, repr.plot.height = 4)
animals2 <- animals[mm$rows,c(4,5,7,8)]
distances <- dd.plot(animals2, quan=1/2, alpha=0.025)

animals.pca <- prcomp(animals[mm$rows,2:ncol(animals)])

options(repr.plot.width = 4, repr.plot.height = 3)
plot(animals.pca)

plot(animals.pca$x)

outliers <- ifelse(animals.pca$x[,1]>800,2,1)
plot(animals.pca$x, col=outliers)

head(missingMatrix(animals))

options(repr.plot.width = 4, repr.plot.height = 4)
vis_miss(animals)

# require(naniar)
options(repr.plot.width = 8, repr.plot.height = 4)
gg_miss_fct(x=animals, fct=species) + 
theme(axis.text.x = element_text(angle=90, size=8))

# require(MissMech)
animals2 <- animals[,c(4,5,7,8)]
miss1 <- TestMCARNormality(data=animals2)
miss1

### Impute missing values

miss1 <- TestMCARNormality(data=animals2, , del.lesscases = 1, imputation.number = 10)
summary(miss1)


options(repr.plot.width = 6, repr.plot.height = 8)
boxplot(miss1)

summary(miss1$imputed.data)

animals.imp <- data.frame(miss1$imputed.data)
idx <- miss1$caseorder

head(animals.imp)
dim(animals.imp)

## Visualize the imputed missing values using scatter plots 
options(repr.plot.width = 8, repr.plot.height = 8)

misspoints <- missingMatrix(animals[idx,])
animals.imp <- data.frame(miss1$imputed.data)
par(mfrow=c(4,3))
for(v in names(animals2)) {
    scatter.smooth(animals.imp[[v]] ~ animals[idx,"species"], main=v, xlab="animals",ylab=v, family="symmetric",
                  lpars =list(col = "red", lwd = 2, lty = 2), col=misspoints[idx,v]+1)
}
par(mfrow=c(1,1))


# library(mice)
init = mice(animals, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth

predM

### Pedict the missing values
set.seed(103)
imputed = mice(animals, method=meth, predictorMatrix=predM, m=5)

summary(imputed)

summary(imputed$imp)
imp1 <- complete(imputed,1)
dim(imp1)

imputed.full <- complete(imputed,action="long")
dim(imputed.full)

## Check if there are any missing on the imputed data
sapply(imputed, function(x) sum(is.na(x)))

options(repr.plot.width = 8, repr.plot.height = 8)

misspoints <- missingMatrix(animals)
animals.imp <- complete(imputed,1)
par(mfrow=c(4,3))
for(v in names(animals)) {
    scatter.smooth(animals.imp[[v]] ~ animals[,"species"], main=v, xlab="animals",ylab=v, family="symmetric",
                  lpars =list(col = "red", lwd = 2, lty = 2), col=misspoints[,v]+1)
}
par(mfrow=c(1,1))


options(repr.plot.width = 8, repr.plot.height = 8)

misspoints <- missingMatrix(animals)
animals.imp <- complete(imputed,5)
par(mfrow=c(4,3))
for(v in names(animals)) {
    scatter.smooth(animals.imp[[v]] ~ animals[,"species"], main=v, xlab="animals",ylab=v, family="symmetric",
                  lpars =list(col = "red", lwd = 2, lty = 2), col=misspoints[,v]+1)
}
par(mfrow=c(1,1))


### gt on each of the five imputations
par(mfrow=c(4,3))

## mice imputation #1
scatter.smooth(complete(imputed,1)[["gt"]] ~ animals[,"species"], main="mice imputation #1", xlab="animals",ylab="gt", family="symmetric",
                  lpars =list(col = "red", lwd = 2, lty = 2), col=misspoints$gt+1)

## mice imputation #2
scatter.smooth(complete(imputed,2)[["gt"]] ~ animals[,"species"], main="mice imputation #2", xlab="animals",ylab="gt", family="symmetric",
                  lpars =list(col = "red", lwd = 2, lty = 2), col=misspoints$gt+1)

## mice imputation #3
scatter.smooth(complete(imputed,3)[["gt"]] ~ animals[,"species"], main="mice imputation #3", xlab="animals",ylab="gt", family="symmetric",
                  lpars =list(col = "red", lwd = 2, lty = 2), col=misspoints$gt+1)

## mice imputation #4
scatter.smooth(complete(imputed,4)[["gt"]] ~ animals[,"species"], main="mice imputation #4", xlab="animals",ylab="gt", family="symmetric",
                  lpars =list(col = "red", lwd = 2, lty = 2), col=misspoints$gt+1)

## mice imputation #5
scatter.smooth(complete(imputed,5)[["gt"]] ~ animals[,"species"], main="mice imputation #5", xlab="animals",ylab="gt", family="symmetric",
                  lpars =list(col = "red", lwd = 2, lty = 2), col=misspoints$gt+1)

par(mfrow=c(1,1))


## require(MatchIt)

animals2 <- animals[,c('species', 'bw', 'brw', 'pi', 'sei', 'odi')]
animals2$sws_miss <- ifelse(is.na(animals$sws)==T, 1, 0)
summary(animals2)

m.out <- matchit(sws_miss ~ bw + brw + pi + sei + odi, data = animals2, method = "nearest", ratio = 1)
summary(m.out)


  options(repr.plot.width = 8, repr.plot.height = 8)
plot(m.out, type = "hist")

animals.matched <- match.data(m.out) 
animals.matched %>% arrange(distance)

