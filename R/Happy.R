Happy <- read.csv("C:/Users/dalit/DataScience/data/SomervilleHappinessSurvey2015.csv", header=FALSE)
View(SomervilleHappinessSurvey2015)

data(Happy)
summary(Happy)
D

### logistic regression model

mod1 <- glm(D ~ ., data=Happy, family = "binomial")
summary(mod1)
exp(mod1$coefficients)

Happy2 <- data.frame(sapply(Happy,factor))

Happy2
