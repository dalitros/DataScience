############################################################################
###### Titanic - Analyzing the titanic data
############################################################################
library(dplyr)
library(ggplot2)

### Import the titanic dataset
titanic <- read.csv(paste(path,"../data/titanic.csv",sep="/"))
head(titanic)

##################################################
### Number of pasangers and how much survived
##################################################
passangers = nrow(titanic)
survived = sum(titanic['Survived'])

print(paste("We have", passangers, "passangers but only", survived, "of them survived (", 
            survived/passangers*100, "%)"))

###################################################
#### Missing values
## How many missing values are in the dataset?
## We can use summary to see the ammount of NA's on each variable
###################################################
summary(titanic)

## another way is ussing sapply
sapply(titanic, function(x) sum(is.na(x)))

#### Note: the results here differ from those we get in Python because in Python
#### pandas first determine where are nulls and then check for the data type, 
#### while in R first determine the data type and character variables with no data
#### are substituted with blanks and not with NA's

##############################################
## Gender distribution and survival
## How distribute the survivors by gender?
##############################################

## using table()
table(sex=titanic$Sex, surv=titanic$Survived)

## using dplyr
titanic %>% group_by(Sex) %>% summarise(count=n(),survived=sum(Survived),percent=(sum(Survived)/n()*100))

## Age distribution and survival
## Plot the Age frequencies of the passangers. (use an histogram)

# using the base plots
hist(titanic$Age)
hist(titanic$Age, breaks=30)

# using ggplot
ggplot(data=titanic, aes(x=Age)) +
  geom_histogram(bins = 30) + 
  title(main="Age distribution", ylab="Frequency")

############################################
### Create a new variable that will divide the passangers in four age categories:
# Babies: 0-5
# Children: 6-12 years old
# Young: 13-17 years old
# Adult: 18-59 years old
# Older: 60+ years old
### How many (number and percent) survived on each age group?
############################################

### using subsetting
titanic$AgeGroup <- "18-59"
titanic$AgeGroup[titanic$Age < 6] <- " 0-5"
titanic$AgeGroup[titanic$Age >= 6 & titanic$Age < 13] <- " 6-12"
titanic$AgeGroup[titanic$Age >= 13 & titanic$Age < 18] <- " 13-17"
titanic$AgeGroup[titanic$Age >= 60] <- " 60+"
titanic$AgeGroup[is.na(titanic$Age)==TRUE] <- "No Age"

table(titanic$AgeGroup)

### using which()
titanic$AgeGroup <- "18-59"
titanic$AgeGroup[which(titanic$Age < 6)] <- " 0-5"
titanic$AgeGroup[which(titanic$Age >= 6 & titanic$Age < 13)] <- " 6-12"
titanic$AgeGroup[which(titanic$Age >= 13 & titanic$Age < 18)] <- " 13-17"
titanic$AgeGroup[which(titanic$Age >= 60)] <- " 60+"
titanic$AgeGroup[which(is.na(titanic$Age)==TRUE)] <- "No Age"

table(titanic$AgeGroup)

### using dplyr and ifelse()
titanic <- titanic %>% 
  mutate(AgeGroup= ifelse(is.na(Age)==TRUE, "No Age",
                   ifelse(Age <5, "0-5", 
                   ifelse(Age < 13, "6-12",
                   ifelse(Age < 18, "13-17",
                   ifelse(Age <60,"18-59","60+"))))))

table(titanic$AgeGroup)

titanic %>% 
  group_by(AgeGroup) %>% 
  summarise(count=n(),survived=sum(Survived),percent=(sum(Survived)/n()*100))

# Age-Gender Survival
# Where there differences on survival by age group and gender?

titanic %>% 
  group_by(Sex,AgeGroup) %>% 
  summarise(count=n(),survived=sum(Survived),percent=(sum(Survived)/n()*100))

#####################################
## Passenger Class and Survival
## Was there any difference in the survival among passangers by their ticket class (Pclass) ?
#####################################

titanic %>% 
  group_by(Pclass) %>% 
  summarise(count=n(),survived=sum(Survived),percent=(sum(Survived)/n()*100))

######################################
## Traveling alone vs with family
## Who survived more, individuals that traveled alone or those who traveled with their families?
######################################

titanic$TravelAlone <- ifelse(titanic$SibSp==0 & titanic$Parch==0, 1, 0)

titanic %>% 
  group_by(TravelAlone) %>% 
  summarise(count=n(),survived=sum(Survived),percent=(sum(Survived)/n()*100))

#######################################
## Embarking port and survival
## Was there any difference in survival related to the embarking port?
#######################################

titanic %>% 
  group_by(Embarked) %>% 
  summarise(count=n(),survived=sum(Survived),percent=(sum(Survived)/n()*100))

#######################################
## Paid fair and survival
## What was the fare range paid by the passangers?
#######################################

hist(titanic$Fare,breaks = 40)

#######################################
## How many individuals didn't paid for their ticket?
#######################################

titanic <- titanic %>%
  mutate(Gratis=ifelse(Fare==0,1,0))

sum(titanic$Gratis)

########################################
## Does this affected their survival?
########################################

titanic %>% 
  group_by(Gratis) %>% 
  summarise(count=n(),survived=sum(Survived),percent=(sum(Survived)/n()*100))

#########################################
## Where there differences in fare rates among the same ticket classes?
## If the answer was yes, does those differences affected the survival of individuals?
#########################################

titanic %>% 
  group_by(Pclass) %>% 
  summarise(count=n(),
            Fare_min=min(Fare),
            Fare_q25=quantile(Fare,0.25),
            Fare_mean=mean(Fare),
            Fare_median=median(Fare),
            Fare_q75=quantile(Fare,0.75),
            Fare_max=max(Fare))
            
titanic$FareGroup = 'Normal'
titanic$FareGroup[titanic['Fare']<30.92 & titanic['Pclass']==1] <- 'Underpaid'
titanic$FareGroup[titanic['Fare']<13. & titanic['Pclass']==2] <- 'Underpaid'
titanic$FareGroup[titanic['Fare']<7.75 & titanic['Pclass']==3] <- 'Underpaid'

titanic$FareGroup[titanic['Fare']>93.5 & titanic['Pclass']==1] <- 'Overpaid'
titanic$FareGroup[titanic['Fare']>26. & titanic['Pclass']==2] <- 'Overpaid'
titanic$FareGroup[titanic['Fare']>15.5 & titanic['Pclass']==3] <- 'Overpaid'

table(titanic$FareGroup)

titanic %>% 
  group_by(Pclass,FareGroup) %>% 
  summarise(count=n(),survived=sum(Survived),percent=(sum(Survived)/n()*100))

###########################################
## The title passanger had and survival
## Which were the five most common titles passanger had? (Sir, Mr, Mrs, etc)?
## For this part we will take the 'Name' column and will split all the words in the name 
##   by the white space.Then we will join all the words and calculate the frequency of 
##  appearance of each world in descending order. We will take the five most common words 
##  (must be titles), and with them we will create a new column. Then we will procede as 
##  we did in the other analyses.
############################################

words = paste(titanic$Name, collapse=" ")
class(words)
print(words)

## we can use the :punct: opperand from the re (regular expression) package:

words = gsub('[[:punct:] ]+',' ',words)
print(words)

## or we can use the function removePunctuation from the tm package 
# install.packages("tm")
library(tm)
words<-removePunctuation(words)
print(words)

## now we split the words into a vector.
## strsplit generates a list with one element that is a vector of strings
## we are only interested on the vector, so we get it adding the [[1]]

wordlist = strsplit(words," ")[[1]]
class(wordlist)
length(wordlist)
wordlist
wordcnt <- data.frame(table(wordlist))

wordcnt %>% arrange(desc(Freq)) %>% top_n(15)

## We have shown here that the most common titles were Mr (521), Miss (182), Mrs (129) 
## and Master (40). This totals 872 out of 891 passengers (97.9%).
## Now we will create a new variable with those titles and check for differences on 
## survival among them.

titanic$Title <- "Other"

titanic[grep("Miss", titanic$Name,fixed = T),"Title"] <- "Miss"
titanic[grep("Mr", titanic$Name,fixed = T),"Title"] <- "Mr"
titanic[grep("Mrs", titanic$Name,fixed = T),"Title"] <- "Mrs"
titanic[grep("Master", titanic$Name,fixed = T),"Title"] <- "Master"

table(titanic$Title)

titanic %>% 
  group_by(Title) %>% 
  summarise(count=n(),survived=sum(Survived),percent=(sum(Survived)/n()*100))

