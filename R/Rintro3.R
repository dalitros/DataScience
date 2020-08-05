##############################################################
##############  TCDS - R intro part 3   ######################
##############################################################

##############################
##########  dplyr
##############################

library(dplyr)

#### transform a data.frame into a dplyr compatible data table
class(iris)

iris2 <- as_data_frame(iris)   ## function imported to dplyr from the 'tibble' package

class(iris2)

iris
iris2

#### selection of columns
iris2 %>% select(Sepal.Length, Sepal.Width)

### selection of rows using a condition
iris2 %>% filter(Species=="setosa")

### add new columns
iris2 %>% mutate(Sepal.ratio = Sepal.Length/Sepal.Width,
                 Petal.ratio = Petal.Length/Petal.Width)


### summarise (summarize)
iris2 %>% summarise(Sepal_len_mean=mean(Sepal.Length,na.rm=T), 
                    Petal_len_mean=mean(Petal.Length,na.rm=T))

### summarise by groupping
iris2 %>% 
  group_by(Species) %>%
  summarise(Sepal_len_mean=mean(Sepal.Length,na.rm=T), 
                    Petal_len_mean=mean(Petal.Length,na.rm=T))

### joining two datasets
df1 <- iris2 %>% 
  mutate(Sepal.ratio = Sepal.Length/Sepal.Width,
         Petal.ratio = Petal.Length/Petal.Width,
         id = 1:nrow(iris2)) %>%
  select(id, Sepal.ratio, Petal.ratio)

iris2 <- iris2 %>%
  mutate(id = 1:nrow(iris2))

iris3 <- inner_join(iris2, df1, by="id")

iris3

#### order the data by a column
iris3 %>%
  arrange(Sepal.Length)

iris3 %>%
  arrange(desc(Petal.Width))

#### counting
iris3 %>% tally()

iris3 %>% group_by(Species) %>% tally()

iris3 %>% group_by(Species) %>% summarise(cnt = n())

######## complex transformation
### Get the minimum, maximum and average of the height and mass, the count and the 
### number of males, females and those without a defined gender, of the characters of 
### the movie 'starwars' that appeared in the film "Attack of the Clones" 
### and by their homeworld procedence

head(starwars)

starwars$films

mysw <- starwars %>%
  group_by(homeworld) %>% 
  mutate(male = ifelse(gender == "male",1,0),
         female = ifelse(gender == "female", 1,0),
         nogender = ifelse(is.na(gender)==T,1,0),
         attack_of_clones = ifelse("Attack of the Clones" %in% films,1,0)) %>%
  filter(attack_of_clones == 1) %>%
  summarise(height_min=min(height,na.rm=TRUE),
            height_mean=mean(height,na.rm=TRUE),
            height_max=max(height,na.rm=TRUE),
            mass_min=min(mass,na.rm=TRUE),
            mass_mean=mean(mass,na.rm=TRUE),
            mass_max=max(mass,na.rm=TRUE),
            males = sum(male, na.rm=TRUE),
            females = sum(female, na.rm=TRUE),
            nogender = sum(nogender, na.rm=TRUE),
            num_individuals=n()) %>%
  arrange(desc(num_individuals))


##############################
##########  ggplot2
##############################

library(ggplot2)

# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

### a simple graph
plot(iris3$Sepal.Length ~ iris3$Sepal.Width)

####################################
### a ggplot2 graph
####################################
### Main graph: ggplot(data = <mydata>, aes( x= <X_var>, y= <Y_var>) ) +
### geometry       geom_XXXXX()
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio))

### adding color by Species
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, color=Species))

### define the shape of the points by species
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, shape=Species, color=Species))

### define the transparency by species
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, alpha=Species, color=Species))


### define size by Species 
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, size=Species))

### define stroke by Petal.Width (doesn't work with factors)
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, stroke=Petal.Length, 
                           color=Species))

### combining many properties in one graph
ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, 
                           color=Species,shape=Species, 
                           alpha=Species, size=as.numeric(Species)))

########## Separate graphs for each class: Facets

ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio)) +
  facet_wrap(~ Species, nrow = 1) 

ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, col=Species)) +
  facet_grid(round(Sepal.Length,0) ~ round(Petal.Length,0))

########### Combining two geometric objects into one graph

ggplot(data=iris3) +
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio)) 

ggplot(data=iris3) +
  geom_smooth(mapping = aes(x = Sepal.ratio, y = Petal.ratio)) 

ggplot(data = iris3) + 
  geom_smooth(mapping = aes(Sepal.ratio, y = Petal.ratio, 
                            linetype = Species, color=Species))

ggplot(data = iris3) + 
  geom_smooth(mapping = aes(Sepal.ratio, y = Petal.ratio, 
                            group=Species, color=Species))

### add two different geometries into one graph
ggplot(data = iris3) + 
  geom_point(mapping = aes(x = Sepal.ratio, y = Petal.ratio, color=Species)) +
  geom_smooth(mapping = aes(x = Sepal.ratio, y = Petal.ratio, color=Species))

### Positioning the mapping in the initial graph definition
ggplot(data = iris3, mapping = aes(x = Sepal.ratio, y = Petal.ratio, color=Species)) + 
  geom_point() +
  geom_smooth()


### setting the color of the points by species
ggplot(data = iris3, mapping = aes(x = Sepal.ratio, y = Petal.ratio)) + 
  geom_point(mapping = aes(color = Species)) + 
  geom_smooth()

### filtering the second geometry data to show all out of setosa
ggplot(data = iris3, mapping = aes(x = Sepal.ratio, y = Petal.ratio)) + 
  geom_point(mapping = aes(color = Species)) + 
  geom_smooth(data = filter(iris3, Species != "setosa"))

######### Other Geometric objects 
ggplot(data = iris3) + 
  geom_histogram(mapping = aes(x=Sepal.Length))

ggplot(data = iris3,mapping = aes(x=Sepal.Length)) + 
  geom_histogram(bins = 30)

### Bar graph
iris3 <- iris3 %>%
  mutate(Sepal.Length.cat=factor(round(Sepal.Length,0)),
         Petal.Length.cat=factor(round(Petal.Length,0)))

ggplot(data = iris3) + 
  geom_bar(mapping = aes(x=Sepal.Length.cat))

#### Colored bars
ggplot(data = iris3) + 
  geom_bar(mapping = aes(x=Sepal.Length.cat, fill=Species))

#### stacked bars
ggplot(data = iris3) + 
  geom_bar(mapping = aes(x=Sepal.Length.cat, fill=Species), position = "fill" )

### Dodge
ggplot(data = iris3) + 
  geom_bar(mapping = aes(x=Sepal.Length.cat, fill=Species), position = "dodge" )

####### Boxplots
ggplot(data = iris3) + 
  geom_boxplot(mapping = aes(x=Species,y=Sepal.ratio))

ggplot(data = iris3) + 
  geom_boxplot(mapping = aes(x=Species,y=Sepal.ratio)) +
  coord_flip()

##################################
#####  Extended layered grammar
##################################
# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(
#   mapping = aes(<MAPPINGS>),
#   stat = <STAT>, 
#   position = <POSITION>
#   ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>
##################################

ggplot(data = iris3) + 
  geom_bar(mapping = aes(x = Sepal.Length.cat, fill=Species)) 

ggplot(data = iris3) + 
  geom_bar(mapping = aes(x = Sepal.Length.cat, fill=Species)) +
  coord_polar() 

####################
#summary(mysw)
#table(mtcars$cyl)
# mpg - wt - cyl

#ggplot(data = mtcars) +
#  geom_point(mapping=aes(x = mpg, y = wt, color=factor(cyl))) +
#  facet_grid(~cyl) +
#  coord_flip()

#######################################################################
###########  Regular Expressions
#######################################################################

#### grep
grep("a", c("abc", "def", "cba a", "aa"), value=FALSE)  ### return indices
grep("a", c("abc", "def", "cba a", "aa"), value=TRUE)   ### return values

head(colours())
grep("orange",colours())
grep("orange",colours(),value=T)

grep("dark",colours())
grep("dark",colours(),value=T)

### "red", "^red", "$red"
grep("red",colours(),value=T)  ## contains red
grep("^red",colours(),value=T) ## ^ -> begins with red
grep("red$",colours(),value=T) ## $ -> ends with red
grep("red.$",colours(),value=T) ## '.' -> match any single character
grep("^red",colours(),value=T) ## '^' -> begins with red
grep("^red[2:4]",colours(),value=T) ## [] -> begins with red and has a number of 2 or 4 
grep("^red[2|4]",colours(),value=T) ## [] -> begins with red and has a number of 3 or 4
grep("^red[2-4]",colours(),value=T) ## [] -> begins with red and has a number between 2 and 4

grep("^red[2:4]|^blue[2:4]",colours(),value=T)  

grep("2|4",colours()[100:150],value=T)  
grep("[24]",colours()[100:150],value=T)  

grep("^[dl].*2$",colours(),value=T)  ## all strings beginin with d or l and ending with 2

#### logical grep
grepl("a", c("abc", "def", "cba a", "aa"))  ##  value=TRUE is not supported
grepl("white",colours()[1:20])

#### Find and Replace once
sub("a", "A", c("abc", "def", "cba a", "aa"))  ##  replace the first occurrence of 'a' 
                                               ##   on each element with an 'A'
#### Find and Replace any occurence
gsub("a", "A", c("abc", "def", "cba a", "aa"))

#### Return the index of the first match of 'a' as a vector
regexpr("a", c("abc", "def", "cba a", "aa"))

#### Return the indices of all the matches of 'a' as a list
gregexpr("a", c("abc", "def", "cba a", "aa"))

#### Return the index of the first match of 'a' as a list
regexec("a", c("abc", "def", "cba a", "aa"))

#### Split an object when a character or string appears.
strsplit(c("abc", "def", "cba a", "aa"),split = "b")

### separating by a comma
strsplit(c("The boy is playing with his car, his father is talking on the phone"),split=',')

### separating by a point
strsplit(c("The boy is playing with his car. His father is talking on the phone"),split='\\.')


####################################################
############   Functions
####################################################

# myfun <- function(arg1, a1g2) {
#    body
#}


xplusone <- function(x) {
  x + 1
}

xplusone(7)
xplusone(20)
b <- c(1,2,3,4,5)
xplusone(b)

### beware from reserved words !!!
c <- function(x) {
  x + 1
}

c(5)
b <- c(1,2,3,4,5)
c(b)

rm(c)

BMI <- function(weight, height) {
  weight/(height^2)
}

BMI(70,1.75)
BMI(85,1.65)

weights <- c(rnorm(10,mean=75,sd=8.5))
heights <- c(rnorm(10,mean=1.75,sd=0.2))

BMI(weights, heights)

BMI()
BMI(65)

BMI <- function(weight, height=1.75) {
  weight/(height^2)
}

BMI(65)
BMI(65,1.65)

BMI <- function(weight, height=NULL) {
  weight/(height^2)
}

BMI()
BMI(65)
BMI(65,1.65)

BMI <- function(weight=NULL, height=NULL) {
  try(weight/(height^2))
}

BMI()
BMI(65)
BMI(65,1.65)


BMI <- function(weight=NULL, height=NULL) {
  if(is.null(weight)) {
    stop("Please enter a value for weight")
  }
  if(is.null(height)) {
    stop("Please enter a value for height")
  }
  weight/(height^2)
}


BMI()
BMI(65)
BMI(65,"a")

BMI <- function(weight=NULL, height=NULL) {
  if(!is.numeric(weight)) {
    stop("Please enter a numeric value for weight")
  }
  if(!is.numeric(height)) {
    stop("Please enter a numeric value for height")
  }
  weight/(height^2)
}

BMI()
BMI(65)
BMI(65,"a")
BMI(65,0)

BMI <- function(weight, height) {
  res <- tryCatch(
    { 
      weight/(height^2)
    },
    error= function(cond) {
        message("Please enter a numeric value for weight or height")
        return(NA)
    },
    finally= {
      message("Your BMI is: ")
    }
  )
  return(res)
}

BMI()
BMI(65)
BMI(65,0)
BMI(65,1.5)




#########################################################
#####   Import / Export data
#########################################################

###### CSV (comma separated text)

write.csv(iris, file="iris.csv")
write.csv(iris, file="iris.csv",row.names = F)

mydata <- read.csv(file="iris.csv")
class(mydata)
str(mydata)

####### Excel
library(xlsx)
df1 <- read.xlsx("excel-example.xlsx",sheetIndex = 1)
df2 <- read.xlsx("excel-example.xlsx",sheetIndex = 2)

### Write a data.frame to an excel file
write.xlsx(df1, "one-sheet-example.xlsx", sheetName="Data Frame")


### generate some worksheets and add them to a workbook, then save it into an excel file
library(dplyr)

countries <- df1 %>% 
  group_by(Country) %>% 
  summarise(age_mean=mean(Age,na.rm=T),
            income_mean=mean(Income, na.rm=T),
            income_min=min(Income, na.rm=T),
            income_max=max(Income, na.rm=T),
            count=n())

calories <- df2 %>% 
  group_by(Diet) %>% 
  summarise(caliries_mean=mean(Calories_per_day,na.rm=T),
            caliries_min=min(Calories_per_day,na.rm=T),
            caliries_max=max(Calories_per_day,na.rm=T),
            count=n())

#### pass the tables to an excel file

file <- "excel-example2.xlsx"

wb <- createWorkbook()
sheet1 <- createSheet(wb, sheetName="Countries")
sheet2 <- createSheet(wb, sheetName="Calories")
sheet3 <- createSheet(wb, sheetName="Diet")
sheet4 <- createSheet(wb, sheetName="Income")

addDataFrame(countries,sheet1)
addDataFrame(calories,sheet2)
addDataFrame(df1,sheet3)
addDataFrame(df2,sheet4)

saveWorkbook(wb, file)


################################
##### SPSS / SAS / STATA
###############################

#### SPSS
library(foreign)
sav <- read.spss(file="c:/mydata.sav", to.data.frame=TRUE) 

#### SAS
library(Hmisc)
sap <- sasxport.get("c:/mydata.xpt")

#### STATA
library(foreign)
stata <- read.dta("c:/mydata.dta")

##########################
##### HTML / XML
##########################

###### HTML

library(XML)

url <- "http://www.google.com/search?q=introduction+to+r"
doc <- htmlParse(url)
links <- xpathSApply(doc, "//a/@href")
free(doc)

length(links)

links[[1]]
links[[20]]

###### XML

myxml <- "<foo>
  <bar>text <baz id = 'a' /></bar>
  <bar>2</bar>
  <baz id = 'b' />
</foo>"

xmldoc <- xmlParse(myxml)
rootNode <- xmlRoot(xmldoc)
rootNode[1]
rootNode[2]

########################
#### JSON files
########################
library(jsonlite)

json_file <- "http://api.worldbank.org/country?per_page=10&region=OED&lendingtype=LNX&format=json"
json_data <- fromJSON(json_file)

json_data[[1]]$per_page

json_df <- as.data.frame(json_data)


########################
#### DATABASE
########################

#### To be able to query the database we need to create a new ODBC DNS on the windows computer


library(DBI)
con <- dbConnect(odbc::odbc(), "tcds")
sql <- "SELECT * FROM acs2015_country_data"
acs <- dbGetQuery(con, sql)
dbDisconnect(con)

