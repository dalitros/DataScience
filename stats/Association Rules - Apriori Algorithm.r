ifelse(!require(arules), install.packages("arules"), require(arules))
ifelse(!require(arulesViz), install.packages("arulesViz"), require(arulesViz))

data(Groceries)

class(Groceries)

str(Groceries)

### Lets see how many transactions the dataset has
length(Groceries)

inspect(Groceries)

inspect(Groceries[1:5])

items <- itemFrequency(Groceries)
head(items)
summary(items)

options(repr.plot.width = 5, repr.plot.height = 3)
hist(items,breaks = 20)

options(repr.plot.width = 6, repr.plot.height = 4)
itemFrequencyPlot(Groceries)

itemFrequencyPlot(Groceries, topN=10, type="absolute")

### using the apriori algorithm
itemsets <- apriori(data = Groceries)

## We can define how to retrieve the list of individual frequent items. For this we set minlen and maxlen to 1.
itemsets <- apriori(data = Groceries, parameter = list(minlen=1,maxlen=1,support=0.02,target="frequent itemsets"))


## Lets see the first ten items with the highest support
inspect(head(sort(itemsets, by="support"),10))

itemsets

## Now we will retrieve the list of frequent paired items. We set minlen and maxlen to 2.
itemsets <- apriori(data = Groceries, parameter = list(minlen=2,maxlen=2,support=0.02,target="frequent itemsets"))


## And check the first ten item pairs with the highest support
inspect(head(sort(itemsets, by="support"),10))


itemsets

## Now we will retrieve the list of frequent triplet items. We set minlen and maxlen to 3.
itemsets <- apriori(data = Groceries, parameter = list(minlen=3,maxlen=3,support=0.02,target="frequent itemsets"))


## And check the first ten item triplets with the highest support
inspect(head(sort(itemsets, by="support"),10))

itemsets

## Now we will retrieve the list of frequent four-items. We set minlen and maxlen to 4.
itemsets <- apriori(data = Groceries, parameter = list(minlen=4,maxlen=4,support=0.02,target="frequent itemsets"))


## And check the first ten tetra-item with the highest support
inspect(head(sort(itemsets, by="support"),10))

itemsets

rules <- apriori(data = Groceries, parameter = list(support=0.001, confidence=0.6,target="rules"))


inspect(head(sort(rules, by="lift"),10))

strong_rules <- sort(rules, by="confidence", decreasing = T)
inspect(head(strong_rules,10))

strong_rules

redundant_rules <- is.redundant(strong_rules)
summary(redundant_rules)

strong_rules <- strong_rules[!redundant_rules]
strong_rules

strong_rules


#rules <- apriori(data = Groceries, parameter = list(support=0.001, confidence=0.1),
#                appearance= list(default="rhs",lhs="hamburger meat"))
rules <- apriori(data = Groceries, parameter = list(support=0.001, confidence=0.10,target="rules"),
                appearance= list(default="rhs",lhs="whole milk"))


rules

inspect(head(sort(rules, by="lift"),15))

plot(rules, method="graph")
