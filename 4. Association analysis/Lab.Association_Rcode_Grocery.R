###################################################
#
# Example: Association Analysis with Grocery Data 
#
###################################################

## Calling libraries

library(arules) #install.packages("arules")
library(arulesViz) #install.packages("arulesViz") 


## Reading data

Grocery = read.transactions("Grocery.csv", format = "single", cols = c(1,3), sep=",", skip=1, rm.duplicate=TRUE)
inspect(Grocery)

str(Grocery)
as(Grocery, "data.frame")[1:10,]


## Generating rules

rules = apriori(Grocery, parameter=list(support=0.1, confidence=0.7, minlen=2), control=list(verbose=F))
rules.sorted = sort(rules, by=c("support","lift")) #sorting data
inspect(rules.sorted)


## Generating a subset of rules

rules.sub = subset(rules, subset = rhs %in% "heineken" & lift > 1) 
inspect(rules.sub)

rules.sub = subset(rules, subset = lhs %in% "heineken" & lift > 1) 
inspect(rules.sub)


## Plotting

plot(rules)
plot(rules, measure = c("support", "lift"), shading = "confidence")


#END
