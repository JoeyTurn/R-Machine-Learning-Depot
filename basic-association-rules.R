#association rules

#measuring rule interest:
# support: measures how frequently item/data appears; support(x) = count(X)/N
# confidence: measures predictive power/accuracy; confidence(X->Y) = support(X, Y)/support(X)
#strong rules: both high support and high confidence

groceries_bad <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter08/groceries.csv", stringsAsFactors = FALSE)
#making sparse matrix since groceries.csv is...problematic
#install.packages("arules")
library(arules)
groceries <- read.transactions("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter08/groceries.csv", sep = ',')
summary(groceries)
inspect(groceries[1:5]) #first 5 transactions

itemFrequency(groceries[, 1:3]) #looks at prop of first three items being bought
itemFrequencyPlot(groceries, support = 0.1) #shows items with at least support% support
itemFrequencyPlot(groceries, topN = 20)
image(groceries[1:5]) #shows first 5 transactions where item (column) was purchased
image(sample(groceries, 100))

grocery_model <- apriori(groceries, parameter = list(support = .006, confidence = .25, minlen = 2))
grocery_model #has 463 association rules
summary(grocery_model)
# lift: measures how much more one item will be purchased relative to its typical purchase rate; lift(X -> Y) = confidence(X -> Y)/support(Y)
# strong lift = rule is important
inspect(grocery_model)
grocery_by_lift <- sort(grocery_model, by = 'lift')
inspect(head(grocery_by_lift))

berryrules <- subset(grocery_by_lift, items %in% "berries") #can also use partial matching %pin% or complete matching %ain%
inspect(head(berryrules))

#saving rules
write(grocery_by_lift, file = "groceryrules.csv", sep = ',', quote = TRUE, row.names = FALSE)

#as dataframe
grocerydf <- as(grocery_by_lift, "data.frame")
str(grocerydf)
summary(grocerydf)
