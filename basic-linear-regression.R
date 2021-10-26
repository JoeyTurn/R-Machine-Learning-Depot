#linear discetion

#dataset used from PacktPublishing ML book
usedcars <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter02/usedcars.csv", stringsAsFactors = FALSE)

#check basic summary of dataset
usedcars$year
summary(usedcars)
summary(usedcars[c("price", "mileage")])
quantile(usedcars$price, seq(from = 0, to = 1, by = .2)) 
boxplot(usedcars$price, main = "Boxplot of Used Car Prices", ylab = "Price in $")
hist(usedcars$price, main = "Histogram of Used Car Prices", xlab = "Price in $")
var(usedcars$price); sd(usedcars$price)

#not specifically needed, just for fun / to mess around with
#linear_model_used_cars <- lm(usedcars$price ~ usedcars$mileage * usedcars$year)
#summary(linear_model_used_cars)

model_table <- table(usedcars$year)
prop.pct <- prop.table(model_table)*100; round(prop.pct, digits = 1)

#making model based on price and mileage
plot(x = usedcars$mileage, y = usedcars$price, main = "Scatterplot of Price v Mileage", xlab ="Used Car Odometer (mi.)", ylab = "Used Car Price ($)")
model.1 <- lm(usedcars$price ~ usedcars$mileage)
abline(model.1)

#next part somewhat outside of linear regression, but kept for any potential later reference

#install.packages("gmodels")
library(gmodels)
usedcars$conservative <- usedcars$color %in% c("Black", "Gray", "Silver", "White")
table(usedcars$conservative)
CrossTable(x = usedcars$model, y = usedcars$conservative)
