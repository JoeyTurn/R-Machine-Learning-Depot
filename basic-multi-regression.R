#solving multireg

reg <- function(x, y) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  solve(t(x) %*% x) %*% t(x) %*% y
}

insurance <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter06/insurance.csv", stringsAsFactors = FALSE)
str(insurance); summary(insurance)
hist(insurance$expenses)
table(insurance$region)
cor(insurance[c("age", "bmi", "children", "expenses")])
pairs(insurance[c("age", "bmi", "children", "expenses")])
#install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")]) #upper = correlation matrix
#oval shaped obj = correlation ellipse, provides visualization of how strongly correlated the variables are
#curve drawn = loess smooth, indicates general relationship between variables
#ie pannel 2,1 shows bmi gradually increases as age increases
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region, data = insurance)
#can also be called as
ins_model_same <- lm(expenses ~ ., data = insurance) #as . character specifies all features
ins_model
#things like sexmale have sex(male) = 1, sex(female) = 0
#can relevel using relevel(attrib, ref = 'marker')
summary(ins_model)
#r^2 adj corrects R-squared by penalizing models with a large number of indep variables

#adding non-linear variables
insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
ins_model2 <- lm(expenses ~ ., data = insurance)
summary(ins_model2)
