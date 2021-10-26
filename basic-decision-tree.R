#decision trees

#infogain(F) = Entropy_pre_split(S_1) - Entropy_post_split(S_2)

#data from PacktPublishing, is pre-EU german bank credit info (no identifiables)
credit <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter05/credit.csv", stringsAsFactors = FALSE)
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

#gets random order of credit dataset
credit_rand <- credit[order(runif(1000)), ]
credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]
prop.table(table(credit_train$default))

#C5.0 is the most UTD decision tree algorithm
library(C50)
credit_train$default<-as.factor(credit_train$default)
credit_model <- C5.0(x = credit_train[-17], y = credit_train$default)
credit_model
summary(credit_model)

#predictions of credit based on C5.0 model
credit_pred <- predict(credit_model, credit_test)

#to get CrossTable again
library(gmodels)
CrossTable(credit_test$default, credit_pred, prop.r = FALSE, prop.c = FALSE, dnn = c('actual default', 'predicted default'))

#using adapive boosting (aka doing it a few times to get better trials)
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

#making a cost matrix, then using it to influence the credit model
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
#error_cost
credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, prop.chisq = FALSE, dnn = c('actual default', 'predicted default'))

#making my own, including both cost matrices and adaptive boosting
credit_boost_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost, trials = 10)
credit_boost_cost_pred <- predict(credit_boost_cost, credit_test)
CrossTable(credit_test$default, credit_boost_cost_pred, prop.chisq = FALSE, dnn = c('actual default', 'predicted default'))
