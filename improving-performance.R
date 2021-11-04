#improving performance

#using caret't train() function will train 150 different models for classification and regression
credit <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter10/credit.csv", stringsAsFactors = TRUE)
library(caret)
m <- train(default ~ ., data = credit)
c50credit <- train(default ~ ., data = credit, method = "C5.0")

p <- predict(m, credit)
table(p, credit$default)
head(predict(m, credit, type = "prob"))

ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
grid <- expand.grid(.model = "tree", .trials = c(1, 5, 10, 15, 20, 25, 30, 35), .winnow = "FALSE")
grid
#adding tuning param metric = "Kappa"
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa", trControl = ctrl, tuneGrid = grid)
m

#meta learning methods = combining + managing the predictions of multiple models 
#ensemble methods = methods based on idea that combining multiple weaker learners creates a strong learner
#combination function: governs how disagreements among the predictions are reconciled
#stacking = process of using the predictions of several models to train a final arbiter model
#ensemble perks: better generalizability, improved performance, distinct domain data synthesis, more nuanced understanding of different learning tasks

#bootstrap aggregating (bagging): generates a number of bootstrap training datasets that create models, then voting/averaging of models to predict
library(ipred)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credot_pred, credit$default)

library(caret)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag", trControl = ctrl)

#caret includes lots of bagging functions: ldaBag, plsBag, nbBag, svmBag, nnetBag
#bagging with svm
str(svmBag)
#creating a bagging control object
bagctrl <- bagControl(fit = svmBag$fit, predict = svmBag$pred, aggregate = svmBag$aggregate)
svmbag <- train(default ~ ., data = credit, "bag", trControl = ctrl, bagControl = bagctrl)
svmbag

#boosting = another popular ensemble-based method, boosts performance of weak learner to attain performance of strong learners
#AdaBoost (adaptive boosting): generates weak learners that iteratively learn a larger portion of difficult-to-classify examples

#random forests = focuses only on ensembles of decision trees
library(randomForest) #masks margin from ggplot2
rf <- randomForest(default ~ ., data = credit)
rf #oob (out-of-bag) error rate = unbiased estimate of test set error

library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16)) #mtry defines how many features are selected at each split
m_rf <- train(default ~ ., data = credit, method = "rf", metric = "Kappa",
              trControl = ctrl, tuneGrid = grid_rf)
#comparing rf to c5.0
grid_c50 <- expand.grid(.model = "tree", .trials = c(10, 20, 30, 40), .winnow = "FALSE")
m_c50 <- train(default ~., data = credit, method = "C5.0", metric - "Kappa",
               trControl = ctrl, tuneGrid = grid_c50)
m_rf; m_c50 #seems rf with mtry=16 is the winner with kappa = .361
