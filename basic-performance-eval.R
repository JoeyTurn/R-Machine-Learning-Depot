#evaluating performance

#from sms classification,
sms_results <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter10/sms_results.csv", stringsAsFactors = TRUE)
head(sms_results)
head(subset(sms_results, actual_type != predict_type))

#some package is messing with crosstable, can't find which
library(gmodels)
crossTable(x= sms_results$actual_type, y = sms_results$predict_type)

#install.packages("caret") #caret has confusion matrix within
library(caret) #masks kernlab's alpha obj
confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")
#kappa statistic adjusts accuracy by accounting for chance predictions
#kappa = (Pr(a) - Pr(e))/(1-Pr(e)) ; a = actual, e = expected

#install.packages("vcd"); install.packages("irr") #vcd = visualizing categorical data, irr = inter-rater reliability
#library(vcd); library(irr)
Kappa(table(sms_results$actual_type, sms_results$predict_type))
kappa2(sms_results[1:2])

#caret has sensitivity() and specitivity() methods
#caret also has precision, method call of posPredValue(), and recall with sensitivity()

#F-measure (F1 score): combines precision and recall using the harmonic mean
# F-measure = 2*TP/(2*TP + FP + FN) = (2*prec*rec)/(prec+rec)

#visualizing performance tradeoffs

#install.packages("ROCR") #ROC = receover operating characteristic
library(ROCR) #masked prediction from neuralnet
pred <- prediction(predictions = sms_results$prob_spam, labels = sms_results$actual_type)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

#perfect classifier = greek Gamma, test classifier = ln(x), classifier without pred power: y=x
plot(perf, main = "ROC curve for SMS spam filter", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2) #
perf.auc <- performance(pred, measure = "auc") #auc = area under curve
str(perf.auc)
unlist(perf.auc@y.values) #turns list vec to num val.. note @ instead of $
#y.values shows the AUC for our classifier is .98, very good (at least for this data)

#using val data (holdout method)
credit <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter10/credit.csv", stringsAsFactors = TRUE)
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:700], ]
credit_val <- credit[random_ids[701:800], ]
credit_test <- credit[random_ids[801:1000], ]

#using stratified random sampling
library(caret)
in_train <- createDataPartition(credit$default, p = .75, list = FALSE)
credit_train <- credit[in_train, ]
credit_test <- credit[-in_train, ]

#using crossval
#library(caret)
folds <- createFolds(credit$default, k = 10)
str(folds)
#book doesn't correctly do crossval, though this is what it should have done:
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:800], ]
credit_test <- credit[random_ids[801:1000], ]
folds <- createFolds(credit_train$default, k = 8)
credit_01_train <- credit[folds$Fold1, ]
credit_01_val <- credit[-folds$Fold1, ]

#automating crossval
#again, i redid it to have the crossval actually be correct with a val set
library(caret); library(C50); library(irr)
folds <- createFolds(credit_train$default, k = 8)
cv_results <- lapply(folds, function(x) {
  credit_train <- credit[x, ]
  credit_val <- credit[-x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_val)
  credit_actual <- credit_val$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})
str(cv_results)
mean(unlist(cv_results)) #poor mean, not very good

#bootstrapping = using samples of data to estimate properties of larger ds (isnt this just statistics?)
#bootstrapping differs from k-means crossval in that bootstrapping uses replacement
#.632 bootstrap = special case of bootstrapping that accounts for perforance issues caused by small dataset
#.632 bs error = .632*error_train + .368*error_test
