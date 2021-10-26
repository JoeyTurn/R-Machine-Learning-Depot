#kNN

#uses PacktPublishing's ML csv file (women's breast cancer data)
wbcd <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter03/wisc_bc_data.csv", stringsAsFactors = FALSE)
wbcd <- wbcd[-1] #to remove column 1, (likely index column?)
table(wbcd$diagnosis)

wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c('B', 'M'), labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)

#z score function for later use
normalize <- function(x) {
  return ((x-mean(x))/sd(x))
}

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

#guess R has no train_test_split() sklearn method
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_levels <- wbcd[1:469, 1]
wbcd_test_levels <- wbcd[470:569, 1]

library(class) #to get the actual knn call
wbcd_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_levels, k = 21) #uses knn(train, test, class, k)
#usually use something close to # in training data (ie 21 approx sqrt(469))

#imports to use CrossTable
library(gmodels)
CrossTable(x = wbcd_test_levels, y = wbcd_pred, prop.chisq = FALSE) #in quadrants: Q1: FP; Q2: TN; Q3: FN; Q4: TP
#wbcd_n was supposed to be min-maxed to (x-min)/(max-min) where FN = .02, not normalized

#not great, less try improving
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ] #wbcd_n was supposed to be min-maxed to (x-min)/(max-min)
wbcd_train_levels <- wbcd[1:469, 1]
wbcd_test_levels <- wbcd[470:569, 1]
wbcd_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_levels, k = 21)
CrossTable(x = wbcd_test_levels, y = wbcd_pred, prop.chisq = FALSE)
#k=1 is better in this case, though that's very likely underfitting the data
