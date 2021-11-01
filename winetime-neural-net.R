#winetime

wine <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter06/whitewines.csv", stringsAsFactors = FALSE)
str(wine)
library(neuralnet)
hist(wine$quality)
normalize <- function(x) {
  return ((x-mean(x))/sd(s))
}
wine_minmax <- as.data.frame(lapply(wine, min_max))
wine_train <- wine_minmax[1:3918, ]
wine_test <- wine_minmax[3919:4898, ]
wine_model <- neuralnet(quality ~ ., data = wine_train, hidden = c(5, 5, 2), stepmax = 5e+06) #model didn't converge with low stepmax
plot(wine_model)
cor(compute(wine_model, wine_test)$net.result, wine_test$quality)
wine_model
