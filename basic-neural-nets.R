#neural nets

concrete <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter07/concrete.csv", stringsAsFactors = FALSE)
str(concrete)
min_max <- function(x) {
  return((x-min(x)) / (max(x) - min(x)))
}
concrete_norm <- as.data.frame(lapply(concrete, min_max))
summary(concrete_norm$strength)
summary(concrete$strength)
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

#install.packages("neuralnet")
library(neuralnet)
concrete_model <- neuralnet(strength ~ ., data = concrete_train)
plot(concrete_model)
model_results <- compute(concrete_model, concrete_test[1:8]) #compute has $neurons and $net.results (predicted vals)
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)

#improving the model
concrete_model2 <- neuralnet(strength ~ ., data = concrete_train, hidden = 5)
plot(concrete_model2)
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

#more params
concrete_model3 <- neuralnet(strength ~ ., data = concrete_train, hidden = 10, threshold = .005, stepmax = 1e+06, rep = 5)
plot(concrete_model3)
model_results3 <- compute(concrete_model3, concrete_test[1:8])
predicted_strength3 <- model_results3$net.result
cor(predicted_strength3, concrete_test$strength)

#more hidden, less runtime (i hope)
concrete_model4 <- neuralnet(strength ~ ., data = concrete_train, hidden = c(4, 4, 2), threshold = .01, stepmax = 5e+04, rep = 2)
plot(concrete_model4)
model_results4 <- compute(concrete_model4, concrete_test[1:8])
predicted_strength4 <- model_results4$net.result
cor(predicted_strength4, concrete_test$strength)
