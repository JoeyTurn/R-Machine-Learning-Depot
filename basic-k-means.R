#k-means classification

#social network service data for teens
teens <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter09/snsdata.csv", stringsAsFactors = FALSE)
str(teens)
table(teens$gender, useNA = "ifany")
summary(teens$age)
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA) #now they're all teens
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$nb <- ifelse(is.na(teens$gender), 1, 0)
mean(teens$age, na.rm = TRUE)
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)
ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE))
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age) #assigns average age to all na values

interests <- teens[5:40]
interests_z  <- as.data.frame(lapply(interests, scale))
teen_clusters <- kmeans(interests, 5)
teen_clusters$size #hmm last cluster contains way too much data on first look
teen_clusters$centers

teens$cluster <- teen_clusters$cluster
aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
