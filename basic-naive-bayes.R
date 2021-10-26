#naïve Bayes

#data taken from PacktPublishing's ML book, is spam/ham email data
sms_raw <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter04/sms_spam.csv", stringsAsFactors = FALSE)

#looking at/messing with data
head(sms_raw)
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

#tm lib gives Corpus & VectorSource & tm_map, tm_map let's use use apply() to a Corpus object, don't know about the other two
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:3])
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords()) #stopwords() removes common words "and", "to", "by", ...
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

sms_dtm <- DocumentTermMatrix(corpus_clean)

#train_test_splitting
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5559, ]
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]

#let's look at the prop tables for the train & test raw data
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_test, type == "ham")

#wordcloud for fun!
library(wordcloud)
wordcloud(spam$text, max.words = 40, scale = c(3, .5))
wordcloud(ham$text, max.words = 40, scale = c(3, .5))


findFreqTerms(sms_dtm_train, 5)
sms_dict <- c(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

#ifelse checks first arg, returns second arg if true, third if false
convert_counts <- function(x) {
  x <- ifelse(x>0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

#
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)

#contains our naïve bayes classifier
library(e1071)

sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_test_pred <- predict(sms_classifier, sms_test)

#again for crosstable
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type, prop.chisq = FALSE, dnn = c('predicted', 'actual'))
