library(tm)
#library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
#library(randomForest)

tweets <- read.csv(choose.files(),stringsAsFactors = F)

table(tweets$class)

corpus <- Corpus(VectorSource(tweets$text))
corpus[[1]]$content

# Convert to lower-case.
corpus <- tm_map(corpus, tolower)
#corpus <- tm_map(corpus, PlainTextDocument)
corpus[[1]]$content

corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content

corpus <- tm_map(corpus, removeWords, c('apple', stopwords('english')))
corpus[[1]]$content

corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content

frequencies <- DocumentTermMatrix(corpus)
inspect(frequencies[1000:1005, 505:515])

# Only 56 terms appear 20 or less times in our tweets.
findFreqTerms(frequencies, lowfreq = 20)

# Remove these words that are not used very often. Keep terms that appear in 0.5% or more of tweets.
# Only 309 terms now (out of previous 3289 terms).
sparse <- removeSparseTerms(frequencies, 0.995)

tweetsSparse <- as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
tweetsSparse$class <- tweets$class

# Build a training and testing set.
set.seed(123)
split <- sample.split(tweetsSparse$class, SplitRatio=0.7)
trainSparse <- subset(tweetsSparse, split==TRUE)
testSparse <- subset(tweetsSparse, split==FALSE)
library(rpart)
library(rpart.plot)
# Build a CART classification regression tree model on the training set.
tweetCART <- rpart(class ~ ., data=trainSparse, method='class')
prp(tweetCART)

predictCART <- predict(tweetCART, newdata=testSparse, type='class')
c1<-table(testSparse$class, predictCART)
c1
c2<-paste("accuracy is:",(round(sum(diag(c1))/sum(c1)*100,digits=2)),"%")
c2

library(caret)
conf.mat <- confusionMatrix(data = predictCART, testSparse$class)
conf.mat
