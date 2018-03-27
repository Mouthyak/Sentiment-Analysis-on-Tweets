library(tm)
library(caTools)
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

corpus <- tm_map(corpus, removeWords,stopwords(kind="en"))
corpus[[1]]$content

corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content

frequencies <- DocumentTermMatrix(corpus)
inspect(frequencies[1000:1005, 505:515])

findFreqTerms(frequencies, lowfreq = 2)

# Remove these words that are not used very often. Keep terms that 
#appear in 0.5% or more of tweets.
sparse <- removeSparseTerms(frequencies, 0.995)

tweetsSparse <- as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
tweetsSparse$class <- tweets$class

# Build a training and testing set.
set.seed(123)
split <- sample.split(tweetsSparse$class, SplitRatio=0.7)
trainSparse <- subset(tweetsSparse, split==TRUE)
testSparse <- subset(tweetsSparse, split==FALSE)
# Random forest model
trainSparse$class<-as.factor(trainSparse$class)
#Import Library
library(e1071) #Contains the SVM 
# create model
model <- svm(class~.,data=trainSparse,kernel='linear',gamma=0.2,cost=100)

#Predict Output
preds <- predict(model,testSparse)
table(preds)
c1<-table(testSparse$class, preds)
c1
c2<-paste("accuracy is:",(round(sum(diag(c1))/sum(c1)*100,digits=2)),"%")
c2

library(caret)
conf.mat <- confusionMatrix(data = preds, testSparse$class)
conf.mat