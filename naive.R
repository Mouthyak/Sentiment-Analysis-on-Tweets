library('tm')
library('dplyr')
library('e1071')
df<-read.csv("demon2.csv",stringsAsFactors = FALSE)
glimpse(df)
set.seed(1)
df<-df[sample(nrow(df)),]
df<-df[sample(nrow(df)),]
glimpse(df)
df$class <- as.factor(df$class)
corpus<-Corpus(VectorSource(df$text))
corpus
inspect(corpus[1:3])
corpus.clean<-corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)
dtm<- DocumentTermMatrix(corpus.clean)
inspect(dtm[40:50, 10:15])
df.train<-df[1:6012,]
df.test<-df[6013:8016,]
dtm.train<-dtm[1:6012,]
dtm.test<-dtm[6013:8016,]
corpus.clean.train <- corpus.clean[1:6012]
corpus.clean.test <- corpus.clean[6013:8016]
dim(dtm.train)
fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))
dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)
dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

dim(dtm.train.nb)

# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}
# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)
# Train the classifier
system.time( classifier <- naiveBayes(trainNB, df.train$class, laplace = .5) )
# Use the NB classifier we built to make predictions on the test set.
system.time(pred <- predict(classifier, newdata=testNB) )

# Create a truth table by tabulating the predicted class labels with the actual class labels 

table("Predictions"= pred , "Actual" = df.test$class)
library(gmodels)
CrossTable(pred,df.test$class,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted','actual'))

# Prepare the confusion matrix
library(caret)
conf.mat <- confusionMatrix(pred, df.test$class)
conf.mat