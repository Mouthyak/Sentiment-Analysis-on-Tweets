df<-read.csv(choose.files(),stringsAsFactors = FALSE)

df_train <- df[1:6012, ]
df_test <- df[6013:8016, ]
df_train$class <- as.factor(df_train$class)
df_test$class <- as.factor(df_test$class)
head(df_train)
mean(sapply(sapply(df_train$text, strsplit, " "), length))
library(tm)
corpus <- Corpus(VectorSource(c(df_train$text,df_test$text)))
corpus
corpus[1]$content
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus[1]$content
dtm <- DocumentTermMatrix(corpus)
dtm
sparse <- removeSparseTerms(dtm, 0.99)
sparse
important_words_df <- as.data.frame(as.matrix(sparse))
colnames(important_words_df) <- make.names(colnames(important_words_df))
# split into train and test
important_words_train_df <- head(important_words_df, nrow(df_train))
important_words_test_df <- tail(important_words_df, nrow(df_test))

# Add to original dataframes
train_data_words_df <- cbind(df_train, important_words_train_df)
test_data_words_df <- cbind(df_test, important_words_test_df)

# Get rid of the original Text field
train_data_words_df$text <- NULL
test_data_words_df$text <- NULL

library(caTools)
set.seed(1234)
# first we create an index with 85% True values based on Sentiment
spl <- sample.split(train_data_words_df$class, .85)
# now we use it to split our data into train and test
eval_train_data_df <- train_data_words_df[spl==T,]
eval_test_data_df <- train_data_words_df[spl==F,]
log_model <- glm(class~., data=eval_train_data_df, family=binomial)
summary(log_model)
log_pred <- predict(log_model, newdata=eval_test_data_df, type="response")

# Calculate accuracy based on prob

log_pred_test <- predict(log_model, newdata=test_data_words_df, type="response")

length(log_pred_test)
log_pred_test<-ifelse(log_pred_test>0.5,"positive","Negative")
length(df_test$class)

library(caret)
conf.mat <- confusionMatrix(data = log_pred_test, df_test$class)
conf.mat
