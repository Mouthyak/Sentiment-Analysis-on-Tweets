library(stringr)
a <- read.csv("demonetization-tweets.csv")
str(a)
b <- as.character(a$text)
sentiment.score <- function(sentences, positive.words, negative.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, positive.words, negative.words)
  {
    
    ## clean up sentences with R's regex-driven global substitute, gsub():
    
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    
    # remove retweets
    sentence <- gsub('(RT|via)((?:\\b\\W*@\\W+)+)', '', sentence)
    
    # remove at people
    sentence <- gsub('@\\w+', '', sentence)
    
    # remove punctuations
    sentence <- gsub('[[:punct:]]', '', sentence)
    
    # remove numbers
    sentence <- gsub('[[:digit:]]', '', sentence)
    
    # remove html links
    sentence <- gsub('http[s]?\\w+', '', sentence)
    
    # remove extra spaces
    sentence <- gsub('[ \t]{2,}', '', sentence)
    sentence <- gsub('^\\s+|\\s+$', '', sentence)
    
    # removing NA's
    sentence <- sentence[!is.na(sentence)]
    
    # convert to lower case:
    sentence <- tolower(sentence)
    
    # split into words. str_split is in the stringr package
    
    word.list <- str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    
    words <- unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    negative.matches <- match(words, negative.words)
    positive.matches <- match(words, positive.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    
    positive.matches <- !is.na(positive.matches)
    negative.matches <- !is.na(negative.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    
    score <- sum(positive.matches) - sum(negative.matches)
    #list1 <- c(score)
    return(score)
  }, positive.words, negative.words, .progress=.progress )
  
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

positive <- scan("positive-words.txt", what= "character", comment.char= ";")
negative <- scan("negative-words.txt", what= "character", comment.char= ";")
b.analysis <- sentiment.score(b, positive, negative, .progress="none")

head(b.analysis)
table = data.frame(text=b.analysis$text,Score=b.analysis$score)

mydata <- c(table$Score)
write.csv(mydata,"C:/Users/Pummy/Documents/mydata.csv")

  