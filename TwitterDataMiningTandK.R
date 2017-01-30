
fetchTweets <- function(query, num, lang) # return a data frame
{
  # extract tweets by related R package and twitter's API
  library(twitteR)
  # Declare Twitter API Credentials
  api_key <- "" # From dev.twitter.com
  api_secret <- "" # From dev.twitter.com
  token <- "" # From dev.twitter.com
  token_secret <- "" # From dev.twitter.com
  
  # Create Twitter Connection
  setup_twitter_oauth(api_key, api_secret, token, token_secret)
  
  # Run Twitter Search and fetch data from twitter. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until)
  rawTweets <- searchTwitter(query, n=num, lang=lang)
  # Transform tweets list into a data frame
  rawTweets <- twListToDF(rawTweets)
}
#natural language processing, use package "tm" or "RTextTools". Here I use "tm"
rawTextProcess <- function(my_text,num_remove=FALSE,keywords_remove=FALSE, keywords, output)
#if output=0 return character; if output=1 return DocumentTermMatrix with columns as each word's frequency
{
  #preprocess raw tweets text
  library(tm)
  docs <- Corpus(VectorSource(my_text))
  #remove weird characters appear in the text
  docs = tm_map(docs, str_replace_all,"[^[A-Za-z0-9]]", " ")
  #remove extra space
  docs <- tm_map(docs, stripWhitespace)
  #remove punctuation
  docs <- tm_map(docs, removePunctuation)
  if(num_remove)
    #remove numbers if num_remove is true
    docs <- tm_map(docs, removeNumbers)
  #convert all characters to lower case
  docs <- tm_map(docs, tolower)
  #remove all stopwords in English
  docs <- tm_map(docs, removeWords, stopwords("english"))
  if(keywords_remove)
  #remove user defined keywords
    docs <- tm_map(docs, removeWords, keywords)   
  library(SnowballC)   
  docs <- tm_map(docs, stemDocument) 
  docs <- tm_map(docs, PlainTextDocument)  
  
  if(output==0)
  {
    #output as dataframe
    df <- data.frame(text = sapply(docs, as.character), stringsAsFactors = FALSE)
    return (df)
  }
  if(output==1)
  {
    #output as document term matrix
    dtm <- DocumentTermMatrix(docs)   
    dtm <- as.matrix(dtm)
    #draw word cloud for top high frequent words
    freq <- sort(colSums(dtm), decreasing=TRUE)   
    library(wordcloud)
    set.seed(142)   
    wordcloud(names(freq), freq, min.freq=5, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
    return (dtm)
  }
}
    
# Run Twitter Search and fetch data from twitter. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until)
rawTweets <- searchTwitter("taylor swift kanye west", n=5000, lang="en", since="2016-06-20")
# Transform tweets list into a data frame
rawTweets <- twListToDF(rawTweets)
#collect tweets data
tweets.text <- rawTextProcess(rawTweets$text,num_remove=TRUE,keywords_remove=TRUE,keywords = c("https", "taylor","swift","kanye","west","justin","bieber","kim","kardashian"))

## try to fetch replies of taylorswift13's latest tweets
# Run Twitter Search and fetch data from twitter. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until)
rawTweetsReply <- searchTwitter("to:taylorswift13", n=2000, lang="en", since="2016-07-17")
# Transform tweets list into a data frame
rawTweetsReply <- twListToDF(rawTweetsReply)
rawTextProcess(rawTweetsReply$text)

rawTweetsPos <- searchTwitter("to:taylorswift13:)", n=2000, lang="en", since="2016-08-01")
# Transform tweets list into a data frame
rawTweetsPos <- twListToDF(rawTweetsPos)
KPPosMat <- rawTextProcess(rawTweetsPos$text)
KPPosMat$emotion <- rep("positive",nrow(KPPosMat))

rawTweetsNeg <- searchTwitter("to:taylorswift13:(", n=2000, lang="en", since="2016-08-01")
# Transform tweets list into a data frame
rawTweetsNeg <- twListToDF(rawTweetsNeg)
KPNegMat <- rawTextProcess(rawTweetsNeg$text)
KPNegMat$emotion <- rep("negative",nrow(KPNegMat))
 
#KPMixMat <- rbind(KPPosMat,KPNegMat)
KPMixMat <- rbind(KPPosMat[1:2000,],KPNegMat[1:2000,])
#library(e1071)
#classifier = naiveBayes(KPMixMat, as.factor(KPMixMat[,2]) )
#predicted = predict(classifier, KPMixMat[11:15,])
#predicted

library(caret)
set.seed(3456)
fit3 <- train(emotion ~ ., 
              data=KPMixMat,
              method = "nb")

pred3 <- predict(fit3, KPMixMat, type="raw")
confusionMatrix(pred3, KPMixMat$emotion)


