
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

rawTextProcess <- function(my_text,num_remove=FALSE,keywords_remove=FALSE, keywords, output)
#if output=0 return character; if output=1 return DocumentTermMatrix with columns as each word's frequency
{
  #preprocess raw tweets text
  library(tm)
  docs <- Corpus(VectorSource(my_text))
  #remove weird characters appear in the text
  docs = tm_map(docs, str_replace_all,"[^[A-Za-z0-9]]", " ")
  
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, removePunctuation)
  if(num_remove)
    docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, tolower)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  if(keywords_remove)
    docs <- tm_map(docs, removeWords, keywords)   
  library(SnowballC)   
  docs <- tm_map(docs, stemDocument) 
  docs <- tm_map(docs, PlainTextDocument)  
  
  if(output==0)
  {
    df <- data.frame(text = sapply(docs, as.character), stringsAsFactors = FALSE)
    return (df)
  }
  if(output==1)
  {
    dtm <- DocumentTermMatrix(docs)   
    dtm <- as.matrix(dtm)
    return (dtm)
  }
}

rawTweetsKPPos <- fetchTweets(query = "to:katyperry:)", num = 500, lang="en")
KPPosMat <- rawTextProcess(rawTweetsKPPos$text,num_remove=FALSE,keywords_remove=TRUE,keywords = c("katy","perry","katyperry"), output=0)
KPPosMat$emotion <- rep("positive",nrow(KPPosMat))
#KPPosMat <- as.matrix(KPPosMat)

rawTweetsKPNeg <- fetchTweets(query = "to:katyperry:(", num = 500, lang="en")
KPNegMat <- rawTextProcess(rawTweetsKPNeg$text,num_remove=FALSE,keywords_remove=TRUE,keywords = c("katy","perry","katyperry"), output=0)
KPNegMat$emotion <- rep("negative",nrow(KPNegMat))
#KPNegMat <- as.matrix(KPNegMat)

#KPMixMat <- rbind(KPPosMat,KPNegMat)
KPMixMat <- rbind(KPPosMat[1:200,],KPNegMat[1:200,])
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


