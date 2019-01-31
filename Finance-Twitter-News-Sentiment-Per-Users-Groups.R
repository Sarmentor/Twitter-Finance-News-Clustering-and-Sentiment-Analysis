########################################
###     Twitter  User Modelling      ###
########################################


# Import the required libraries
rm(list=ls())
library(devtools)
library(rtweet)
library(tm)
library(wordcloud)
library(ggplot2)
library(fpc)
library(qdap)
library(igraph)
library(stats)
library(lsa)


#Application Details
APIKey=''
APISecret=''
AccessToken=''
AccessTokenSecret=''

# Set up authentication
Auth<-create_token(consumer_key=APIKey, consumer_secret=APISecret,access_token = AccessToken, access_secret =AccessTokenSecret)


#List of twitter accounts
#Finance related
list.tweet.latest.news <- c(
  'Schuldensuehner',
  'LizAnnSonders',
  'NorthmanTrader',
  'Frances_Coppola',
  'bySamRo',
  'ErikFossing',
  'GaveKalCapital',
  'CiovaccoCapital',
  'JLyonsFundMGMT',
  'TDANSherrod',
  'SconsetCapital',
  'Samir_Madani',
  'Amena__Bakr',
  'Brenda_Kelly',
  'IvanTheK',
  'StockCats',
  'Financial_Orbit',
  'hmeisler',
  'MarkYusko',
  'Livesquawk',
  'katie_martin_fx',
  'faithmight',
  'vexmark',
  'andrewnyquist',
  'TheStalwart',
  'Sassy_SPY',
  'RyanDetrick',
  '_SeanDavid',
  'TheBubbleBubble',
  'AnneMarieTrades',
  'KeithMcCullough',
  'ritholtz',
  'JeffMacke',
  'IGSquawk',
  'ChrisWeston_IG',
  'EvanLucas_IG', 
  'FXCM',
  'Kitjuckes',
  'callieabost',
  'Ukarlewitz',
  'PredictedMkts',
  'jsblokland',
  'Robeco',
  'TihoBrkan',
  '5_min_macro',
  'RaoulGMI',
  'z8angela',
  'zerohedge',
  'AndyBiotech',
  'Stephanie_Link',
  'marketminute',
  'JustinWolfers',
  'cullenroche',
  'JLyonsFundMgmt',
  'Ole_S_Hansen',
  'JoelKruger',
  'LMT978',
  'russian_market'
)


#Fetching tweets of input Hashtag 
fetchHashtag <- function(hashtag,number) {
  tweets <- search_tweets(q=hashtag, n=number)
  return(tweets)
}

#Fetching tweets of input list of Financial Gurus 
fetchUserTweets <- function(userlist, ntweets=5){
  last.tweets <- get_timelines(userlist, n=ntweets)
  return(last.tweets)
}



# Pre-processing of Corpus
makeCorpus <- function(text){ #Function for making corpus and cleaning the tweets fetched
  twitterdf <- data.frame(text=text, stringsAsFactors = FALSE)#store the fetched tweets as a data frame
  twitterdf$text <- sapply(twitterdf$text,function(row) iconv(row, "latin1", "ASCII", sub=""))#Removing emoticons from tweets
  twitterCorpus <- Corpus(VectorSource(twitterdf$text)) #Creating Corpus
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) #function to replace a pattern to white space using regex
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)") #match rt or via
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "@\\w+") #match @
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "[ \t]{2,}") #match tabs
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "[ |\n]{1,}") #match new lines
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "^ ") #match white space at begenning
  twitterCorpus <- tm_map(twitterCorpus, toSpace, " $") #match white space at the end
  twitterCorpus <- tm_map(twitterCorpus, PlainTextDocument)
  twitterCorpus <- tm_map(twitterCorpus, removeNumbers)
  twitterCorpus <- tm_map(twitterCorpus, removePunctuation)
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "http[[:alnum:]]*") #remove url from tweets
  twitterCorpus <- tm_map(twitterCorpus,removeWords,stopwords("en"))
  twitterCorpus <- tm_map(twitterCorpus, content_transformer(tolower))
  return(twitterCorpus)
}


#df.tag.tweets <- fetchHashtag('bitcoin', 1000)
df.list <- fetchUserTweets(list.tweet.latest.news)

head(df)

#Wordcloud
makeWordcloud<-function (getText){ #plotting wordcloud
  twicorpus<-makeCorpus(getText)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf))) #Create TDM
  matrix<-as.matrix(myTdm)
  wordFreq <- sort(rowSums(matrix), decreasing=TRUE)#find frequency of words and sorting them in descending
  set.seed(375) 
  grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
  wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,colors=grayLevels)
}




freqPlot<-function (getText){ #frequency plot of word count
  twicorpus<-makeCorpus(getText)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf)))
  matrix<-as.matrix(myTdm)
  termFrequency <- rowSums(matrix)
  termFrequency <- subset(termFrequency, termFrequency>=10)
  df <- data.frame(term=names(termFrequency), freq=termFrequency)
  ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + coord_flip()
}


#Clustering content
hCluster.content<-function (content){ #hierarchical clustering 
  twicorpus<-makeCorpus(content)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf)))
  myTdm2 <- removeSparseTerms(myTdm, sparse=0.98) #removing sparse terms
  m2 <- as.matrix(myTdm2)
  distMatrix <- dist(scale(m2)) #calculating distance between terms
  fit <- hclust(distMatrix, method="ward.D") #clustering terms
  plot(fit)
  rect.hclust(fit, k=5) #cutting the tree into 5 clusters
  (groups <- cutree(fit, k=5))
}

#Clustering users
hCluster.users<-function (sim.matrix){ #hierarchical clustering
  fit <- hclust(sim.matrix, method="ward.D") #clustering users
  plot(fit)
  rect.hclust(fit, k=5) #cutting the tree into 5 clusters
  (clust.groups <<- cutree(fit, k=5))
}


#Sentiment Analysis
tSentimen<-function (content){
  twicorpus<-makeCorpus(content)
  df<-data.frame(text=twicorpus$content, stringsAsFactors=F) # storing corpus as data frame
  poldat <- with(df, polarity(text)) #getting polarity of the tweets
  return(poldat)
}

#just for example, fetch latest twitter user news
ntweets <- 5 #5 tweets per user

get <- fetchUserTweets(list.tweet.latest.news, ntweets)

#aggregate several tweets by unique user_id
all.text.user <- aggregate(text ~ user_id + name, data = get, paste, collapse = " ")

#new dataframe, this time compatible with tm package (doc_id needed as a variable)
all.text.user <- data.frame(doc_id = all.text.user$name, text=all.text.user$text, stringsAsFactors = FALSE)

#create corpus
corpus <- Corpus(DataframeSource(all.text.user))

#convert to UTF8
corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))

#convert corpus into lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

#remove punctuation
corpus <- tm_map(corpus, removePunctuation)

#remove numbers
corpus <- tm_map(corpus, removeNumbers)


Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('\\b+RT', '', x) ## Remove RT
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]', '', x) ## Remove Punctuations
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub(' +',' ',x) ## Remove extra whitespaces
}

corpus <- tm_map(corpus,Textprocessing)

# remove extra whitespace
corpus <- tm_map(corpus, stripWhitespace)

dtm.text <- DocumentTermMatrix(corpus, control = list(weighting =
                                                        function(x)
                                                          weightTfIdf(x, normalize =
                                                                        TRUE)))

#make costable
costable.dtm.text <- cosine(t(as.matrix(dtm.text)))
costable.dtm.text <- round(costable.dtm.text, digits = 2)


#global word count
freqPlot(all.text.user$text) #creating frequency plots

#global word cloud
makeWordcloud(all.text.user$text) #creating wordcloud

#cluster similarity
hCluster.users(as.dist(costable.dtm.text)) #hierarchical clustering

#sentiment analysis per cluster of users
for (i in 1:5){
  users.clust <- names(clust.groups[which(clust.groups==i)])
  text.clust <- paste(all.text.user[which(all.text.user$doc_id %in% users.clust),"text"])
  cat("Sentiment is ", tSentimen(text.clust)$group$stan.mean.polarity, " in cluster ", i, "\n") #sentiment
}

#wordcloud analysis per cluster of users
for (i in 1:5){
  users.clust <- names(clust.groups[which(clust.groups==i)])
  text.clust <- paste(all.text.user[which(all.text.user$doc_id %in% users.clust),"text"])
  makeWordcloud(text.clust)
}
