## Read the tweet data set
tweets = read.csv('Binary Classification_ Twitter sentiment analysis.csv', 
                  header = TRUE, stringsAsFactors = FALSE)
colnames(tweets) <- c("sentiment", "tweets") # Set the column names
tweets[, 'sentiment'] = ifelse(tweets$sentiment == 4, 1, 0)  # set sentiment to {0,1}
head(tweets) # Have a look at the data frame

## Create a tm text corpus from the tweets
library(tm)  ## tm package for text mining
temp = VectorSource(tweets['tweets'])
str(temp)
tweet.corpus <- Corpus(temp)
# What is the class of the corpus
cat('')
class(tweet.corpus)

## Normalize tweets text
tweet.corpus <- tm_map(tweet.corpus, content_transformer(removeNumbers))
tweet.corpus <- tm_map(tweet.corpus, content_transformer(removePunctuation))
tweet.corpus <- tm_map(tweet.corpus, content_transformer(stripWhitespace))
tweet.corpus <- tm_map(tweet.corpus, content_transformer(tolower))

## ----- Convert the corpus to a term document matrix
to.tdm = function(corpus, sparse = 0.998){
  require(tm)
  ## Compute a term-document matrix and then 
  require(slam) # Sparse matrix package
  tdm <- TermDocumentMatrix(corpus, control = list(stopwords = FALSE))
  tdm <- removeSparseTerms(tdm, sparse)
  tdm
}
tdm = to.tdm(tweet.corpus) # Create a term document matrix
str(tdm) # Look at sparse tdm
findFreqTerms(tdm, 2000) # Words that occur at least 2000 times


## Compute the word fequency from the tdm
to.wf = function(tdm){
  ## compute the word frequencies.
  require(slam)
  freq <- row_sums(tdm, na.rm = T)   
  ## Sort the word frequency and build a dataframe
  ## including the cumulative frequecy of the words.
  freq <- sort(freq, decreasing = TRUE)
  word.freq <- data.frame(word = factor(names(freq), levels = names(freq)), 
                          frequency = freq)
  word.freq['Cumulative'] <- cumsum(word.freq['frequency'])/sum(word.freq$frequency)
  word.freq
}
wf = to.wf(tdm)
head(wf, n = 10)


## Make a bar chart of the word frequency
word.bar = function(wf, num = 50){
  require(ggplot2)
  ggplot(wf[1:num,], aes(word, frequency)) +
    geom_bar(stat = 'identity') +
    ggtitle('Frequency of common words') +
    ylab('Frequency') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
word.bar(wf)


## Make cumulative distribution plots of the most frequent words
word.cdf = function(wf, num = 50){
  require(ggplot2)
  ggplot(wf[1:num,], aes(word, Cumulative)) +
    geom_bar(stat = 'identity') +
    ggtitle('Cumulative fraction of common words') +
    ylab('Cumulative frequency') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
word.cdf(wf)

## Load stop words from a file and ensure they are 
stopWords = read.csv('stopwords.csv', header = TRUE, stringsAsFactors = FALSE)
stopWords = unique(stopWords) # Ensure the list is unique
cat(nrow(stopWords))
stopWords[1:100,] # Look at the first 100 stop words

## Remove the stop words from the corpus
tweet.corpus <- tm_map(tweet.corpus, removeWords, stopWords[, 'words'])

## View the results
tdm = to.tdm(tweet.corpus) # Create a term document matrix
findFreqTerms(tdm, 2000) # Words that occur at least 2000 times
wf = to.wf(tdm)  # Compute word fequency
head(wf, n = 10)  # Look at the most common words
word.bar(wf) # Plot word frequency
word.cdf(wf) # Plot cdf

## Use the porter stemmer in Snowball package
##
require(SnowballC) ## For Porter stemming words
tweet.corpus <- tm_map(tweet.corpus, stemDocument)


## View the results
tdm = to.tdm(tweet.corpus, sparse = 0.99) # Create a term document matrix
findFreqTerms(tdm, 2000) # Words that occur at least 2000 times
wf = to.wf(tdm)  # Compute word fequency
head(wf, n = 10)  # Look at the most common words
word.bar(wf) # Plot word frequency
word.cdf(wf) # Plot cdf

## Compute a tdm
require(RTextTools)
model.matrix = create_matrix(tweets$tweets, language="english",                               
                             removeNumbers=TRUE,
                             stemWords=TRUE, 
                             removeSparseTerms=.998, 
                             removeStopwords = TRUE, 
                             stripWhitespace = TRUE,
                             toLower = TRUE)                            

## Create the a container for the tdm and label
tweet.cont = create_container(model.matrix, 
                              tweets$sentiment, 
                              trainSize = 1:120000, 
                              virgin=TRUE)

## Compute a logistic regresson model for sentiment classification
tweet.glmnet <- train_model(tweet.cont, "GLMNET")

## Test classification
tweet.class = classify_model(tweet.cont, tweet.glmnet)
tweet.metrics = create_analytics(tweet.cont, tweet.class)

## Examine some raw metrics
tweet.metrics@label_summary
cbind(head(tweet.metrics@document_summary, n = 10), head(tweets$sentiment, n = 10))

#----------------------------------------------
## Compute TFIDF weighted tdm
## Compute a tdm
tdm.tools2 = create_matrix(tweets$tweets, 
                           language="english",                               
                           removeNumbers=TRUE,
                           stemWords=TRUE, 
                           removeSparseTerms=.998, 
                           removeStopwords = TRUE, 
                           stripWhitespace = TRUE,
                           toLower = TRUE,
                           weighting = tm::weightTfIdf)


## Create the a container for the TfIdf weighted tdm and label
tweet.cont = create_container(tdm.tools2,tweets$sentiment, trainSize = 1:120000, virgin=TRUE)

## Compute a logistic regresson model for sentiment classification
tweet.glmnet.TfIdf <- train_model(tweet.cont,"GLMNET")

## Test classification
tweet.class.TfIdf = classify_model(tweet.cont, tweet.glmnet.TfIdf)
tweet.metrics.TfIdf = create_analytics(tweet.cont, tweet.class.TfIdf)

## Examine some raw metrics
tweet.metrics.TfIdf@label_summary
results = head(tweet.metrics.TfIdf@document_summary, n = 20)
results

## Look at the confusion matrix and compare to the unweighte tdf model
create_precisionmRecallSummary(tweet.cont, tweet.class.TfIdf)
create_precisionRecallSummary(tweet.cont, tweet.class)

## Load the data set as a vector corpus of 20 documents
library(tm)
data(crude)
writeLines(as.character(crude[[1]]))

## Compute the term document matrix
crude.tdm = TermDocumentMatrix(crude, control = list(removePunctuation = TRUE,
                                                     tolower = TRUE,
                                                     removePunctuation = TRUE,
                                                     removeNumbers = TRUE,
                                                     stopwords = TRUE,
                                                     stemming = TRUE))
## Have a look at the tdm 
inspect(crude.tdm[202:210, 1:10])


## Which terms occur 10 times or more?
crudeTDMHighFreq <- findFreqTerms(crude.tdm, 10, Inf)
crudeTDMHighFreq


# Do these terms show up in the first 5 documents?
inspect(crude.tdm[crudeTDMHighFreq, 1:5]) 

## Compute the DTM
crude.dtm = DocumentTermMatrix(crude, control = list(removePunctuation = TRUE,
                                                     stopwords = TRUE))
crude.dtm  ## Check the drm


## Apply a topic model to the news articles
##load topic models library
library(topicmodels)

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 5

ldaOut = LDA(crude.dtm, k, method= "Gibbs", 
             control = list(nstart = nstart, 
                            seed = seed, 
                            best = best, 
                            burnin = burnin, 
                            iter = iter, 
                            thin=thin))

## Examine the topics
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.topics

## And the terms
ldaOut.terms <- as.matrix(terms(ldaOut,6))
head(ldaOut.terms)

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
head(topicProbabilities)

#Find relative importance of top topic
topic1ToTopic2 <- lapply(1:nrow(crude.dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])
unlist(topic1ToTopic2)

#Find relative importance of second most important topics
topic2ToTopic3 <- lapply(1:nrow(crude.dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])
unlist(topic2ToTopic3)
