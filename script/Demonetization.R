library(wordcloud)
library(tm)
library(plyr)
library(ggplot2)
library(grid)
library(sentiment)
library(Rgraphviz)
library(lubridate)

# source("http://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
# install.packages('tm')
# install.packages('wordcloud')
# download.file("http://cran.cnr.berkeley.edu/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz", "Rstem_0.4-1.tar.gz")
# install.packages("Rstem_0.4-1.tar.gz", repos=NULL, type="source")
# install.packages("Rstem", repos = "http://www.omegahat.org/R")
# download.file("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", "sentiment.tar.gz")
# install.packages("sentiment.tar.gz", repos=NULL, type="source")

# The following three lines will read in the 
# CSV file to the workspace read for further analysis

df<- read.csv("../input/demonetization-tweets.csv", stringsAsFactors = FALSE)

# see how many unique Twitter accounts in the sample
length(unique(df$screenName)) 

# Create a new column of random numbers in place of the usernames and redraw the plots
# find out how many random numbers we need
n <- length(unique(df$screenName))
# generate a vector of random number to replace the names, we'll get four digits just for convenience 
randuser <- round(runif(n, 1000, 9999),0)
# match up a random number to a username
screenName <- unique(df$screenName)
screenName <- sapply(screenName, as.character)
randuser <- cbind(randuser, screenName)
# Now merge the random numbers with the rest of the Twitter data, and match up the correct random numbers with multiple instances of the usernames...
rand.df  <-  merge(randuser, df, by="screenName")


# determine the frequency of tweets per account
counts <- table(rand.df$randuser)
# create an ordered data frame for further manipulation and plotting
countsSort <- data.frame(user = unlist(dimnames(counts)), count = sort(counts, decreasing = TRUE), row.names = NULL)
# create a subset of those who tweeted at least 5 times or more
countsSortSubset <- subset(countsSort,countsSort$count > 0)
# extract counts of how many tweets from each account were retweeted
# first clean the twitter messages by removing odd characters
rand.df$text <- sapply(rand.df$text,function(row) iconv(row,to = 'UTF-8')) 
# remove @ symbol from user names
trim <- function (x) sub('@','',x) 
# pull out who the message is to
require(stringr)
rand.df$to <- sapply(rand.df$text, function(name) trim(name)) 
# extract who has been retweeted
rand.df$rt <- sapply(rand.df$text, function(tweet) 
  trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2])) 
# replace names with corresponding anonymising number 
randuser <- data.frame(randuser)
rand.df$rt.rand <- as.character(randuser$randuser)[match(as.character(rand.df$rt), 
                                                         as.character(randuser$screenName))]
# make a table with anonymised IDs and number of RTs for each account
countRT <- table(rand.df$rt.rand)
countRTSort <- sort(countRT)
# subset those people RTd at least twice
countRTSortSubset <- subset(countRTSort,countRT>2)
# create a data frame for plotting
countRTSortSubset.df <-data.frame(user = as.factor(unlist(dimnames(countRTSortSubset))), RT_count = as.numeric(unlist(countRTSortSubset)))
# combine tweet and retweet counts into one data frame
countUser <- merge(randuser, countsSortSubset, by.x = "randuser", by.y = "user")
TweetRetweet <- merge(countUser, countRTSortSubset.df, by.x = "randuser", by.y = "user", all.x = TRUE)
# create a Cleveland dot plot of tweet counts and retweet counts per Twitter account
# solid data point = number of tweets, letter R = number of retweets
require(ggplot2)
require(grid)
ggplot() +  
  geom_point(data = TweetRetweet, mapping =  aes(reorder(randuser, count), count), size = 3) + 
  geom_point(data = TweetRetweet, mapping =  aes(randuser, RT_count), size = 4, shape = "R") +
  xlab("Author") + 
  ylab("Number of messages") + 
  coord_flip() + 
  theme_bw() + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) +
  theme(axis.title.y = element_text(size = 14, angle=90)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))

# calculate the number of followers of each Twitter account
# extract the usernames from the non-anonymised dataset
# Note that this may take some time! And that's it's dependant on the Twitter
# API, which changes frequently.
users <- unique(df$screenName)
users <- sapply(users, as.character)
# make a data frame for further manipulation 
users.df <- data.frame(users = users, followers = "", stringsAsFactors = FALSE)
# loop to populate users$followers with a follower count obtained from Twitter API
for (i in 1:nrow(users.df)) 
{
  # tell the loop to skip a user if their account is protected 
  # or some other error occurs  
  result <- try(getUser(users.df$users[i])$followersCount, silent = FALSE);
  if(class(result) == "try-error") next;
  # get the number of followers for each user
  users.df$followers[i] <- getUser(users.df$users[i])$followersCount
  # tell the loop to pause for 60 s between iterations to 
  # avoid exceeding the Twitter API request limit
  # this is going to take a long time if there are a lot
  # of users, good idea to let it run overnight
  print('Sleeping for 60 seconds...')
  Sys.sleep(60); 
}
# merge follower count with number of tweets per author
followerCounts <- merge(TweetRetweet, users.df, by.x = "screenName", by.y = "users")
# convert to value to numeric for further analysis
followerCounts$followers <- as.numeric(followerCounts$followers)
followerCounts$counts <-    as.numeric(followerCounts$counts)

# create a plot
ggplot(data = followerCounts, aes(count, followers)) +
  geom_text(aes(label = randuser, size = RT_count)) +
  scale_size(range=c(3,10)) +
  scale_x_log10(breaks = c(10,20,40,60,80,100)) + 
  scale_y_log10(breaks = c(10,100,seq(1000,7000,1000))) +
  xlab("Number of Messages") + 
  ylab("Number of Followers") + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) +
  theme(axis.title.y = element_text(size = 14, angle=90)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))

# Ratio of retweets to tweets for some more insights
#
# make table with counts of tweets per person
t <- as.data.frame(table(rand.df$randuser))  
# make table with counts of retweets per person
rt<- as.data.frame(table(rand.df$rt.rand))  
# combine tweet count and retweet count per person
t.rt<- merge(t,rt,by="Var1") 
# creates new col and adds ratio tweet/retweet
t.rt["ratio"] <- t.rt$Freq.y / t.rt$Freq.x
# sort it to put names in order by ratio
sort.t.rt<- t.rt[order(t.rt$ratio),] 
# exclude those with 2 tweets or less
sort.t.rt.subset<- subset(sort.t.rt,sort.t.rt$Freq.x>2) 
#
# drop unused levels leftover from subsetting
sort.t.rt.subset.drop<- droplevels(sort.t.rt.subset) 
# plot nicely ordered counts of tweets by person for 
# people> 5 tweets
ggplot(sort.t.rt.subset.drop, aes(reorder(Var1, ratio), ratio)) + 
  xlab("Author") + 
  ylab("Ratio of messages retweeted by others to original messages")+ 
  geom_point() + 
  coord_flip() + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + 
  theme(axis.title.y = element_text(size = 14, angle=90)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))

# extract tweeter-retweeted pairs
rt<- data.frame(user=rand.df$randuser, rt=rand.df$rt.rand)
# omit pairs with NA and get only unique pairs
rt.u<- na.omit(unique(rt)) #
# begin social network analysis plotting
require(igraph)
require (sna)
degree<- sna::degree
g <- graph.data.frame(rt.u, directed = F) 
# plot a basic network graph, ready for further customisation:
plot.igraph(g)
# and here is a rough approximation of the published plot
plot(g,  			#the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=3,			#the font of the name labels
     vertex.label.cex=0.5,			#specifies the size of the font of the labels. can also be made to vary
     vertex.size = 0.5
)

# find out how many communities exist in the network using the walktrap
g.wc<- walktrap.community(g, steps = 1000, modularity=TRUE)
plot(as.dendrogram(g.wc))
max(g.wc$membership)+1

# some basic and widely-used text mining techniques to identify the issues 
# that captured the attention of Twitter-using anthropologists during the meeting. 
require(tm) 
a <- Corpus(VectorSource(df$text)) # create corpus object
a <- tm_map(a, tolower) # convert all text to lower case
a <- tm_map(a, removePunctuation) 
a <- tm_map(a, removeNumbers)

# have a look at common words, in this case, those that appear at least 30 times, good to get high freq words and add to stop word list and re-make the dtm, in this case add aaa, panel, session
a <- tm_map(a, stemDocument, language = "english") # converts terms to tokens
a <- tm_map(a, PlainTextDocument)
a.tdm<- TermDocumentMatrix(a, control = list(minWordLength = 3)) # create a term document matrix, keepiing only tokens longer than three characters, since shorter tokens are very hard to interpret
inspect(a.tdm[1:10,1:10]) # have a quick look at the term document matrix
findFreqTerms(a.tdm, lowfreq=30) 

# finds associated words and strength of the common words. I repeated this
findAssocs(a.tdm, 'narendramodi', 0.3) 

#Plot Cluster
# remove sparse terms
myTdm <- removeSparseTerms(a.tdm, sparse=0.95)
m1 <- as.matrix(myTdm)
# cluster terms
distMatrix <- dist(scale(m1))
fit <- hclust(distMatrix, method="ward.D")

plot(fit)

# transpose the matrix to cluster documents (tweets)
m2 <- t(m1)
# set a fixed random seed
set.seed(122)
# k-means clustering of tweets
k <- 8
kmeansResult <- kmeans(m2, k)
# cluster centers
round(kmeansResult$centers, digits=3)
for (i in 1:k) {
  cat(paste("cluster ", i, ":  ", sep=""))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:3], "\n")
  # print the tweets of every cluster
  # print(rdmTweets[which(kmeansResult$cluster==i)])
}

# partitioning around medoids with estimation of number of clusters
library(fpc)
# partitioning around medoids with estimation of number of clusters
pamResult <- pamk(m2, metric="manhattan")
# number of clusters identified
(k <- pamResult$nc)
pamResult <- pamResult$pamobject
# print cluster medoids
for (i in 1:k) {
  cat(paste("cluster", i, ":  "))
  cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
  # print tweets in cluster i
  # print(rdmTweets[pamResult$clustering==i])
}

# plot clustering result
plot(pamResult, color=F, labels=4, lines=0, cex=.8, col.clus=1,
     col.p=pamResult$clustering)

# investigate the URLs contained in the Twitter messages
require(stringr)
require(ggplot2)
require(grid)
df$link<- sapply(df$text,function(tweet) str_extract(tweet,("http[[:print:]]+"))) # creates new field and extracts the links contained in the tweet
df$link<- sapply(df$text,function(tweet) str_extract(tweet,"http[[:print:]]{16}")) # limits to just 16 characters after http so I just get the shortened link. 
countlink<- data.frame(URL = as.character(unlist(dimnames(sort(table(df$link))))), N = sort(table(df$link))) # get frequencies of each link and put in rank order
rownames(countlink) <- NULL # remove rownames
countlinkSub<- subset(countlink, N>2) # subset of just links appearing more than twice
# plot to see distribution of links
ggplot(countlinkSub, aes(reorder(URL, N), N)) + 
  xlab("URL") + 
  ylab("Number of messages containing the URL")+ 
  geom_point() + 
  coord_flip() + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + 
  theme(axis.title.y = element_text(size = 14, angle=90)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))

# plot links
countlink<-data.frame(na.omit((df$link)))
names(countlink)="link"
qplot(countlink$link, geom="bar")+coord_flip() # but not sorted, so let's keep going...

# This is based on Jeffrey Breen's excellent tutorial at http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/

# download sentiment word list from here: http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar un-rar and put somewhere logical on your computer
hu.liu.pos = scan('../input/positive-words.txt', what = 'character',comment.char=';') #load +ve sentiment word list
hu.liu.neg = scan('D:/opinion-lexicon-English/negative-words.txt',what = 'character',comment.char= ';') #load -ve sentiment word list

pos.words = c(hu.liu.pos)
neg.words = c(hu.liu.neg)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches= !is.na(pos.matches)
    neg.matches= !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
require(plyr)
aaa.text<- df$text # get text of tweets
length(aaa.text) #check how many tweets, make sure it agrees with the original sample size
head(aaa.text, 5) #check content sample, see that it looks as expected, no weird characters, etc. 
aaa.scores<- score.sentiment(aaa.text,pos.words,neg.words,.progress='text') # get scores for the tweet text 
# create a histogram of sentiment scores
ggplot(aaa.scores, aes(x=score)) + 
  geom_histogram(binwidth=1) + 
  xlab("Sentiment score") + 
  ylab("Frequency") + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + 
  theme(axis.title.y=element_text(size = 14, angle=90, vjust = -0.25)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))
aaa.pos<- subset(aaa.scores,aaa.scores$score>= 2) # get tweets with only very +ve scores
aaa.neg<- subset(aaa.scores,aaa.scores$score<= -2) # get tweets with only very -ve scores


# Now create subset based on tweets with certain words, such as the high frequency words identified in the text mining. eg. Science
# repeat this block with different high frequency words
scien<- subset(aaa.scores, regexpr("scien", aaa.scores$text) > 0)   # extract tweets containing only 'scien'
# plot histogram for this token, 
ggplot(scien, aes(x = score)) + geom_histogram(binwidth = 1) + xlab("Sentiment score for the token 'scien'") + ylab("Frequency") + theme_bw()  + theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + theme(axis.title.y = element_text(size = 14, angle = 90, vjust = -0.25)) + theme(plot.margin = unit(c(1,1,2,2), "lines")) 


# this analysis is based on http://www.statmethods.net/advstats/cluster.html 
# scale and transpose data for cluster analysis
a.tdm.sp<- removeSparseTerms(a.tdm, sparse=0.989)  
a.tdm.sp.df<- as.data.frame(inspect(a.tdm.sp )) # convert document term matrix to data frame
nrow(a.tdm.sp.df) # check to see how many words we're left with after removing sparse terms
# this analysis is based on http://www.statmethods.net/advstats/cluster.html 
# scale and transpose data for cluster analysis
a.tdm.sp.df.sc.t <- t(scale(a.tdm.sp.df))
require(pvclust)
fit<- pvclust(a.tdm.sp.df.sc.t, method.hclust = "average", method.dist = "correlation", nboot = 10) # this method may take a few hours the bootstraping, you can reduce the nboot value for a quicker result
plot(fit)  # draw the dendrogram

# Before going right into generating the topic model and analyzing the output, we need to decide on the number of topics that the model should use
# Here's a function to loop over different topic numbers, get the log likelihood of the model for each topic number and plot it so we can pick the best one
# The best number of topics is the one with the highest log likelihood value.
require(topicmodels)
best.model<- lapply(seq(2, 50, by = 1), function(d){LDA(a.tdm.sp.t.tdif, d)}) # this will make a topic model for every number of topics between 2 and 50... it will take some time! 
best.model.logLik<- as.data.frame(as.matrix(lapply(best.model, logLik)))  # this will produce a list of logLiks for each model... 

# plot the distribution of logliklihoods by topic
best.model.logLik.df<- data.frame(topics=c(2:50), LL = as.numeric(as.matrix(best.model.logLik)))
ggplot(best.model.logLik.df, aes(x = topics, y = LL)) + 
  xlab("Number of topics") + 
  ylab("Log likelihood of the model") + 
  geom_line() + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + 
  theme(axis.title.y=element_text(size = 14, angle=90, vjust= -0.25)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))

# Before going right into generating the topic model and analyzing the output, we need to decide on the number of topics that the model should use
# Here's a function to loop over different topic numbers, get the log likelihood of the model for each topic number and plot it so we can pick the best one
# The best number of topics is the one with the highest log likelihood value.
require(topicmodels)
best.model<- lapply(seq(2, 50, by = 1), function(d){LDA(a.tdm.sp.t.tdif, d)}) # this will make a topic model for every number of topics between 2 and 50... it will take some time! 
best.model.logLik<- as.data.frame(as.matrix(lapply(best.model, logLik)))  # this will produce a list of logLiks for each model... 

# plot the distribution of logliklihoods by topic
best.model.logLik.df<- data.frame(topics=c(2:50), LL = as.numeric(as.matrix(best.model.logLik)))
ggplot(best.model.logLik.df, aes(x = topics, y = LL)) + 
  xlab("Number of topics") + 
  ylab("Log likelihood of the model") + 
  geom_line() + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + 
  theme(axis.title.y=element_text(size = 14, angle=90, vjust= -0.25)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))

# it's not easy to see exactly which topic number has the highest LL, so let's look at the data...
best.model.logLik.df.sort<- best.model.logLik.df[order(-best.model.logLik.df$LL), ] # sort to find out which number of topics has the highest loglik, in this case 23 topics. 
best.model.logLik.df.sort # have a look to see what's at the top of the list, the one with the highest score
ntop<- best.model.logLik.df.sort[1,]$topics


# generate a LDA model the optimum number of topics
# get keywords for each topic, just for a quick look
lda<- LDA(a.tdm.sp.t.tdif, ntop)
get_terms(lda, 5) 


# gets topic numbers per document 
# gives table of words per topic with words ranked in order of beta values. Useful for determining the most important words per topic
get_topics(lda, 5) 
lda_topics<-get_topics(lda, 5) 
beta<- lda@beta # create object containing parameters of the word distribution for each topic
gamma<- lda@gamma # create object containing posterior topic distribution for each document
terms <- lda@terms # create object containing terms (words) that can be used to line up with beta and gamma
colnames(beta) <- terms # puts the terms (or words) as the column names for the topic weights.
id<- t(apply(beta, 1, order)) # order the beta values
beta_ranked<- lapply(1:nrow(id),function(i)beta[i,id[i,]])



corp <- Corpus(VectorSource(df$text)) 
corp <- tm_map(corp, tolower) 
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
# corp <- tm_map(corp, stemDocument, language = "english") 
corp <- tm_map(corp, removeWords, c("the", stopwords("english"))) 
corp <- tm_map(corp, PlainTextDocument)
corp.tdm <- TermDocumentMatrix(corp, control = list(minWordLength = 3)) 
corp.dtm <- DocumentTermMatrix(corp, control = list(minWordLength = 3)) 

wordcloud(corp, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))

findFreqTerms(corp.tdm, lowfreq=30)
findAssocs(corp.tdm, 'narendramodi', 0.2)

corp.tdm.df <- data.frame(inspect(corp.tdm))

corp.tdm.df <- sort(rowSums(corp.tdm.df),decreasing=TRUE) # populate term frequency and sort in decesending order
df.freq <- data.frame(word = names(corp.tdm.df),freq=corp.tdm.df) # Table with terms and frequency

# Set minimum term frequency value. The charts will be created for terms > or = to the minimum value that we set.
freqControl <- 1000

# Frequency Plot
freqplotData <- subset(df.freq, df.freq$freq > freqControl)
freqplotData$word <- ordered(freqplotData$word,levels=levels(freqplotData$word)[unclass(freqplotData$word)])
freqplot <- ggplot(freqplotData,aes(reorder(word,freq), freq))
freqplot <- freqplot + geom_bar(stat="identity")
freqplot <- freqplot + theme(axis.text.x=element_text(angle=90,hjust=1)) + coord_flip() 
freqplot + xlim(rev(levels(freqplotData$word)))+ ggtitle("Frequency Plot")

# Wordcloud
# To change the proportion of words that are rotated by 90 degrees from the 20%, change option rot.per=0.2 appropriately
dark2 <- brewer.pal(6,"Dark2")
wordcloud(df.freq$word,df.freq$freq,min.freq=freqControl,random.order=FALSE, rot.per=0.2,colors=dark2,main="Corpus wordcloud")

# Correlation Plot
# 50 of the more frequent words has been chosen as the nodes and include links between words
# when they have at least a correlation of 0.5
# By default (without providing terms and a correlation threshold) the plot function chooses a
# random 20 terms with a threshold of 0.7
plot(corp.tdm,terms=findFreqTerms(corp.tdm,lowfreq=freqControl)[1:50],corThreshold=0.9, main="Correlation Plot")



hu.liu.pos = scan('../input/positive-words.txt', what = 'character',comment.char=';') 
hu.liu.neg = scan('../input/negative-words.txt',what = 'character',comment.char= ';') 
pos.words = c(hu.liu.pos)
neg.words = c(hu.liu.neg)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches= !is.na(pos.matches)
    neg.matches= !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

review.scores<- score.sentiment(df$text,pos.words,neg.words,.progress='text') 

ggplot(review.scores, aes(x=score)) + 
  geom_histogram(binwidth=1) + 
  xlab("Sentiment score") + 
  ylab("Frequency") + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + 
  theme(axis.title.y=element_text(size = 14, angle=90, vjust = -0.25)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))

review.pos<- subset(review.scores,review.scores$score>= 2) 
review.neg<- subset(review.scores,review.scores$score<= -2)
modi <- subset(review.scores, regexpr("narendramodi", review.scores$text) > 0) 
ggplot(modi, aes(x = score)) + geom_histogram(binwidth = 1) + xlab("Sentiment score for the token 'narendramodi'") + ylab("Frequency") + theme_bw()  + theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + theme(axis.title.y = element_text(size = 14, angle = 90, vjust = -0.25)) + theme(plot.margin = unit(c(1,1,2,2), "lines"))

#classify emotion
class_emo = classify_emotion(df$text, algorithm="bayes", prior=1.0)
#get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(df$text, algorithm="bayes")

# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=df$text, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of Feedback", 
       title = "classification by emotion",
       plot.title = element_text(size=12))

# plot distribution of emotions
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="emotion categories", y="number of Feedback", 
       title = "classification by emotion",
       plot.title = element_text(size=12))

# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = df$text[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)
