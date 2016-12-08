# download.file("http://cran.cnr.berkeley.edu/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz", "Rstem_0.4-1.tar.gz")
# install.packages("Rstem_0.4-1.tar.gz", repos=NULL, type="source")
# download.file("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", "sentiment.tar.gz")
# install.packages("sentiment.tar.gz", repos=NULL, type="source")
library(sentiment)
library(wordcloud)
library(ggplot2)

df<- read.csv("../input/demonetization-tweets.csv", stringsAsFactors = FALSE)

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

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="emotion categories", y="number of Feedback", 
       title = "classification by polarity",
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