#section 1 (search data from twitter and write csv files)

library(stringr)
library(plyr)
library(twitteR)
library(ROAuth)
library(devtools)
library(dplyr)
library(streamR)
library(syuzhet)
library(rvest)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(ggplot2)
library(caret)
library(lattice)
library(quanteda)
library(tidyr)
library(topicmodels)
library(tidytext)
library(ldatuning)
library(stringr)

consumer_key <- "t6DbEmTUUtzk42aPQnKNhqgLF"
consumer_secret <- "37zd5ydEvHWJUaswtwrmS6sANKct4eftYF6uNBjWqCko6KbzhS"
access_token <- "910526070625095681-KDWobLMWjdUmPK141gKZq0RupjmfwqA"
access_secret <- "IqdF141IfbAlql9c9Avu8wifgygDbYLgRssg1itxxUB4m"

TwitterAuth<-setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#Get data from Twitter
search.twitter <- searchTwitter("United airlines exclude:retweets", n = 50000,
                                lang = "en", since = '2017-10-30',
                                until = "2017-11-02", retryOnRateLimit = 120)
search.twitter.df<-twListToDF(search.twitter)
search.twitter.df

write.csv(search.twitter.df,"netflix.csv")

netflix$TextLength<-nchar(as.character(netflix$text))

netflix<- netflix[,-c(6,7,8,9,10)]

write.csv(netflix,"delta1.csv")

netflix <- read.csv("united1.csv")

netflix<-netflix[1:8000,]


tweets.df<-as.data.frame(netflix$text)
colnames(tweets.df)[1]<-"text"

#Section 2 - (text processing)_________________________________________________

tweets.df<-lapply(tweets.df, function(x)gsub("@\\w+", "", x))
tweets.df<-lapply(tweets.df, function(x)gsub("#\\w+", '', x ))
tweets.df<-lapply(tweets.df, function(x)gsub("RT\\w+", "", x ))
tweets.df<-lapply(tweets.df, function(x)gsub("http.*", "", x ))
tweets.df<-lapply(tweets.df, function(x)gsub("RT", "", x))
tweets.df<-lapply(tweets.df, function(x)gsub(":", "", x))
tweets.df<- as.data.frame(tweets.df)


corpus<-Corpus(VectorSource(tweets.df$text))

#for windows users
corpus<-tm_map(corpus, function(x)iconv(x, "latin1", "ASCII", ""))
#iconv(utf8towcs(x), sub='byte')
#iconv( from = "UTF-8", to = "ASCII", sub = "")


#transforming text into lower case
corpus<- tm_map(corpus, content_transformer(tolower))
 
#removing the words that are not very uselful for analysis
corpus<-tm_map(corpus, removeWords, c("delta","southwest","united","airline","get","now","can","one","airlines","fly","flight","flights","airport","anthem","singing","plane","just"))

#removing the punctuations
corpus <- tm_map(corpus, removePunctuation)

#removing the stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

#removing the Whitespaces
corpus <- tm_map(corpus, stripWhitespace)

#removing the Numbers
corpus<- tm_map(corpus, removeNumbers)
corpus<- tm_map(corpus, stemDocument)

df <- data.frame(text = get("content", corpus))

df <- sapply(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
#df <- apply(corpus,2,function(x) gsub("â???T",' ',x))
df <-as.data.frame(df)
View(df)
#netflix<-netflix[1:10000,]
#Section 3 - 1) Topic number_________________________________________

dtm <- DocumentTermMatrix(corpus, control=list(minDocFreq=2, minWordLength=2))

#Remove empty cells and create a new corpus that aligns with the processed dtm
rowTotals <- apply(dtm , 1, sum)
empty.rows<-dtm[rowTotals == 0,]$dimnames[1][[1]]
empty.rows<-as.numeric(empty.rows)
corpus <- corpus[-empty.rows]

#Create a dataframe of the new corpus
corpus.df<-as.data.frame(corpus$content)

#Create the dtm again with the new corpus
dtm <- DocumentTermMatrix(corpus, control=list(minDocFreq=2, minWordLength=2))

#Making sure that the original data set i.e., model.raw aligns with the new corpus. To do that we have to remove the same row numbers in empty.rows in model.raw
x<-length(as.numeric(empty.rows))# calculate the number of empty.rows


#Write a loop that goes through the row numbers of model.raw and delete those rows that match with delete.rows
empty.rows[1]
for (i in 1:x){
  netflix<-netflix[-empty.rows[i],]
  i<-i+1
}

#Random check to see consistency between the new and old datasets
corpus.df[743,]
netflix[743,]$text


dtm<-DocumentTermMatrix(corpus, control = list(weighting=weightTf))
m <- as.matrix(dtm)
dtm


rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]

lda<-LDA(dtm.new, k=7, control = list(seed=2343))

topics<-tidy(lda, matrix="beta")
topics

#Showing the top terms and grouping them by topics created - Using dplyr's top_n limiting to 10
top_terms<-topics %>%
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

#Visualization of the top terms.
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  theme_bw()+
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#Convert top_terms$topic to a factor variable - for visualization 
top_terms$topic<-as.factor(top_terms$topic)

#Creating a per-document-per-topic probability list
documents<-tidy(lda, matrix="gamma")
documents


#Finding the highest probablity gamma score to assign topic number for each document
i<-1
max_pos<-vector()
for(i in documents$document){
  
  x<-filter(documents, document==i)
  max<-max(x$gamma) 
  max_pos[length(max_pos)+1]<-max
  #mutate(documents, max)
  
}

df <- data.frame(text = get("content", corpus))

df <- sapply(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
#df <- apply(corpus,2,function(x) gsub("â???T",' ',x))
df <-as.data.frame(df)
View(df)

#Merging the per document topic probabilities to model.raw
documents$max<-max_pos
documents$diff<-documents$gamma-documents$max
documents$df<-df$text

documents<-filter(documents, diff==0)
documents$document<-as.numeric(documents$document)
documents<-arrange(documents,document)

model.raw<-rbind.fill(netflix,documents)




ggplot(model.raw, aes(x=TextLength, y=favoriteCount))+geom_point(aes(color=topic))+theme_bw()

#Export results 

#Finding out how many topics to create
result<-FindTopicsNumber(dtm,
                         topics=seq(from = 2, to =15, by=1),
                         metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                         method = "Gibbs",
                         control = list(seed = 77),
                         mc.cores = 3L,
                         verbose = TRUE)

FindTopicsNumber_plot(result)

#Section 3 - 2) sentiment label, score and hour of the day___________________

netflix <- read.csv("southwest1.csv")

colnames(df)[1]<-"text"

tweets.df <- sapply(df$text, function(x) iconv(x, to='UTF-8', 'ASCII', sub=''))

tweets.df<-gsub("@\\w+", "", tweets.df)
tweets.df<-gsub("#\\w+", '', tweets.df)
tweets.df<-gsub("RT\\w+", "", tweets.df)
tweets.df<-gsub("http.*", "", tweets.df)
tweets.df<-gsub("RT", "", tweets.df)
pos<-readLines("positive_words.txt")
neg<-readLines("negative_words.txt")

score.sentiment<-function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores<-laply(sentences,
                function(sentence, pos.words, neg.words)
                {
                  # remove punctuation
                  sentence<-gsub("[[:punct:]]", "", sentence)
                  # remove control characters
                  sentence<-gsub("[[:cntrl:]]", "", sentence)
                  # remove digits?
                  sentence<-gsub('\\d+', '', sentence)
                  sentence <- iconv(sentence, 'UTF-8', 'ASCII')
                  #convert to lower
                  sentence<-tolower(sentence)
                  
                  
                  # split sentence into words with str_split (stringr package)
                  word.list<- str_split(sentence, "\\s+")
                  words<- unlist(word.list)
                  
                  # compare words to the dictionaries of positive & negative terms
                  pos.matches<-match(words, pos)
                  neg.matches<- match(words, neg)
                  
                  # get the position of the matched term or NA
                  # we just want a TRUE/FALSE
                  pos.matches<- !is.na(pos.matches)
                  neg.matches<- !is.na(neg.matches)
                  
                  # final score
                  score<- sum(pos.matches) - sum(neg.matches)
                  return(score)
                }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df<- data.frame(text=sentences, score=scores)
  return(scores.df)
}
#df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)

#df<- as.character(df)

scores_reviews<-score.sentiment(tweets.df, pos, neg, .progress='text')
View(scores_reviews)



#Convert sentiment scores from numeric to character to enable the gsub function 
scores_reviews$score_chr<-as.character(scores_reviews$score)

#After looking at the summary(scores_facebook$score) decide on a threshold for the sentiment labels
scores_reviews$period<-gsub("^0$", "Neutral", edo$period)
scores_reviews$period<-gsub("^1$|^2$|^3$|^4$|^5$", "Positive", edo$period)
scores_reviews$score_chr<-gsub("^-1$|^-2$|^-3$|^-4$|^-5$|^-6$|^-7$|^-8$", "Negative", edo$period)

#Convert score_chr to factor for visualizations
scores_reviews$score_chr<-as.factor(scores_reviews$score_chr)


#Create a bar chart that shows distributions of sentiment labels
ggplot(scores_reviews, aes(x=score_chr))+geom_bar()+theme_bw()

ggplot(scores_reviews, aes(x=score_chr))+geom_bar()+theme_bw()


summary(scores_reviews)
View(summary(scores_reviews))
View(scores_reviews)


#section 3- hour of the day____________________________________________________

netflix.date<-netflix[,c(2,5)]




new <- do.call( rbind , strsplit( as.character(netflix.date$date ) , " " ) )
#     [,1]         [,2]   
#[1,] "13:11:2013" "15:39"
#[2,] "13:11:2013" "16:15"
#[3,] "13:11:2013" "17:52"


#  Cbind them to the original data liek so...
edo <-cbind( netflix.date , Time = new[,2] , Date = new[,1] )
#             Start  Date       Time
#1 13:11:2013 15:39 15:39 13:11:2013
#2 13:11:2013 16:15 16:15 13:11:2013
#3 13:11:2013 17:52 17:52 13:11:2013

edo <- edo[,c(2,3,4)]
View(edo)

datetime <- edo[,c(2)]
#datetime <- as.data.frame(datetime)

hour <- as.integer(substr(datetime, 0, 2))
#hour <- as.data.frame(hour)

conversion <- data.frame(hour=hour,
                         period=cut(hour, c(-Inf, 10, 15, 19, Inf),
                                    labels=c("morning", "afternoon", "evening","night")))
edo<- cbind(edo,conversion)


edo<-as.data.frame(read.csv('southwest1.csv'))
edo<-sapply(strsplit(as.character(edo$period), " "), "[", 1)
View(edo)
search.twitter1.aldi.df$time<-sapply(strsplit(as.character(search.twitter1.aldi.df$created), " "), "[", 2)
search.twitter1.aldi.df$hours<-sapply(strsplit(as.character(search.twitter1.aldi.df$time), ":"), "[", 1)
View(search.twitter1.aldi.df)
search.twitter1.aldi.df$daylevels<-as.character(search.twitter1.aldi.df$hours)
edo<-gsub("^03$|^04$|^05$|^06$|^07$|^08$|^09$|^10$", "Morning", edo$period)
edo<-gsub("^11$|^12$|^13$|^14$", "Afternoon",edo$period)
edo<-gsub("^15$|^16$|^17$|^18$|^19$|^20$", "Evening", edo$period)
edo<-gsub("^21$|^22$|^23$|^00$|^01$|^02$","Night",edo$period)
edo<-as.factor(edo$period)

edo<-edo[rowSums(is.na(edo)) ==0,]

#Create a bar chart that shows distributions of sentiment labels
ggplot(edo, aes(x=period))+geom_bar()+theme_bw()
View(edo)

scores_reviews<- cbind(edo,scores_reviews)
scores_reviews<- scores_reviews[,-c(1)]
scores_reviews<- scores_reviews[,c(5,6,7,1,2,3,4)]

#section 4 - Wordcloud__________________________________________________________

dtm1<-TermDocumentMatrix(corpus, control = list(weighting=weightTf))
m1 <- as.matrix(dtm1)

v <- sort(rowSums(m1),decreasing=TRUE)
df <- data.frame(word = names(v),freq=v)

head(df,10) 

wordcloud(words = df$word, freq = df$freq, min.freq = 20, max.words=1000, scale=c(4,0.1), random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))




