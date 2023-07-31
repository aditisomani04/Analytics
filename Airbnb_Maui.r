
## MKTG - 5910 : Marketing Analytics Airbnb Assignment - Hawaii(Maui)

##Airbnb Text Mining Code for Hawaii - Maui location##

#setwd("Copy and Paste Data File Address") 
setwd("/Users/aditisomani/Desktop")

#install packages. SKIP this if packages are already installed. 
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("NLP")
install.packages("slam")
install.packages("topicmodels")
install.packages("wordcloud")
install.packages("ggmap")
install.packages("SnowballC")
install.packages("tm")
install.packages("SentimentAnalysis")

#load packages. If the packages don't load, run the above set of lines first. 
library("dplyr")
library("tidyr")
library("ggplot2")
library("NLP")
library("slam")
library("topicmodels")
library("wordcloud")
library("ggmap")
library("SnowballC")
library("tm")
library("SentimentAnalysis")

#combine reviews data and listings data
reviews <- read.csv("reviews.csv")
listings <- read.csv("listings.csv")

#checking missing values
any(is.na(reviews))
any(is.na(listings))

#reviews <- na.omit(reviews)
#listings <- na.omit(listings)

#first change the name of the common variable (so that they are the same)
#listings <- mutate(listings, listing_id=id)

#combine review and listings data using the merge function 
reviews_by_listing=merge(listings, reviews, by="listing_id")

#filter data to simplify (based on a nominal variable)
#reviews.superhost=filter(reviews_by_listing, host_is_superhost == "t")
#reviews.maui=filter(reviews_by_listing, neighbourhood_group_cleansed == "Maui")

#filter data to simplify (based on a continuous variable); make sure to remove $ sign before importing to R
listings$price=as.numeric(listings$price)
summary(listings$price)
reviews_by_listing$price=as.numeric(reviews_by_listing$price)
reviews.cheap=filter(reviews_by_listing, price < 387)

#using multiple filter criteria (e.g., based on neighbourhood, superhost status (not SH), and price)
#reviews.maui.notsuper.cheap=filter(reviews_by_listing, neighbourhood_group_cleansed == "Maui" & host_is_superhost == "f" & price < 387 )

reviews.cheap=filter(reviews_by_listing, price < 387 )

#change the filtered data name
reviews.filtered <- reviews.cheap

#save the filtered file just in case
write.csv(reviews.filtered,file="reviews_filtered.csv")

#Creating DTM and getting the Word Count
#changing the column names so that the alogorithm detects them 
reviews.filtered <- reviews.filtered %>% mutate(doc_id=reviewer_id,text=comments)
#creating a database for texts
review.corpus <- VCorpus(DataframeSource(reviews.filtered))

#cleaning the text data
#PC Users, use the following line 
review.corpus.clean <- tm_map(review.corpus, content_transformer(tolower)) #Interface to apply transformation functions to corpora.
#PC users, if the above line doesn't work, try the code below
review.corpus.clean <- tm_map(review.corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = 'byte')))
#Mac Users, use the following two lines
review.corpus.clean <- tm_map(review.corpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))
review.corpus.clean <- tm_map(review.corpus.clean, content_transformer(tolower)) #Interface to apply transformation functions to corpora.
#All users, run these lines
review.corpus.clean <- tm_map(review.corpus.clean, removeWords, stopwords("english"))
review.corpus.clean <- tm_map(review.corpus.clean, removePunctuation)
review.corpus.clean <- tm_map(review.corpus.clean, removeNumbers)
review.corpus.clean <- tm_map(review.corpus.clean, stemDocument, language="english") #perform stemming which truncates words
review.corpus.clean <- tm_map(review.corpus.clean,stripWhitespace)

write.csv(reviews.filtered,file="review.corpus.clean.csv")

dtm <- DocumentTermMatrix(review.corpus.clean)

#chekcing the first ten documents using the words: clean and room
inspect(dtm[1:10,c("host","room")])

## Check frequency and make frequency plot
freq <- colSums(as.matrix(dtm)) 

###Very hard to see, so let's make a visualization###
#Step 1: make a separate term.count datasheet
term.count <- as.data.frame(as.table(dtm)) %>%
  group_by(Terms) %>%
  summarize(n=sum(Freq))

#Step 2:Keep High Frequency words only, e.g, 0.25%
term.count %>% 
  filter(cume_dist(n) > 0.9975) %>% #cume_dist is the cumulative distribution function which gives the proportion of values less than or equal to the current rank
  ggplot(aes(x=reorder(Terms,n),y=n)) + geom_bar(stat='identity') + 
  coord_flip() + xlab('Counts') + ylab('')

#Another way to find the frequent terms 
findFreqTerms(dtm, lowfreq=150)

# find terms correlated with "room" 
room <- data.frame(findAssocs(dtm, "room", 0.1))
room %>%
  add_rownames() %>%
  ggplot(aes(x=reorder(rowname,room),y=room)) + geom_point(size=4) + 
  coord_flip() + ylab('Correlation') + xlab('Term') + 
  ggtitle('Terms correlated with Room') + theme(text=element_text(size=20))

# find terms correlated with "locat" <- stemmed word for location
locat <- data.frame(findAssocs(dtm, "locat", 0.05))
locat %>%
  add_rownames() %>%
  ggplot(aes(x=reorder(rowname,locat),y=locat)) + geom_point(size=4) + 
  coord_flip() + ylab('Correlation') + xlab('Term') + 
  ggtitle('Terms correlated with location') + theme(text=element_text(size=20))

## Make a wordcloud
#install.packages("wordcloud")
#library("wordcloud")

popular.terms <- filter(term.count,n > 100)
wordcloud(popular.terms$Terms,popular.terms$n,colors=brewer.pal(8,"Dark2"))

###########################################################################################
# SENTIMENT ANALYSIS
# R package: Download the packages to your computer from the links
###########################################################################################
#install.packages("SentimentAnalysis")
#library("SentimentAnalysis")

#PC Users run the below 
sentiment <- analyzeSentiment(as.character(reviews.filtered$text))

#if the above line does not work and you have a MAC, try this
recode <-function(x) {iconv(x, to='UTF-8-MAC', sub='byte')}
sentiment <- analyzeSentiment(recode(reviews.filtered$text))

#all users, run the line below
sent_df = data.frame(polarity=sentiment$SentimentQDAP, business = reviews.filtered, stringsAsFactors=FALSE)

# Plot results and check the correlation betweeen polarity and review stars #

#First, check the summary statistics of the polarity score
summary(sent_df$polarity)

#Second, check for the missing value. Why is there a missing value? 
sent_df %>% filter(is.na(polarity)==TRUE)%>%select(business.text)

#Now, correct for that missing value, NA
sent_df$polarity[is.na(sent_df$polarity)]=0
sent_df$business.review_scores_rating<-as.numeric(sent_df$business.review_scores_rating)

#check the correlation between star ratings and polarity score
cor(sent_df$polarity,sent_df$business.review_scores_rating)

#visualize the correlation 
sent_df %>%
  group_by(business.review_scores_rating) %>%
  summarize(mean.polarity=mean(polarity,na.rm=TRUE)) %>%
  ggplot(aes(x=business.review_scores_rating,y=mean.polarity)) +  geom_bar(stat='identity',fill="blue") +  
  ylab('Mean Polarity') + xlab('Review Score')  + theme(text=element_text(size=20))

#visulize using a line chart
sent_df %>% ggplot(aes(x=business.review_scores_rating, y=polarity)) +
  geom_point() +
  geom_smooth(method=lm) + 
  ggtitle("Polarity Score by Review Rating")+xlab("Review Rating")+ ylab("Polarity Score")


###########################################################################################
# TOPIC MODELING
# R package: "topicmodels"
###########################################################################################

## set.up.dtm.for.lda.1
#library("topicmodels")
#library("slam")

dtm.lda <- removeSparseTerms(dtm, 0.98)
review.listing_id <- reviews.filtered$review.listing_id[row_sums(dtm.lda) > 0]
dtm.lda <- dtm.lda[row_sums(dtm.lda) > 0,]

## run LDA algorithm - WARNING: takes a while to run!k = number of topics 
lda.airbnb <- LDA(dtm.lda,k=20,method="Gibbs",
                control = list(seed = 2011, burnin = 1000,
                               thin = 100, iter = 5000))
#to be safe, save the file 
save(lda.airbnb,file='lda_results_bb.rda')

## loading results for later use
load('lda_results_bb.rda')

#get the posterior probability of the topics for each document and of the terms for each topic
post.lda.airbnb <- posterior(lda.airbnb) 

##  sum.lda to get a matrix with topic by terms "cleaning data process"
sum.terms <- as.data.frame(post.lda.airbnb$terms) %>% #matrix topic * terms
  mutate(topic=1:20) %>% #add a column
  gather(term,p,-topic) %>% #gather makes wide table longer, key=term, value=p, columns=-topic (exclude the topic column)
  group_by(topic) %>%
  mutate(rnk=dense_rank(-p)) %>% #add a column
  filter(rnk <= 10) %>%
  arrange(topic,desc(p)) 

## see the words in each topic
# words in topic 1
sum.terms %>%
  filter(topic==1) %>%
  ggplot(aes(x=reorder(term,p),y=p)) + geom_bar(stat='identity') + coord_flip() + 
  xlab('Term')+ylab('Probability')+ggtitle('Topic 1') + theme(text=element_text(size=20))

#words in topic 2
sum.terms %>%
  filter(topic==2) %>%
  ggplot(aes(x=reorder(term,p),y=p)) + geom_bar(stat='identity') + coord_flip() + 
  xlab('Term')+ylab('Probability')+ggtitle('Topic 2') + theme(text=element_text(size=20))

#words in topic 3
sum.terms %>%
  filter(topic==3) %>%
  ggplot(aes(x=reorder(term,p),y=p)) + geom_bar(stat='identity') + coord_flip() + 
  xlab('Term')+ylab('Probability')+ggtitle('Topic 3') + theme(text=element_text(size=20))


#words in topic 4
sum.terms %>%
  filter(topic==4) %>%
  ggplot(aes(x=reorder(term,p),y=p)) + geom_bar(stat='identity') + coord_flip() + 
  xlab('Term')+ylab('Probability')+ggtitle('Topic 4') + theme(text=element_text(size=15))

#words in topic 5
sum.terms %>%
  filter(topic==5) %>%
  ggplot(aes(x=reorder(term,p),y=p)) + geom_bar(stat='identity') + coord_flip() + 
  xlab('Term')+ylab('Probability')+ggtitle('Topic 5') + theme(text=element_text(size=15))

#words in topic 6
sum.terms %>%
  filter(topic==6) %>%
  ggplot(aes(x=reorder(term,p),y=p)) + geom_bar(stat='identity') + coord_flip() + 
  xlab('Term')+ylab('Probability')+ggtitle('Topic 6') + theme(text=element_text(size=15))

#words in topic 7
sum.terms %>%
  filter(topic==7) %>%
  ggplot(aes(x=reorder(term,p),y=p)) + geom_bar(stat='identity') + coord_flip() + 
  xlab('Term')+ylab('Probability')+ggtitle('Topic 7') + theme(text=element_text(size=15))

#words in topic 8
sum.terms %>%
  filter(topic==8) %>%
  ggplot(aes(x=reorder(term,p),y=p)) + geom_bar(stat='identity') + coord_flip() + 
  xlab('Term')+ylab('Probability')+ggtitle('Topic 8')


#words in topic 9
sum.terms %>%
  filter(topic==9) %>%
  ggplot(aes(x=reorder(term,p),y=p)) + geom_bar(stat='identity') + coord_flip() + 
  xlab('Term')+ylab('Probability')+ggtitle('Topic 9')

#words in topic 10
sum.terms %>%
  filter(topic==10) %>%
  ggplot(aes(x=reorder(term,p),y=p)) + geom_bar(stat='identity') + coord_flip() + 
  xlab('Term')+ylab('Probability')+ggtitle('Topic 10')
