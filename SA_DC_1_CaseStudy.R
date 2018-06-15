#Start of the Script

# Load dplyr and tidytext
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)

load(file="geocoded_tweets.rda")

head(geocoded_tweets)

str(geocoded_tweets)

#Comments about data: 
#            state - a State in United States
#           word - wprd used in tweet
#           freq - the average frequency of that word in that state

#Start of the Sentiment Analysis

# Choose the bing lexicon
get_sentiments("bing")

# Choose the nrc lexicon
get_sentiments("nrc") %>%
  count(sentiment) # Count words by sentiment

# While the "bing" lexicon classifies words into 2 sentiments, 
# positive or negative, there are 10 sentiments conveyed in the "nrc" lexicon.

# Access bing lexicon: bing
bing <- get_sentiments("bing")

# Use data frame with text data
tweets_bing<- geocoded_tweets %>%
            # With inner join, implement sentiment analysis using `bing`
              inner_join(bing)

tweets_bing
#We can see the average frequency and the sentiment associated with each word, 
#that exists in both data frames.

# Access nrc lexicon: nrc
nrc <- get_sentiments("nrc")

# Use data frame with text data
tweets_nrc <- geocoded_tweets %>%
                  inner_join(nrc)

tweets_nrc


#Question: What are the most common sadness words?

sad_words<- tweets_nrc %>%
            # Filter to only choose the words associated with sadness
            filter(sentiment=="sadness") %>%
            # Group by word
            group_by(word) %>%
            # Use the summarize verb to find the mean frequency
            summarize(freq = mean(freq)) %>%
            # Arrange to sort in order of descending frequency
            arrange(desc(freq))

sad_words

#Plotting the result for first 20 sad words
sad_words %>%
  top_n(20) %>%
  mutate(word = reorder(word,freq)) %>%
  ggplot(aes(x=word,y=freq))+
  geom_col()+ #geom_bar(stat="identity")
  coord_flip()

#Question : What are the most common joy words?

joy_words<- tweets_nrc %>%
  # Filter to only choose the words associated with sadness
  filter(sentiment=="joy") %>%
  # Group by word
  group_by(word) %>%
  # Use the summarize verb to find the mean frequency
  summarize(freq = mean(freq)) %>%
  # Arrange to sort in order of descending frequency
  arrange(desc(freq))

joy_words

#Plotting the result for first 20 Joy words
joy_words %>%
  top_n(20) %>%
  mutate(word = reorder(word,freq)) %>%
  ggplot(aes(x=word,y=freq))+
  geom_col()+ #geom_bar(stat="identity")
  coord_flip()

#looking at differences by state
state_data<-tweets_bing %>%
            group_by(state,sentiment) %>%
            summarize(freq=mean(freq)) %>%
            spread(sentiment,freq) %>%
            ungroup()

state_negative<-state_data %>%
          mutate(state=reorder(state,negative))

state_negative

ggplot(data=state_negative,aes(x=state,y=negative))+
  geom_col()+
  coord_flip()

state_positive<-state_data %>%
  mutate(state=reorder(state,positive))

state_positive

ggplot(data=state_positive,aes(x=state,y=positive))+
  geom_col()+
  coord_flip()

#**Comments : Kentucky is the State which uses lots of postive words, 
# while Louisiana with most negative words.**
