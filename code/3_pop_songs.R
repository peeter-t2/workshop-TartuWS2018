#Compiled for R tidyverse workshop in Tartu Winter School 2018, by Peeter Tinits
#dataset: https://github.com/walkerkq/musiclyrics
#based on: https://en.wikipedia.org/wiki/Billboard_Hot_100
#code built on the workshop design by Alberto Acerbi

lapply(c("tidytext","tidyverse"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

library(tidyverse)
library(tidytext) 


# Commands for text processing
# count(variable) - counts the number of items
# top_n(number, variable) - make toplists by variable
# left_join(dataframe) - add one dataframe to another
# unnest_tokens(unit, column) - make texts into tokens
# str_detect(column, "string") - partial match of a string



billboard_data <- read_csv("data/billboard_lyrics_1964-2015.csv")


billboard_data %>%
  count(Artist)

billboard_data %>%
  count(Artist)



billboard_data %>%
  count(Artist) %>%
  arrange(desc(n))


billboard_data %>%
  count(Artist) %>%
  count(n) %>%
  arrange(desc(n))



billboard_data %>%
  group_by(Year) %>%
  count(Artist) %>%
  arrange(desc(n))


billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10)


top10 <- billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10)


billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  left_join(billboard_data)




billboard_data %>%
  filter(Artist=="lady gaga") %>%
  group_by(Song) %>%
  summarise(minrank=min(Rank))

billboard_data %>%
  filter(Artist=="lady gaga") %>%
  ggplot(aes(x=Year,y=Rank,group=Song,color=Song))+
  geom_line()+
  geom_point()+
  theme_minimal()



billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  left_join(billboard_data) %>%
  group_by(Artist) %>%
  summarise(min_year=min(Year),max_year=max(Year)) %>%
  gather(type,year,c(min_year,max_year)) %>%
  ggplot(aes(y=Artist))+
  geom_line(aes(x=year),size=3)+
  theme_minimal()



## Trends within lyrics
## Uses tidytext tools https://www.tidytextmining.com/tidytext.html

billboard_tokens <- billboard_data %>%
  unnest_tokens(word, Lyrics)

# VISUALISE TRENDS OF SINGLE WORDS:
# as proportion of all the words

word_to_search <- "love"

my_title <- paste("Trend of '",word_to_search, "'", sep="")

billboard_data %>%
  count(word, Year) %>%
  filter(word==word_to_search) %>%
#  complete(Year=1965:2015, fill=list(n=0)) %>%
  rename(count=n) %>%
  left_join(count(billboard_data, Year)) %>%
  ggplot(aes(x=Year, y=count/n)) +
  geom_point(shape=19, alpha=.5, colour="red" ) +
  geom_smooth(method=loess, colour="red", fill="grey") +
  labs(y="Proportion", title=my_title)+
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5)) # +
 # ggsave(filename="plots/singe_word_example.pdf", width = 5, height = 5)





## Sentiment analysis
## uses a simple vocabulary "get_sentiments("bing")" to track sentiments
## https://www.tidytextmining.com/sentiment.html


billboard_data %>%
  filter(Artist=="lady gaga") %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song) %>%
  mutate(wordnumber = row_number()) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  mutate(line = wordnumber %/% 10) %>%
  count(line,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=line,y=sentiment))+
  geom_bar(aes(fill = sentiment>0),stat = 'identity')+
  theme_minimal()+
  facet_wrap(~Song)



billboard_data %>%
  #filter(Artist=="lady gaga") %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song) %>%
  mutate(min_rank = min(Rank)) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  group_by(Artist,Song,min_rank) %>%
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(y=sentiment, x=min_rank))+
  geom_point()+
  geom_smooth()

billboard_data %>%
  #filter(Artist=="lady gaga") %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song) %>%
  mutate(wordnumber = row_number()) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  group_by(Artist,Song,Rank) %>%
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  group_by(Artist) %>%
  summarise(positive=mean(positive),negative=mean(negative),sentiment=mean(sentiment),songs_in_top=n_distinct(Song)) %>%
  ggplot(aes(y=sentiment, x=songs_in_top))+
  geom_point()+
  geom_smooth()

  


## Keyword analysis
## tf_idf https://en.wikipedia.org/wiki/Tf%E2%80%93idf
## "term frequency–inverse document frequency"
## reflects how important a word is to a document in a collection or corpus.
## https://www.tidytextmining.com/tfidf.html


tf_idf <- billboard_data %>%
  filter(Artist=="lady gaga") %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(Artist,Song) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, Song, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()



tf_idf %>% 
  group_by(Song) %>%
  filter(n>10) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = Song)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~Song,scale="free_y")



tf_idf_artist <- billboard_data %>%
  #filter(Artist=="lady gaga") %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(Artist) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, Artist, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()

tf_idf_artist %>%
  rename(count=n) %>%
  inner_join(top10,by="Artist") %>%
  group_by(Artist) %>%
  top_n(10,tf_idf) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = Artist)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~Artist,scale="free_y")




tf_idf_decade <- billboard_data %>%
  mutate(Decade=floor(Year/10)*10) %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(Decade) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, Decade, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()

tf_idf_decade %>%
  rename(count=n) %>%
  group_by(Decade) %>%
  top_n(10,tf_idf) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = factor(Decade))) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~Decade,scale="free_y")


tf_idf_decade %>%
  rename(count=n) %>%
  group_by(Decade) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(LIWC) %>% 
  group_by(Decade,sentiment) %>%
  top_n(10,tf_idf) %>%
  arrange(desc(tf_idf)) %>%
  ggplot(aes(x=reorder(word,count), y=tf_idf, fill = factor(sentiment))) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~Decade+sentiment,scale="free_y") 

tf_idf_decade %>%
  rename(count=n) %>%
  group_by(Decade) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(LIWC) %>% 
  group_by(Decade,sentiment) %>%
  top_n(10,tf_idf) %>%
  mutate(linenumber=row_number()) %>%
  arrange(desc(tf_idf)) %>%
  ggplot(aes(Decade, (-1*linenumber))) + geom_point(color="white") + 
  geom_text(aes(label=word, color=factor(Decade)),  fontface='bold', size=4) + 
  theme(legend.position="none", plot.title = element_text(size=18), 
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.text.x=element_text(size=16)) + 
  labs(title="Most Characteristic Lyrics by Decade \n Billboard Year-End Top 100, 1965-2015") + 
  xlab("") + ylab("Ranking")+
  facet_wrap(~sentiment) +
  scale_y_continuous(limits=c(-25,-1), breaks=c(-20, -10, -.5), labels=c("#20", "#10", "#1"))+
  scale_x_continuous(limits=c(1955,2015))+
  theme_classic()# +
  #scale_color_manual(values = mycolors)



#sentiments through different datasets
#inner_join(get_sentiments("bing"), by = "word")
#inner_join(LIWC)

# SENTIMENT ANALYSIS:
LIWC_negemo <- read_csv("negemo.csv")
LIWC_posemo <- read_csv("posemo.csv")
LIWC <- data_frame( word=c(LIWC_negemo$Negative,LIWC_posemo$Positive ),
                    sentiment=c(rep("negative",2108), rep("positive",1903)))

# GENERAL TRENDS:
billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  inner_join(LIWC) %>%
  count(Year, sentiment) %>%
  spread(sentiment, n, fill = 0, convert=T) %>%
  left_join(count(unnest_tokens(billboard_data,word, Lyrics), Year)) %>%
  ggplot(aes(Year, (positive)/n)) + # change here
  geom_point(shape=19, alpha=.5, colour="black" ) +
  geom_smooth(method=lm, colour="black", fill="grey") +
  labs(y="Proportion", title="Positive emotions")  +
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5)) +
  ggsave(filename="plots/trend_example1.pdf", width = 5, height = 5)


# GENERAL TRENDS:
billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  inner_join(LIWC) %>%
  count(Year, sentiment) %>%
  spread(sentiment, n, fill = 0, convert=T) %>%
  left_join(count(unnest_tokens(billboard_data,word, Lyrics), Year)) %>%
  ggplot(aes(Year, (negative)/n)) + # change here
  geom_point(shape=19, alpha=.5, colour="black" ) +
  geom_smooth(method=lm, colour="black", fill="grey") +
  labs(y="Proportion", title="Negative emotions")  +
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5)) +
  ggsave(filename="plots/trend_example2.pdf", width = 5, height = 5)



