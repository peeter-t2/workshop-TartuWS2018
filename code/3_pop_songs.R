#Compiled for R tidyverse workshop in Tartu Winter School 2018, by Peeter Tinits
#dataset: https://github.com/walkerkq/musiclyrics
#based on: https://en.wikipedia.org/wiki/Billboard_Hot_100
#code built on the workshop design by Alberto Acerbi


#This command installs the libraries needed to run the code, if you don't have them.
lapply(c("tidytext","tidyverse","gridExtra"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

#Libraries need to be opened each time you open R. These commands open the libraries/packages in the current environment.
library(tidyverse)
library(tidytext)
library(gridExtra)


# Commands for text processing
# count(variable) - counts the number of items
# top_n(number, variable) - make toplists by variable
# left_join(dataframe) - add one dataframe to another
# unnest_tokens(unit, column) - make texts into tokens
# str_detect(column, "string") - partial match of a string



billboard_data <- read_csv("data/billboard_lyrics_1964-2015.csv")


#Let's take a random sample of 10 songs in the last year
#For fun, how many can songs or artists you recognize?
now <- billboard_data %>%
  filter(Year==max(Year)) %>%
  sample_n(10)


#Let's take a random sample of 10 songs from 1997
year1997 <- billboard_data %>%
  filter(Year==1997) %>%
  sample_n(10)


#Number of instances in top 100 per artist
billboard_data %>%
  count(Artist)


#all by some artist
#Data for beyonce
beyonce <- billboard_data %>%
  filter(Artist=="beyonce")

#Beyonce's songs by rank and year
beyonce %>%
  ggplot(aes(x=Year,y=-Rank,color=Song))+
  geom_point()+
  geom_line()

#Taylor Swift songs by Rank and year (same as with beyonce, except that we don't store a variable)
billboard_data %>%
  filter(Artist=="taylor swift") %>%
  ggplot(aes(x=Year,y=-Rank,color=Song))+
  geom_point()+
  geom_line()


#The number of times the best group was in top 100 in each year
billboard_data %>%
  group_by(Year) %>%
  count(Artist) %>%
  top_n(1) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=Year,y=n))+
  geom_point()+
  geom_line()

######################################
#### Ideas proposed in the seminar ###
######################################
#Some questions proposed in the seminar to try to look at
#Some are solved in the code, you can try to follow up on others
#all by some artist
#The worst year in music (most repetitions)
#!song lenght in type and token...
#!are songs becoming more repetitive, richness of vocabulary..
##!title lenghts
#songs with same/similar title
#development of keywords per year.
#particular words and development in time... (love)
#most featured artist (featuring someone..)
#bad,,, (uptake during 1990s)
#genre-specific words..., to look for prevalene of some genres...
#look for cursewords...
#nanana lalala hihihi, trends of ho he ye S
#he vs she..
# i vs you over time
#song length, ranks
#keywords of top 10 ranking songs...
##########################################
## Try if you can work some of them out ##
##########################################



#Top 10 of all time, per instances in top 100
billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10)


#Top 20 of all time, per instances in top 100
billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  head(20)

#How many artists have how many songs
billboard_data %>%
  count(Artist) %>%
  count(n) %>%
  arrange(desc(n))


#The worst year in music (most repetitions)
billboard_data %>%
  group_by(Year) %>%
  count(Artist) %>%
  arrange(desc(n))



#Top 10 all time stored as variable
top10 <- billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10)


#Add the count of how many times total to the main dataframe
billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  left_join(billboard_data)



#The best rank for each song of lady gaga
billboard_data %>%
  filter(Artist=="lady gaga") %>%
  group_by(Song) %>%
  summarise(minrank=min(Rank))

#All song ranks of lady gaga plotted
billboard_data %>%
  filter(Artist=="lady gaga") %>%
  ggplot(aes(x=Year,y=Rank,group=Song,color=Song))+
  geom_line()+
  geom_point()+
  theme_minimal()


##The duration of productive lifespan, 
# Taylor swift is very quick, michael jackson was on top for very long
billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  left_join(billboard_data) %>%
  group_by(Artist) %>%
  summarise(min_year=min(Year),max_year=max(Year)) %>%
  gather(type,year,c(min_year,max_year)) %>% # a new function that changes the dataframe a bit
  ggplot(aes(y=Artist))+
  geom_line(aes(x=year),size=3)+
  theme_minimal()



## Trends within lyrics
## Uses tidytext tools https://www.tidytextmining.com/tidytext.html

#Words in song lyrics
billboard_tokens <- billboard_data %>%
  unnest_tokens(word, Lyrics) #new function of unnest_tokens, takes the words from the text, and sets them as separate observations

#Are songs getting shorter or longer?  (just plotting the averages here per year)
billboard_tokens %>%
  group_by(Song,Artist,Year) %>%
  summarise(types=n_distinct(word),tokens=length(word)) %>%
  ggplot(aes(x=Year,y=tokens))+
  geom_point()+
  geom_smooth()

#Is the vocabulary increasing in time?
billboard_tokens %>%
  group_by(Song,Artist,Year) %>%
  summarise(types=n_distinct(word),tokens=length(word)) %>%
  ggplot(aes(x=Year,y=types))+
  geom_point()+
  geom_smooth()

#Is the vocabulary becoming more repetitive/diverse
#see "type-token ratio" online for more information
billboard_tokens %>%
  group_by(Song,Artist,Year) %>%
  summarise(types=n_distinct(word),tokens=length(word)) %>%
  ggplot(aes(x=Year,y=types/tokens))+
  geom_point()+
  geom_smooth()


#Words in titles
billboard_titles<- billboard_data %>%
  unnest_tokens(word, Song) #It is taking from variable "Song" that is song title

#Are song titles getting shorter or longer (plotting all)
billboard_titles %>%
  group_by(Lyrics, Artist,Year) %>%
  summarise(types=n_distinct(word),tokens=length(word)) %>%
  ggplot(aes(x=Year,y=tokens))+
  geom_point()+
  geom_smooth()


#Are song titles getting shorter or longer (just plotting the averages here per year)
billboard_titles %>%
  group_by(Lyrics, Artist,Year) %>%
  summarise(types=n_distinct(word),tokens=length(word)) %>%
  ungroup() %>%
  group_by(Year) %>%
  summarise(tokens=mean(tokens)) %>%
  ggplot(aes(x=Year,y=tokens))+
  geom_point()+
  geom_smooth()







# VISUALISE TRENDS OF SINGLE WORDS:
# as proportion of all the words

word_to_search <- "love"

my_title <- paste("Trend of '",word_to_search, "'", sep="")

billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  count(word, Year) %>%
  filter(word==word_to_search) %>%
  #  complete(Year=1965:2015, fill=list(n=0)) %>%
  rename(count=n) %>%
  left_join(count(unnest_tokens(billboard_data,word, Lyrics), Year)) %>%
  ggplot(aes(x=Year, y=count/n)) +
  geom_point(shape=19, alpha=.5, colour="red" ) +
  geom_smooth(method=loess, colour="red", fill="grey") +
  labs(y="Proportion", title=my_title)+
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5)) # +
# ggsave(filename="plots/singe_word_example.pdf", width = 5, height = 5)

#Same thing with separate variable for tokens (quicker)
billboard_tokens %>%
  count(word, Year) %>%
  filter(word==word_to_search) %>%
  #  complete(Year=1965:2015, fill=list(n=0)) %>%
  rename(count=n) %>%
  left_join(count(billboard_tokens, Year)) %>%
  ggplot(aes(x=Year, y=count/n)) +
  geom_point(shape=19, alpha=.5, colour="red" ) +
  geom_smooth(method=loess, colour="red", fill="grey") +
  labs(y="Proportion", title=my_title)+
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5)) # +




## Sentiment analysis
## uses a simple vocabulary "get_sentiments("bing")" to track sentiments
## https://www.tidytextmining.com/sentiment.html

#How do the sentiments go over time
billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song,Year) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(Artist,Song,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=Year,y=sentiment))+
  geom_point(aes(color = sentiment>0),stat = 'identity')+
  geom_smooth()+
  theme_minimal()
#More or less fair coverage of all varieties across the time, somewhat more below 0 (negative) emotions in 2000s


#How about prevalence of negative or positive emotions in individual songs
plot1 <- 
  billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song,Year) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(Artist,Song,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=Year,y=positive))+
  geom_point(color="red",stat = 'identity')+
  geom_smooth()+
  theme_minimal()

plot2 <- 
  billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song,Year) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(Artist,Song,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=Year,y=negative))+
  geom_point(color="green",stat = 'identity')+
  geom_smooth()+
  theme_minimal()

#Put multiple graphs in one plot
gridExtra::grid.arrange(plot1,plot2)



#Looking at sentiments within songs
#Eminem has fairly negative songs throughout
billboard_data %>%
  filter(Artist=="eminem") %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song) %>%
  mutate(wordnumber = row_number()) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  mutate(line = wordnumber %/% 20) %>% #Change the number here to group the words into smaller or larger chunks
  count(line,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=line,y=sentiment))+
  geom_bar(aes(fill = sentiment>0),stat = 'identity')+
  theme_minimal()+
  facet_wrap(~Song)






#Looking at sentiments between songs. Is there an association between rank and sentiment?
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


#Are happier artists more popular? By artist, number songs in top and their sentiment scores. Top scoring artists seem fairly balanced, lower artists are all over.
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
## reflects how important a word is to a document in a collection or corpus
## Meaning: it finds the words that are special to that text, compared to all other texts in the comparison set
## Read more in https://www.tidytextmining.com/tfidf.html

#Find the keywords of each lady gaga song compared to other lady gaga songs
tf_idf <- billboard_data %>%
  filter(Artist=="lady gaga") %>% #Here only lady gaga songs are considered
  unnest_tokens(word, Lyrics) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(Artist,Song) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, Song, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()

#Now plot the dataframe
tf_idf %>% 
  group_by(Song) %>%
  top_n(10) %>% #And take the top 10 from them - might be less available
  filter(tf_idf>0) %>% #Ifnore keywords that are not at all special to the text
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = Song)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~Song,scale="free_y")



#Top 10 all time stored as variable
top10 <- billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10)

##Distinguishing keywords for each artist compared to other artists (all songs are lumped together per artist)
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
  filter(count>5) %>% #If we filter for words that occurred at least 5 times per that artist. Must be done AFTER tf_idf is calculated.
  top_n(10,tf_idf) %>%
  filter(tf_idf>0) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = Artist)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~Artist,scale="free_y")



##Distinguishing keywords for each decade compared to other decades (all songs and artists are lumped together per decade)
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
  filter(count>10) %>% #If we filter for words that occurred at least 10 times for that decade. This must be done after tf_idf
  top_n(10,tf_idf) %>%
  filter(tf_idf>0) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = factor(Decade))) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~Decade,scale="free_y")

#The word thoia appears 156 times, but just in one song
thoia <- billboard_tokens %>%
  filter(word=="thoia")

#Plotting option 1
tf_idf_decade %>%
  rename(count=n) %>%
  group_by(Decade) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing")) %>% 
  group_by(Decade,sentiment) %>%
  filter(count>5) %>% #If we filter for words that occurred at least 5 times for that decade. This must be done after tf_idf
  arrange(desc(tf_idf)) %>%
  top_n(10,tf_idf) %>% #top 10 rows, top_n() method has a problem if there are too many ties
  filter(tf_idf>0) %>%
  arrange(desc(tf_idf)) %>%
  ggplot(aes(x=reorder(word,count), y=tf_idf, fill = factor(sentiment))) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~Decade+sentiment,scale="free_y") 

#Plotting option 2 with same data
tf_idf_decade %>%
  rename(count=n) %>%
  group_by(Decade) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing")) %>% 
  group_by(Decade,sentiment) %>%
  filter(count>5) %>% #If we filter for words that occurred at least 5 times for that decade. This must be done after tf_idf
  top_n(10,tf_idf) %>%
  filter(tf_idf>0) %>%
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

#1960s has less distinct values than the following decades. It uses more common vocabulary

# General trends in positive emotions
plot1 <-
  billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  inner_join(get_sentiments("bing")) %>%
  count(Year, sentiment) %>%
  spread(sentiment, n, fill = 0, convert=T) %>%
  left_join(count(unnest_tokens(billboard_data,word, Lyrics), Year)) %>%
  ggplot(aes(Year, (positive)/n)) + # change here
  geom_point(shape=19, alpha=.5, colour="black" ) +
  geom_smooth(method=lm, colour="black", fill="grey") +
  labs(y="Proportion", title="Positive emotions")  +
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5))# +
#  ggsave(filename="plots/trend_example1.pdf", width = 5, height = 5)

#General trend for negative words
plot2 <-
  billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  inner_join(get_sentiments("bing")) %>%
  count(Year, sentiment) %>%
  spread(sentiment, n, fill = 0, convert=T) %>%
  left_join(count(unnest_tokens(billboard_data,word, Lyrics), Year)) %>%
  ggplot(aes(Year, (negative)/n)) + # change here
  geom_point(shape=19, alpha=.5, colour="black" ) +
  geom_smooth(method=lm, colour="black", fill="grey") +
  labs(y="Proportion", title="Negative emotions")  +
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5))# +
#  ggsave(filename="plots/trend_example2.pdf", width = 5, height = 5)


#Positive emotions go up, negative emotions go down
#But the scale is proportion of all of vocabulary, and it is around 4-5% for positive vocabulary and 2-3% for negative vocabulary for the entire duration
gridExtra::grid.arrange(plot1,plot2,ncol=2)
