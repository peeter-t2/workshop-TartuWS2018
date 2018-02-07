#Compiled for R tidyverse workshop in Tartu Winter School 2018, by Peeter Tinits
#
#from https://github.com/rstudio/webinars/blob/master/46-tidyverse-visualisation-and-manipulation-basics/ (RStudio webinar by garretgman, CC)
#and http://rpubs.com/aelhabr/tidyverse-basics



lapply(c("gapminder","tidyverse"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))


library(gapminder)
library(tidyverse)


#The Gapminder dataset
#https://www.gapminder.org/answers/how-does-income-relate-to-life-expectancy/



gapminder %>%
  group_by(country) %>%
  filter(year==max(year)) %>%
  ggplot(aes(y=lifeExp,x=gdpPercap,size=pop,color=continent))+
  geom_point()+
  scale_x_log10()


#Basic tidyverse transformations
#
#basic model is the following
#data %>%
#  process()
#
# %>% - carry the data into function
#select() selecting variables
#filter() provides basic filtering capabilities
#arrange() ordering data
#group_by() groups data by categorical levels
#summarise() summarise data by functions of choice
#join() joining separate dataframes
#mutate() create new variables

gapminder


gapminder %>%
  filter(country=="Finland")


gapminder %>%
  filter(country=="Finland") %>%
  select(year,pop)
  

gapminder %>% 
  filter(year == 2007) %>% 
  arrange(desc(pop))

gapminder %>% 
  filter(year == 2007) %>% 
  arrange(desc(lifeExp)) %>% 
  select(country, lifeExp)

gapminder %>% 
  mutate(gdp = pop * gdpPercap)


#Plotting function
#data %>%
#ggplot(mapping = aes(<MAPPINGS>)) +
#  <GEOM_FUNCTION>()


gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=year,y=pop))+
  geom_line()+
  theme_minimal()

gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=year,y=pop))+
  geom_line()+
  theme_minimal()




gapminder


gapminder %>% 
  mutate(gdp = pop * gdpPercap)

gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  summarise(max_gdp = max(gdp))


gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(max_gdp = max(gdp))


gapminder %>% 
  group_by(country) %>% 
  summarise(max_gdpPercap = max(gdpPercap)) %>% 
  arrange(desc(max_gdpPercap))


gapminder %>% 
  group_by(continent) %>% 
  summarise(mean_gdpPercap = mean(gdpPercap)) %>% 
  arrange(desc(mean_gdpPercap))


gapminder %>% 
  group_by(continent,year) %>% 
  summarise(mean_gdpPercap = mean(gdpPercap)) %>% 
  arrange(desc(continent,year)) %>% 
  ggplot(aes(x=year,y=mean_gdpPercap,color=continent))+
  geom_line()+
  theme_minimal()




#Trends in time

gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  summarise(first_gdp = first(gdp), last_gdp = last(gdp))


gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  summarise(first_gdp = first(gdp), last_gdp = last(gdp))
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(first_gdp = first(gdp), last_gdp = last(gdp))



gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(gdp1952 = first(gdp), gdp2007 = last(gdp))
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(gdp1952 = first(gdp), gdp2007 = last(gdp)) %>% 
  mutate(cagr = ((gdp2007 / gdp1952) ^ (1/55) - 1) * 100) %>% 
  arrange(desc(cagr)) %>% 
  select(country, cagr)



gapminder
gapminder %>% 
  filter(year == 1952) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  arrange(desc(gdp)) %>% 
  select(country, gdp)




top_10 <-
  gapminder %>% 
  filter(year == 1952) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  arrange(desc(gdp)) %>% 
  top_n(10, gdp) %>% 
  pull(country) #makes it into a simple sequence instead of datframe


gapminder
gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  ggplot() +
  geom_line(mapping = aes(x = year, y = gdp, color = country))




gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap)

gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  mutate(scaled_gdp = gdp / first(gdp)) %>% 
  ggplot() +
  geom_line(mapping = aes(x = year, y = scaled_gdp, color = country)) +
  labs(title = "GDP Per Capita (Scaled)")



gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(start = first(gdp), end = last(gdp)) %>% 
  mutate(cagr = ((end/start) ^ (1 / 55) - 1) * 100) %>% 
  arrange(desc(cagr)) %>% 
  select(country, cagr)
gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(start = first(gdp), end = last(gdp)) %>% 
  mutate(cagr = ((end/start) ^ (1 / 55) - 1) * 100) %>% 
  arrange(desc(cagr)) %>% 
  select(country, cagr) %>% 
  ggplot() +
  geom_col(mapping = aes(x = country, y = cagr))

