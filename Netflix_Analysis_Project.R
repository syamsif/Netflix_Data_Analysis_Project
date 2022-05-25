#The purpose of this analysis is to practice analyzing and visualizing data, and hopefully communicate some best-practices along the way.

#This project will use the following dataset:
#Dataset source: https://www.kaggle.com/datasets/victorsoeiro/netflix-tv-shows-and-movies

#Bussiness Task
#Which movie had biggest and lowest imdb Score all the time?
#Which genre had biggest IMDb score for time frame?
#If there is a new film project, what would you recommend?

#========================
#Prepare the data
#========================
##load packages
library(tidyverse)
library(plyr)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)

##In order to understand the contents of this dataset, a number of analyzes were carried out
##$where is the default directory?
getwd()
##set new directory
setwd("C:/Users/Syamsi/Documents/GitHub/Netflix")

##load csv data
movie_title<-read_csv("titles.csv")
movie_credit<-read_csv("credits.csv")

##check the data
head(movie_title)
colnames(movie_title)
str(movie_title)

##check total columns and rows
dim(movie_title)

##how many serial movie there?
sum(movie_title$seasons>1,na.rm = TRUE)

movie_title %>%  
  select(imdb_score) %>% 
  summary()

head(movie_title)

#movie_title[movie_title$genres=="[]",]

#========================
#Clean the data
#========================
#We just want to know the score of IMDb. We need the movie with IMDb score recorded
#Filter out the movie that doesn't have Imdb score
movie_title_2<-movie_title[!(is.na(movie_title$imdb_score)|movie_title$genres=="[]"),]
head(movie_title_2)

movie_title_2[movie_title_2$genres=="[]",]

##Make sure there is no duplicate data on "Id"
n_distinct(movie_title_2$id)
nrow(movie_title_2)
isTRUE(n_distinct(movie_title_2$id) == nrow(movie_title_2))

##Because genres column could has more than one genre,
##We will separate them by comma. But we need to delete the other character
##Like [,].' and <space>" ".
movie_title_2 %>% 
  select(genres,imdb_score)

movie_title_2$genres<-gsub("\\[","",as.character(movie_title_2$genres))
movie_title_2$genres<-gsub("\\]","",as.character(movie_title_2$genres))
movie_title_2$genres<-gsub("\\'","",as.character(movie_title_2$genres))
movie_title_2$genres<-gsub(" ","",as.character(movie_title_2$genres))

##check the new data frame
movie_title_2

##check whether number of unique rows are match with number of rows
nrow(table(movie_title_2$genres))

##what is the year range in this new data frame?
max_year<-max(movie_title_2$release_year)
max_year
min_year<-min(movie_title_2$release_year)
min_year

#========================
#ANALYSIS
#========================
##Descriptive Analysis for IMDb_score in 1953 - 2022
summary(movie_title_2$imdb_score)

##We want to know the average score for each genre
genre_avg<-movie_title_2 %>%
  tidyr::separate_rows(genres,sep=",") %>%
  dplyr::group_by(genres) %>%
  dplyr::summarize(score_avg=mean(imdb_score))

head(genre_avg)
View(genre_avg)

##What is minimum and maximum score of average imdb score?
max(genre_avg$score_avg)
min(genre_avg$score_avg)

##Which genre had maximum and minimum score?
genre_avg[which.max(genre_avg$score_avg),]
genre_avg[which.min(genre_avg$score_avg),]

##plot graph of the Genres Average imdb Score
genre_avg %>% 
  mutate(genres=fct_reorder(genres,desc(score_avg))) %>% 
  ggplot(aes(x=genres,y=score_avg,fill=genres))+ 
  geom_col(position = "dodge")+
  geom_text(aes(label=sprintf("%0.2f", round(score_avg, digits = 2))),vjust=-0.5)+
  theme(axis.text.x = element_text(angle=45,vjust=0.5,hjust=0.3))+
  labs(title="Average IMDb Score by Genre",
       subtitle = "Timeframe:1953-2022")

##What is the Average IMDb score for the past 10 years for each genre?
movie_title_3<-movie_title_2[!(movie_title_2$release_year<(max_year-10)),]
 
genre_avg_10pyr<-movie_title_3 %>% 
  tidyr::separate_rows(genres,sep=",") %>% 
  dplyr::group_by(genres) %>%
  dplyr::summarize(score_avg=mean(imdb_score))

head(genre_avg_10pyr)

###plot the average IMDb score grouped by genres for timeframe (2012-2022)
genre_avg_10pyr %>% 
  mutate(genres=fct_reorder(genres,desc(score_avg))) %>% 
  ggplot(aes(x=genres,y=score_avg,fill=genres))+ 
  geom_col(position = "dodge")+
  geom_text(aes(label=sprintf("%0.2f", round(score_avg, digits = 2))),vjust=-0.5)+
  theme(axis.text.x = element_text(angle=45,vjust=0.5,hjust=0.3))+
  labs(title="Average IMDb Score by Genre",
       subtitle = "Timeframe:2012-2022")

##Number of movie for each genres in the past 10 yrs
dplyr::count(movie_title_3,'title')
numb_movie<-movie_title_3 %>%
  tidyr::separate_rows(genres,sep=",") %>% 
  dplyr::group_by(genres) %>%
  dplyr::count(genres)

##Number of movie released 2012-2022
numb_movie_2<-numb_movie %>% arrange(-n)
View(numb_movie_2)

### plot number of movie released 2012-2022
numb_movie_2 %>%
  mutate(genres=fct_reorder(genres,desc(n))) %>%
  ggplot(aes(x=genres,y=n,fill=genres))+
  geom_col(position='dodge')+
  geom_text(aes(label=sprintf("%1.0f",n)),vjust=-0.5)+
  theme(axis.text.x = element_text(angle=45,vjust=0.5,hjust=0.3))+
  labs(title="Number of movie released",
       subtitle = "Timeframe:2012-2022")

##Number of movie per year for each genre
numb_movie4<-movie_title_3 %>%
  group_by(release_year) %>%
  separate_rows(genres,sep=",") %>% 
  select(genres,release_year)

growth_genre<-numb_movie4 %>% 
  group_by(genres,release_year) %>% 
  dplyr::mutate(count=n())

###plot line diagram about movie released per year for each genre
growth_genre %>% 
  ggplot(aes(x=release_year,y=count,group=genres,color=genres))+
  geom_line(size=1)+
  scale_x_continuous(breaks = 2012:2022)+
  labs(title="Number of movie released",
       subtitle = "Timeframe:2012-2022")

#Drama is a genre that has more films released in the last 10 years.
#Although the average rating is not the best.
#But there are indications that customers prefer movies with the drama genre.

##Return 10 movie that have highest score in genre drama for the past 10 yrs?
history_movie<-movie_title_3 %>%
  arrange(-imdb_score) %>% 
  filter(genres=='drama') %>% 
  separate_rows(genres,sep=",")%>%
  select(imdb_score,title,type,release_year,genres)

history_movie[1:10,]
nrow(history_movie)  


#Is that any correlation between runtime and score for dramas?
drama_score<-movie_title_3[(movie_title_3$genres=='drama'),] %>%
  separate_rows(genres,sep=",") %>% 
  group_by(genres)

head(drama_score)
  
drama_score %>% 
  mutate(genres=fct_reorder(genres,desc(imdb_score))) %>%
  ggplot(mapping=aes(x=runtime,y=imdb_score,color=genres))+ 
  geom_point()+
  labs(title="Runtime (minutes) VS imdb score for genre drama",
       subtitle = "timeframe : 2012-2022")

#========================
#CONCLUSION
#========================
#In this analysis, there are several conclusions, namely:
#1. Even though it has a high average IMDb score, it does not mean that the genre has a greater number of films than other genres.
#2. Even though the historical genre has a low IMDb average, the number of films released is far less than dramas. This shows that movies with the history genre have good quality but fewer fans than dramas. There needs to be a supporting dataset such as the number of viewers.
#3. There is no correlation between duration and IMDb score.

#Analysis that can be done at a later date
##It is necessary to have audience data for each film,
##as well as audience data from each country.
##This dataset will be interesting because it is possible to remake movies
##into local cultures and languages with local actors in certain countries
##to increase Netflix engagement in a country/region.

