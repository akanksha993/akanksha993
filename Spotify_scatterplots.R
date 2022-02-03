#read data
spotify.df <- read.csv("spotify_decade.csv", stringsAsFactors = TRUE)
#checking correlations
cor(spotify.df$target , spotify.df$danceability)
cor(spotify.df$target , spotify.df$energy)
cor(spotify.df$target , spotify.df$key)
cor(spotify.df$target , spotify.df$loudness)
cor(spotify.df$target , spotify.df$speechiness)
cor(spotify.df$target , spotify.df$acousticness)
cor(spotify.df$target , spotify.df$instrumentalness)
cor(spotify.df$target , spotify.df$liveness)
cor(spotify.df$target , spotify.df$valence)
cor(spotify.df$target , spotify.df$tempo)
cor(spotify.df$target , spotify.df$duration_ms)
cor(spotify.df$target , spotify.df$chorus_hit)

View(spotify.df)
selected.var <- c(4,5,6,7,8,9,10,11,12, 13,14,15,16,17,18,19)
music.df <- spotify.df[, selected.var]
set.seed(100)  # set seed for reproducing the partition

# Random sample indexes
train.index <- sample(1:nrow(music.df), nrow(music.df)*0.7)  

# Build training and validation set by indexing
train.df <- music.df[train.index, ]
valid.df <- music.df[-train.index, ]

library(rpart)
library(rpart.plot)

default.ct <- rpart(target ~ ., data = train.df, method = "class")
## Plotting Decision tree 
rpart.plot(default.ct, extra = 1)
#Names of Columns
names(spotify.df)

#Scatterplots
library(ggplot2)
hits.spotify.df <- spotify.df[spotify.df$target == 1 ,c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
spotify.df$target <- factor(spotify.df$target)
levels(spotify.df$target) <- c("Flop", "Hit")
ggplot(data = spotify.df,aes(x = loudness, y = energy, color=target)) +
  geom_point(alpha = 0.4 ) +
  ggtitle(" Energy vs Loudness")


hits.spotify.df <- spotify.df[spotify.df$target == 1 ,c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
spotify.df$target <- factor(spotify.df$target)
levels(spotify.df$target) <- c("Flop", "Hit")
ggplot(data = spotify.df,aes(x = acousticness, y = speechiness, color=target)) +
  geom_point(alpha = 0.4 ) +
  ggtitle(" Acousticness vs Speechiness")

hits.spotify.df <- spotify.df[spotify.df$target == 1 ,c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
spotify.df$target <- factor(spotify.df$target)
levels(spotify.df$target) <- c("Flop", "Hit")
ggplot(data = spotify.df,aes(x = valence, y = danceability, color=target)) +
  geom_point(alpha = 0.4 ) +
  ggtitle(" Danceability vs Valence")

##waste
#line graph#

sixties.df <- hits.spotify.df[hits.spotify.df$Decade == 1960,]
valence <- sixties.df$valence
danceability <- sixties.df$danceability

# Vectors
plot(valence, danceability, type = "l") +
  title("valence vs danceability")


install.packages("tidyverse")
require(tidyverse)
spotify.df %>%
  select( track , artist, uri, danceability, energy, key, loudness, mode, speechiness, 
  acousticness, instrumentalness, liveness, valence, tempo, duration_ms, time_signature,
  chorus_hit, sections, target, Decade) %>%
  filter(target==1)



names(spotify.df)

hitmusic.df <- filter(spotify.df, spotify.df$target)
filter(spotify.df, spotify.df$target ==1)
View(hitmusic.df)
View(hitmusic.df)
ggplot(data = spotify.df) + geom_bar(aes(x = Decade, y = danceability), stat = "summary", fun = "mean", size = 10, alpha = 0.5) 
