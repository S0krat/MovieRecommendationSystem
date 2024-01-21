library(readr)
library(tidyverse)

biased_train_set <- read_rds("movielens/biased_train_set.Rds")
movie_map <- read_rds("movielens/movie_map.Rds")

movies10k <- biased_train_set %>%
  group_by(movieId) %>%
  summarise(n=n()) %>%
  top_n(7000, n) %>%
  .$movieId

mdf <- data.frame(movieId=movies10k)

biased_train_set <- filter(biased_train_set, movieId %in% movies10k)

i <- 0
movie_corr <- sapply(movies10k, function(as) {
  print(i/70)
  i <<- i + 1
  users306 <- biased_train_set %>%
    filter(movieId==as) %>%
    rename("movie_bias"="user_bias") %>%
    select(userId, movie_bias)
  
  biased_train_set %>%
    filter(userId %in% users306$userId) %>%
    left_join(users306, by="userId") %>%
    group_by(movieId) %>%
    summarise(corr=sum(user_bias * movie_bias)) %>%
    mutate(corr=corr/max(corr)) %>%
    right_join(mdf, by="movieId") %>%
    mutate(corr=ifelse(is.na(corr), 0, corr)) %>%
    .$corr
})

movie_corr <- data.frame(movie_corr)
colnames(movie_corr) <- movies10k
movie_corr <- mutate(movie_corr, movieId=movies10k)
write_rds(movie_corr, "~/HarvardX-s-Data-Science-Professional-Certificate/movielens/movie_corr.Rds")
