library(readr)
library(tidyverse)

biased_train_set <- read_rds("movielens/biased_train_set.Rds")
fit_movies <- read_rds("movielens/fit_movies.Rds")
movie_map <- read_rds("movielens/movie_map.Rds")

rated <- read.csv(file="C:\\Lev\\rated.csv")
rated <- rated %>% 
  filter(!is.na(rating)) %>% 
  mutate(rating = rating / 2) %>% 
  left_join(fit_movies, by="movieId") %>% 
  mutate(bias=rating-b_i) %>% 
  select(movieId, rating, bias) %>% 
  mutate(bias = bias - mean(bias)) %>%
  arrange(movieId)

corrs <- biased_train_set %>%
  filter(movieId %in% rated$movieId) %>%
  group_by(userId) %>%
  filter(n() > 10) %>%
  left_join(rated, by="movieId") %>%
  summarise(corr=sum(user_bias * bias)) %>%
  mutate(corr=corr/sum(abs(corr)))

u_bias <- biased_train_set %>%
  filter(!movieId %in% rated$movieId) %>% 
  inner_join(corrs, by="userId") %>% 
  group_by(movieId) %>%
  summarise(u_bias=sum(user_bias * corr))

rm(corrs, biased_train_set)

movie_corr <- read_rds("movielens/movie_corr.Rds")
m_bias <- pivot_longer(movie_corr, cols=-movieId, names_to="movieId2", values_to = "corr") %>% 
  mutate(movieId2 = as.numeric(movieId2)) %>%
  inner_join(rated, by="movieId") %>%
  group_by(movieId2) %>%
  summarise(m_bias=sum(bias * corr) / sum(abs(corr))) %>%
  rename("movieId"="movieId2")

rm(movie_corr)

movie_map %>% 
  filter(!movieId %in% rated$movieId) %>%
  filter(!movieId %in% c(159817, 179135, 171011, 142115)) %>%
  inner_join(fit_movies, by="movieId") %>%
  left_join(m_bias, by="movieId") %>%
  mutate(m_bias=ifelse(is.na(m_bias), 0, m_bias)) %>%
  left_join(u_bias, by="movieId") %>%
  mutate(u_bias=ifelse(is.na(u_bias), 0, u_bias)) %>%
  mutate(m_impact=0.64 * m_bias, u_impact=1.71 * u_bias) %>%
  mutate(predict=b_i + m_impact + u_impact) %>%
  select(movieId,title, predict,b_i, m_impact, u_impact) %>%
  arrange(desc(predict)) %>%
  top_n(20, predict)
  
  
