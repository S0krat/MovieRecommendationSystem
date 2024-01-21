library(readr)
library(caret)
library(tidyverse)

test_set <- read_rds("movielens/test_set.Rds")
test_set <- select(test_set, userId, movieId, rating)
test_set <- test_set %>% mutate(userId=as.factor(userId))

set.seed(10)

test_index <- createDataPartition(test_set$userId, times = 1, p = 0.1, list = FALSE)
test_test <- test_set[test_index,]
train_test <- test_set[-test_index,]

fit_movies <- read_rds("movielens/fit_movies.Rds")

fit_users <- train_test %>%
  left_join(fit_movies, by="movieId") %>%
  mutate(rating=rating-b_i) %>%
  group_by(userId) %>%
  summarise(b_u=mean(rating))

RMSE <- function(a, b) {
  sqrt(sum((a-b)**2)/length(a))
}

test_test %>% left_join(fit_movies, by="movieId") %>%
  left_join(fit_users, by="userId") %>%
  mutate(predict=b_i+b_u) %>%
  summarise(rmse=RMSE(rating, predict)) %>%
  .$rmse

rm(list=ls())
