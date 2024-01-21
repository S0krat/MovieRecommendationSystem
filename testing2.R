library(readr)
library(caret)
library(tidyverse)

val_set <- read_rds("movielens/test_set.Rds")
val_set <- select(val_set, userId, movieId, rating)

movie_corr <- read_rds("movielens/movie_corr.Rds")
movie_corr <- pivot_longer(movie_corr, cols=-movieId, names_to="movieId2", values_to = "corr")
movie_corr <- movie_corr %>% mutate(movieId2 = as.numeric(movieId2))

movie_corr <- filter(movie_corr, abs(corr) > 0.05)

set.seed(10)

test_index <- createDataPartition(as.factor(val_set$userId), times = 1, p = 0.1, list = FALSE)
test_val <- val_set[test_index,]
train_val <- val_set[-test_index,]
rm(val_set, test_index)

fit_movies <- read_rds("movielens/fit_movies.Rds")

fit_users <- train_val %>%
  left_join(fit_movies, by="movieId") %>%
  mutate(rating=rating-b_i) %>%
  group_by(userId) %>%
  summarise(b_u=mean(rating))

RMSE <- function(a, b) {
  sqrt(sum((a-b)**2)/length(a))
}

train_val_biased <- train_val %>%
  filter(movieId %in% movie_corr$movieId) %>%
  left_join(fit_movies, by="movieId") %>%
  left_join(fit_users, by="userId") %>%
  mutate(user_bias=rating-b_u-b_i) %>%
  select(userId, movieId, user_bias)

test_val <- mutate(test_val, bias=0)

i <- 1

val_users <- unique(train_val_biased$userId)
isss <- length(val_users)

for (id in val_users) {
  if (i %% 100 == 0) {
    print(i/isss)
  }
  i <<- i + 1
  tt <- filter(train_val_biased, userId==id) %>%
    select(movieId, user_bias)
  test_movies <- filter(test_val, userId==id) %>% .$movieId
  movie_corr_temp <- filter(movie_corr, movieId2%in%test_movies)
  tt <- inner_join(tt, movie_corr_temp, by="movieId") %>%
    group_by(movieId2) %>%
    summarise(bias=sum(user_bias * corr) / sum(abs(corr))) %>%
    rename("movieId"="movieId2")
  
  ids <- which(test_val$userId==id)
  test_val$bias[ids] <- test_val[ids, 2] %>%
    left_join(tt, by="movieId") %>%
    .$bias
}

alpha <- 0.81
test_val <- filter(test_val, !is.na(bias))
test_val %>% 
  left_join(fit_movies, by="movieId") %>% 
  left_join(fit_users, by="userId") %>%
  mutate(predict=b_u+b_i+alpha*bias) %>%
  summarise(rmse=RMSE(predict, rating)) %>%
  .$rmse

# 0.79145
