library(readr)
library(tidyverse)

train_set <- read_rds("movielens/train_set.Rds")

train_set <- select(train_set, movieId, userId, rating)

fit_movies <- train_set %>% group_by(movieId) %>%
  summarise(b_i=sum(rating) / n(), movieId=movieId[1])

# write_rds(fit_movies, "~/HarvardX-s-Data-Science-Professional-Certificate/movielens/fit_movies.Rds")

fit_users <- train_set %>%
  left_join(fit_movies, by="movieId") %>%
  mutate(rating=rating-b_i) %>%
  group_by(userId) %>%
  summarise(b_u=sum(rating) / n(), userId=userId[1])

biased_train_set <- train_set %>%
  left_join(fit_users, by="userId") %>%
  left_join(fit_movies, by="movieId") %>%
  mutate(user_bias=rating-mu-b_u-b_i) %>%
  select(movieId, userId, user_bias) %>%
  arrange(movieId)

write_rds(biased_train_set, "~/HarvardX-s-Data-Science-Professional-Certificate/movielens/biased_train_set.Rds")
