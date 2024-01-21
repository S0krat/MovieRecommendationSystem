library(readr)
library(tidyverse)

movielens_dataset <- read.csv("C:\\Users\\limon\\OneDrive\\Документы\\HarvardX-s-Data-Science-Professional-Certificate\\ml-latest\\ratings.csv")
movie_map <- read.csv("C:\\Users\\limon\\OneDrive\\Документы\\HarvardX-s-Data-Science-Professional-Certificate\\ml-latest\\movies.csv")
movie_map <- select(movie_map, movieId, title)
write_rds(movie_map, "~/HarvardX-s-Data-Science-Professional-Certificate/movielens/movie_map.Rds")

movielens_dataset <- movielens_dataset %>% group_by(movieId) %>%
  filter(n() > 50) %>%
  ungroup()

movielens_dataset <- movielens_dataset %>% group_by(userId) %>%
  filter(n() > 20) %>%
  ungroup()

userIds <- unique(movielens_dataset$userId)
n <- length(userIds)
user_subset <- sample(userIds, n / 5) 

train_set <- movielens_dataset %>% 
  filter(!userId %in% user_subset)
test_set <- movielens_dataset %>% 
  filter(userId %in% user_subset[1:n/10])
validate_set <- movielens_dataset %>%
  filter(userId %in% user_subset[n/10+1:n/5])
rm(movielens_dataset, user_subset, n, userIds)

write_rds(train_set, "~/HarvardX-s-Data-Science-Professional-Certificate/movielens/train_set.Rds")
write_rds(test_set, "~/HarvardX-s-Data-Science-Professional-Certificate/movielens/test_set.Rds")
write_rds(validate_set, "~/HarvardX-s-Data-Science-Professional-Certificate/movielens/validate_set.Rds")