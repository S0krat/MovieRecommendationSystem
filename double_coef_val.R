library(readr)
library(tidyverse)

m_val <- read_rds("movielens/test_val1.Rds")
u_val <- read_rds("movielens/test_val2.Rds")

m_val <- rename(m_val, "m_bias"="bias")
u_val <- rename(u_val, "u_bias"="bias")
u_val_joined <- u_val %>%
  left_join(m_val, by="predict")
  
u_val_joined <- u_val_joined %>%
  rename("rating"="rating.x") %>%
  select(rating, u_bias, m_bias, predict)

RMSE <- function(a, b) {
  sqrt(sum((a-b)**2)/length(a))
}

alpha <- 0.64
beta <- 1.71
# 0.7871568 0.64 1.71

alpha_set <- seq(0.3, 1, 0.01)

rmses <- sapply(alpha_set, function(a) {
  u_val_joined %>%
    mutate(final=predict+a*m_bias+beta*u_bias) %>%
    filter(!is.na(final)) %>%
    summarise(rmse=RMSE(final, rating)) %>%
    .$rmse
})

alpha_set[which.min(rmses)]
min(rmses) 
plot(alpha_set, rmses)


beta_set <- seq(1, 3, 0.01)

rmses <- sapply(beta_set, function(b) {
  u_val_joined %>%
    mutate(final=predict+alpha*m_bias+b*u_bias) %>%
    filter(!is.na(final)) %>%
    summarise(rmse=RMSE(final, rating)) %>%
    .$rmse
})

beta_set[which.min(rmses)]
min(rmses) 
plot(beta_set, rmses)
