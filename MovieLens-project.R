##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)

RMSE_target <- 0.86490

#Download of data (from the course)

#MovieLens 10M dataset:
#https://grouplens.org/datasets/movielens/10m/
#http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

###Added after analysis but it was put here to avoid modify the edx and validation separated and having different values in the variable unique ratings
#release year
movielens <- movielens %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),title = str_remove(title, "[/(]\\d{4}[/)]$"))
#unique ratings
movielens <- movielens %>% group_by(userId) %>% 
  mutate(unique_ans = n_distinct(rating)) %>% ungroup()

#Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

#Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########################################################
# Analysis
##########################################################

###This part I made it after the download, after I finish doing data analysis I mutated
###the columns I added in edx to movielens, that means before the 
###data split of edx and validation

###DO NOT RUN, is not needed for the analysis, created before download section changes
###add a column year of release and remove year from movie title
edx <- edx %>% 
  mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),title = str_remove(title, "[/(]\\d{4}[/)]$"))
###NOW YOU CAN RUN

###plot of mean of the ratings vs the year of release of each movie
plot_point <- edx %>% group_by(movieId) %>% add_count(rating) %>%
  mutate(mean_rating=mean(rating)) %>%
  select(movieId,title,releaseyear,n,mean_rating) %>% ungroup() %>% unique() %>%
  ggplot(aes(releaseyear,mean_rating)) +
  geom_point(alpha=0.0875,colour = "blue") +
  ggtitle("Mean rating of each movie") +
  ylab("Mean rating of each movie") +
  xlab("Year of release")

plot_point + edx %>% group_by(movieId) %>% mutate(mean_rating=mean(rating)) %>% 
  ungroup() %>% group_by(releaseyear) %>% 
  summarise(mean_year = mean(mean_rating), .groups='keep') %>%
  geom_line(mapping=aes(releaseyear,mean_year),size=1,alpha=0.8)

remove(plot_point)

###DO NOT RUN, is not needed for the analysis, created before download section changes
###How many different ratings did each user has? 
edx <- edx %>% group_by(userId) %>% mutate(unique_ans = n_distinct(rating)) %>% ungroup()
###NOW YOU CAN RUN

#Distribution of the groups
ggplot(edx,aes(unique_ans)) +
  ggtitle("Bar graph of different ratings by user") +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.4, colour = "black") +
  scale_x_discrete(name="Number of different ratings",
                   limits = c("1","2","3","4","5","6","7","8","9","10")) +
  ylab("Users quantity")

##Is there a relation between the number of unique answers and how users rate movies?

#Create a matrix where rows are the ratings, the columns the number 
#of unique ratings a user has, and the value is how many ratings are
rating_m <- edx %>% group_by(unique_ans) %>% select(unique_ans,rating) %>% 
  table() %>% matrix(10,10)
colnames(rating_m) <- seq(0.5,5,0.5)
rownames(rating_m) <- 1:10

df <- as.data.frame(rating_m) %>% gather(key='y', value='val')
df$x <- rep(1:10, 10)
ggplot(df, aes(x, y, fill= log(val+1) )) + 
  geom_tile() +
  geom_text(aes(x, y, label=round(val,2))) +
  scale_fill_gradient(low = "white", high = "red") + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10")) +
  ggtitle("Distribution of ratings grouped by unique_ans") +
  ylab("Rating") +
  xlab("Number of unique ratings") +
  theme_bw() +
  theme(legend.position = "none")
remove(rating_m,df)

#All genres
genres <- edx %>% separate_rows(genres, sep = "\\|") %>%
  select(genres)

###Plot of count for each genre in each rating
##Not in .rmd file, after the analysis I decided to use the combo version 
##of the variable, maybe useful for future work.
genres %>% ggplot(aes(genres)) +
  geom_bar() +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.4, colour = "black")

###Table of each genre and combo
##In .rmf file, because I can compare the low numbers in the combo genres table
##with the genres table
genres_combo_sorted <- as.data.frame(sort(table(edx$genres),decreasing = TRUE))
colnames(genres_combo_sorted) <- c("Genres Combo","Frequency")

genres_sorted <- as.data.frame(sort(table(genres),decreasing = TRUE))
colnames(genres_sorted) <- c("Genres","Frequency")

genres_combo_sorted %>% {
  rbind(head(., 5), tail(., 5))
} 

genres_sorted %>% {
  rbind(head(., 5), tail(., 5))
} 
#remove the unnecessary variables
rm(genres,genres_combo_sorted,genres_sorted)

#Movies with no genre in the data set
edx %>% filter(genres == "(no genres listed)") %>% pull(title) %>% unique()

#timestamp
###using the lubridate library
#the first part: year(as_datetime(edx$timestamp)) we get the year of the rating
#we already have the year of release
#we get the difference between the year of rating and the year of release 
#this value should be greater or equal to 0
#but we find that in the data this is NOT always the case
sum((year(as_datetime(edx$timestamp)) - edx$releaseyear ) < 0)
#just -1 and -2 probably a rounding error
unique((year(as_datetime(edx$timestamp)) - edx$releaseyear )[(year(as_datetime(edx$timestamp)) - edx$releaseyear ) < 0])
sum((year(as_datetime(edx$timestamp)) - edx$releaseyear )[(year(as_datetime(edx$timestamp)) - edx$releaseyear ) < 0] == -2)
##########################################################
# Methods
##########################################################

#RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

###Split edx, in train and test
#Test set will be 10% of edx data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_edx <- edx[-test_index,]
temp <- edx[test_index,]

#Make sure userId and movieId in validation set are also in train_edx set
test_edx <- temp %>% 
  semi_join(train_edx, by = "movieId") %>%
  semi_join(train_edx, by = "userId")

#Add rows removed from test_edx set back into train_edx set
removed <- anti_join(temp, test_edx)
train_edx <- rbind(train_edx, removed)

#Remove unnecessary variables
rm(test_index, temp, removed)

####Model approach

###Naive approach (mean rating)
{
  mu <- mean(train_edx$rating)
  naive_rmse <- RMSE(test_edx$rating,mu)
  
  rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
}
###Movie effect
{
  b_i <- train_edx %>%
    group_by(movieId) %>%
    summarize(b_i = mean(rating - mu), .groups = "keep")
  
  predicted_ratings <- test_edx %>%
    left_join(b_i, by="movieId") %>%
    mutate(pred = mu+b_i) %>%
    pull(pred)
  
  rmse_movies <- RMSE(test_edx$rating, predicted_ratings)
  rmse_results <- rmse_results %>% 
    add_row(method="+ Movie effect", RMSE=rmse_movies)
}
###User effect
{
  b_u <- train_edx %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i), .groups = "keep")
  
  predicted_ratings <- test_edx %>%
    left_join(b_u, by="userId") %>%
    left_join(b_i, by="movieId") %>%
    mutate(pred = mu+b_i+b_u) %>%
    pull(pred)
  
  rmse_user <- RMSE(test_edx$rating, predicted_ratings)
  rmse_results <- rmse_results %>% 
    add_row(method="+ User effect", RMSE=rmse_user)
}
###Genre combo effect
{
  b_g <- train_edx %>%
    left_join(b_u, by="userId") %>%
    left_join(b_i, by="movieId") %>%
    group_by(genres) %>%
    summarize(b_g = mean(rating - mu - b_i - b_u), .groups = "keep")
  
  predicted_ratings <- test_edx %>%
    left_join(b_u, by="userId") %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_g, by="genres") %>%
    mutate(pred = mu+b_i+b_u+b_g) %>%
    pull(pred)
  
  rmse_genres <- RMSE(test_edx$rating, predicted_ratings)
  rmse_results <- rmse_results %>% 
    add_row(method="+ Genre combo effect", RMSE=rmse_genres)
}
###Release year effect
{
  b_r <- train_edx %>%
    left_join(b_u, by="userId") %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_g, by="genres") %>%
    group_by(releaseyear) %>%
    summarize(b_r = mean(rating - mu - b_i - b_u - b_g), .groups = "keep")
  
  predicted_ratings <- test_edx %>%
    left_join(b_u, by="userId") %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_r, by="releaseyear") %>%
    mutate(pred = mu+b_i+b_u+b_g+b_r) %>%
    pull(pred)
  
  rmse_releaseyear <- RMSE(test_edx$rating, predicted_ratings)
  rmse_results <- rmse_results %>% 
    add_row(method="+ Release year effect", RMSE=rmse_releaseyear)
}
###Unique ratings effect
{
  b_ru <- train_edx %>%
    left_join(b_u, by="userId") %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_r, by="releaseyear") %>%
    group_by(unique_ans) %>%
    summarize(b_ru = mean(rating - mu - b_i - b_u - b_g - b_r), .groups = "keep")
  
  predicted_ratings <- test_edx %>%
    left_join(b_u, by="userId") %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_r, by="releaseyear") %>%
    left_join(b_ru, by="unique_ans") %>%
    mutate(pred = mu+b_i+b_u+b_g+b_r+b_ru) %>%
    pull(pred)
  
  rmse_uniqueans <- RMSE(test_edx$rating, predicted_ratings)
  rmse_results <- rmse_results %>% 
    add_row(method="+ Unique answers effect", RMSE=rmse_uniqueans)
}

rmse_results

##########################################################
# Regularization
##########################################################
###model training / testing
{
  lambdas <- seq(0, 10, 0.1)
  
  rmses_regularization <- sapply(lambdas, function(lambda){
    
    #Mean
    mu <- mean(train_edx$rating)
    
    #Movie effect
    b_i <- train_edx %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+lambda), .groups = "keep")
    
    #User effect
    b_u <- train_edx %>%
      left_join(b_i, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - mu - b_i)/(n()+lambda), .groups = "keep")
    
    #Genre combo effect
    b_g <- train_edx %>%
      left_join(b_u, by="userId") %>%
      left_join(b_i, by="movieId") %>%
      group_by(genres) %>%
      summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+lambda), .groups = "keep")
    
    #Release year effect
    b_r <- train_edx %>%
      left_join(b_u, by="userId") %>%
      left_join(b_i, by="movieId") %>%
      left_join(b_g, by="genres") %>%
      group_by(releaseyear) %>%
      summarize(b_r = sum(rating - mu - b_i - b_u - b_g)/(n()+lambda), .groups = "keep")
    
    #Unique rating effect
    b_ru <- train_edx %>%
      left_join(b_u, by="userId") %>%
      left_join(b_i, by="movieId") %>%
      left_join(b_g, by="genres") %>%
      left_join(b_r, by="releaseyear") %>%
      group_by(unique_ans) %>%
      summarize(b_ru = mean(rating - mu - b_i - b_u - b_g - b_r)/(n()+lambda), .groups = "keep")
    
    #Predicted ratings
    predicted_ratings <- test_edx %>%
      left_join(b_u, by="userId") %>%
      left_join(b_i, by="movieId") %>%
      left_join(b_g, by="genres") %>%
      left_join(b_r, by="releaseyear") %>%
      left_join(b_ru, by="unique_ans") %>%
      mutate(pred = mu+b_i+b_u+b_g+b_r+b_ru) %>%
      pull(pred)
    
    return(RMSE(test_edx$rating, predicted_ratings))
  })
  
  qplot(lambdas, rmses_regularization) +
    geom_hline(aes(yintercept = as.numeric(rmse_results[nrow(rmse_results),2])),
               color="Blue") +
    ggtitle("Lambda vs RMSE with regularization") +
    geom_point(aes(y=rmses_regularization[which.min(rmses_regularization)],
                   x=lambdas[which.min(rmses_regularization)]), color="red")
    
  lambda <- lambdas[which.min(rmses_regularization)]
  
  rmse_reg <- rmses_regularization[which.min(rmses_regularization)]
  
  rmse_results <- rmse_results %>% 
    add_row(method="All effects + Regularization", RMSE=rmse_reg)
}

##########################################################
# Results (Using whole edx and validation data sets)
##########################################################
###model training / predictions
{
  #Mean
  mu <- mean(edx$rating)
  
  #Movie effect
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda), .groups = "keep")
  
  #User effect
  b_u <- edx %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+lambda), .groups = "keep")
  
  #Genre combo effect
  b_g <- edx %>%
    left_join(b_u, by="userId") %>%
    left_join(b_i, by="movieId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+lambda), .groups = "keep")
  
  #Release year effect
  b_r <- edx %>%
    left_join(b_u, by="userId") %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_g, by="genres") %>%
    group_by(releaseyear) %>%
    summarize(b_r = sum(rating - mu - b_i - b_u - b_g)/(n()+lambda), .groups = "keep")
  
  #Unique rating effect
  b_ru <- edx %>%
    left_join(b_u, by="userId") %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_r, by="releaseyear") %>%
    group_by(unique_ans) %>%
    summarize(b_ru = mean(rating - mu - b_i - b_u - b_g - b_r)/(n()+lambda), .groups = "keep")
  
  #Predicted ratings
  predicted_ratings <- validation %>%
    left_join(b_u, by="userId") %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_r, by="releaseyear") %>%
    left_join(b_ru, by="unique_ans") %>%
    mutate(pred = mu+b_i+b_u+b_g+b_r+b_ru) %>%
    pull(pred)
  
  #Validation RMSE
  rmse_final <- RMSE(validation$rating, predicted_ratings)
  rmse_final
}

###checking values of predictions
max(predicted_ratings)
min(predicted_ratings)
sum(predicted_ratings < 0.5 | predicted_ratings > 5)
###percentage of impossible ratings
round(mean(predicted_ratings < 0.5 | predicted_ratings > 5)*100,3)

errors <- validation$rating - predicted_ratings
errors <- as.data.frame(errors)

mean(errors$errors)
sd(errors$errors)
