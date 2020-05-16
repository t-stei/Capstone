if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

#I included the data set loading. If you already have edx & validation set loaded, you can skip this part.
#Important: Code was written on R 3.6.3
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################

#Here we add an extra column containing the timestamp variable in a date format
edx <- mutate(edx, date=as_datetime(timestamp))
#Mutating the date variable to week, so it is easier for further analyzing
edx <- mutate(edx, date = round_date(date, unit = "week"))

#Creating Mu from entire edx data set
mu <- mean(edx$rating)
#Creating b_i from entire edx data set
movie_avgs <- edx %>% group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
#Creating b_u from entire edx data set
user_avgs <- edx %>% 
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating- b_i -mu))
#Creating b_g from entire edx data set
genre_avgs <- edx %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))
#Creating b_d from entire edx data set
date_avgs <- edx %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(genre_avgs, by="genres") %>%
  group_by(date) %>%
  summarize(b_d = mean(rating - mu - b_i - b_u - b_g))

#Adding the b_i, b_u and b_g variables to the validation data frame
validation <- validation %>%
  mutate(date=as_datetime(timestamp)) %>%
  mutate(date = round_date(date, unit = "week")) %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(genre_avgs, by="genres") %>%
  left_join(date_avgs, by="date")

#Replacing NA Values in validation data frame
validation$b_i[is.na(validation$b_i)] <- 0
validation$b_u[is.na(validation$b_u)] <- 0
validation$b_g[is.na(validation$b_g)] <- 0
validation$b_d[is.na(validation$b_d)] <- 0

#Doing our Prediction
prediction <- validation %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
  .$pred

#Calculating RMSE
final_RMSE <- RMSE(validation$rating, prediction)

#Adding our final RMSE to the table
rmse_results <- tibble(method="Final Result",  
                                 RMSE = final_RMSE)
#Printing our RMSE
rmse_results %>% knitr::kable()




