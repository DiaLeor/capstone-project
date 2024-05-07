# Project Overview --------------------------------------------------------
# Goal: to train a machine learning algorithm using the inputs in one subset
# to predict movie ratings in the validation set.

# I will use the following code to generate my datasets. I will develop my
# algorithm using the edx set. For a final test of my final algorithm, I will
# predict movie ratings in the final_holdout_test set as if they were unknown.
# RMSE will be used to evaluate how close my predictions are to the true
# values in the final_holdout_test set.

# It's important to note that the final_holdout_test data will NOT be used
# for training, developing, or selecting my algorithm and it will ONLY be
# used for evaluating the RMSE of my final algorithm. The final_holdout_test
# set will only be used at the end of my project with my final model. It
# will not be used to test the RMSE of multiple models during model development.
# I will split the edx data into separate training and test sets and/or
# use cross-validation to design and test my algorithm.

# By accessing this site, I am agreeing to the terms of the edX Honor Code.
# This means I am expected to submit my own work and can be removed from the
# course for substituting another student's work as my own.

# Create edx and final_holdout_test sets ----------------------------------
# The following code has been provided to all learners to kickstart their
# capstone project.

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = NULL) # the original code provided called for
# options(timeout = 120). I have disregarded the timeout, as it took
# longer than 120 for my computer to download the .zip file below

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Data Exploration and Preprocessing --------------------------------------

# Explore the structure of the dataset
str(edx)
head(edx)

# Check for missing values in the dataset
sum(is.na(edx)) # No NA values found

# Check for duplicate ratings by combining 'userId' and 'movieId' columns
paste(edx$userId, edx$movieId, sep = "-") %>%
  duplicated() %>% sum() # No duplicates found

# I'm not sure whether the timestamp is relevant to what I'm trying to achieve.
# Convert timestamp to POSIXct
edx$timestamp <- as.POSIXct(edx$timestamp, origin = "1970-01-01", tz = "UTC")

# Aggregate data by year
ratings_yearly <- edx %>%
  mutate(year = format(timestamp, "%Y")) %>%
  group_by(year) %>%
  summarise(total_ratings = n())

# Plot yearly ratings
ggplot(ratings_yearly, aes(x = year, y = total_ratings)) +
  geom_point() +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Total Ratings")

# A good research question:
  # is an actual question, not a statement.
  # is open-ended
  # grounded in theory, prior research, or practice
  # not too simple, not too complex

# Prasing a question prevents you from being biased toward what you expect to
# see after you analyze the data and set some more neutral tone that allows your
# skillful analysis of the data to reveal evidence of an answer. In general,
# questions that begin with "why" are not good research questions because they
# tend to be too complex and opinion based.

# How do user preferences vary across different genres?

# We start by posing the question we're trying to answer in terms of the
# subject matter, not using data or statistical jargon. From there, we come
# up with a suite of simpler, more closed ended data questions that we can
# answer through exploring, visualizing, and analyzing the data.

# Question board: we start at the top with our open-ended, grounded, not-too-
# simple-not-too-complex research question. We can then break this question
# down into a series of more specific, closed-ended questions. Some of these
# questions, we may not be able to answer if there are limitations to the data
# available to us. Other data questions may lead to additional data questions
# or more specific research questions that could be addressed among a smaller
# team or at a different time. If you find that the process of translating a
# research question into data questions is too hard, it's important to take a
# step back and evaluate if your research question is good enough or it needs
# to be refined:
  
# Rubric:
  # Does the question strike the right balance between simple and complex?
  # Will it take more than a simple data analysis or a single visualization
  # to answer the question?
  # Do we have access to the right amount and the right types of data to
  # sufficiently answer the question?
  # Will it be clear when we've sufficiently answered the question?

# Thinking through your research question helps to storyboard the process to
# collecting the right data, performing the right analysis, creating the right
# visualizations, and telling the right story.