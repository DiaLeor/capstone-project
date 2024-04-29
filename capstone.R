
# Introduction ------------------------------------------------------------

# You will use the following code to generate your datasets. Develop your
# algorithm using the edx set. For a final test of your final algorithm,
# predict movie ratings in the final_holdout_test set as if they were unknown.
# RMSE will be used to evaluate how close your predictions are to the true
# values in the final_holdout_test set.

# Important: The final_holdout_test data should NOT be used for training,
# developing, or selecting your algorithm and it should ONLY be used for
# evaluating the RMSE of your final algorithm. The final_holdout_test set
# should only be used at the end of your project with your final model. It
# may not be used to test the RMSE of multiple models during model development.
# You should split the edx data into separate training and test sets and/or
# use cross-validation to design and test your algorithm.

# Also remember that by accessing this site, you are agreeing to the terms of
# the edX Honor Code. This means you are expected to submit your own work and
# can be removed from the course for substituting another student's work as
# your own.

# Create edx and final_holdout_test sets ----------------------------------
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = NULL)

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
set.seed(1) # if using R 3.6 or later
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

# MovieLens Instructions --------------------------------------------------

# The submission for the MovieLens project will be three files: a report in the
# form of an Rmd file, a report in the form of a PDF document knit from your
# Rmd file, and an R script that generates your predicted movie ratings and
# calculates RMSE. The R script should contain all of the code and comments
# for your project. Your grade for the project will be based on two factors:
    
    # 1. Your report and script (75%)

    # 2. The RMSE returned by testing your algorithm on the
    # final_holdout_test set (25%)

# Note that to receive full marks on this project, you may not simply copy code
# from other courses in the course series and be done with your analysis. Your
# work on this project needs to build on code that is already provided.

# Please note that once you submit your project, you will not be able to make
# changes to your submission.

# Report and Script (75%) -------------------------------------------------

# Your report and script will be graded by your peers, based on a rubric
# defined by the course staff. Each submission will be graded by three peers
# and the median grade will be awarded. To receive your grade, you must
# review and grade the submissions of five of your fellow learners after
# submitting your own. This will give you the chance to learn from your peers.

# Please pay attention to the due dates listed! The project submission is
# due before the end of the course to allow time for peer grading. Also
# note that you must grade the reports of your peers by the course close
# date in order to receive your grade.

# RMSE (25%) --------------------------------------------------------------

# Your movie rating predictions will be compared to the true ratings in the
# final_holdout_test set using RMSE. Be sure that your report includes the
# RMSE and that your R script outputs the RMSE.

# Note that to receive full marks on this project, you may not simply copy
# code from other courses in the course series and be done with your analysis.
# Your work on this project needs to build on code that is already provided.

# IMPORTANT: Make sure you do NOT use the final_holdout_test set to train
# your algorithm. The final hold-out test set should ONLY be used to test
# your final algorithm. The final hold-out test set should only be used at
# the end of your project with your final model. It may not be used to test
# the RMSE of multiple models during model development. You should split
# the edx data into a training and test set and/or use cross-validation.

# Honor Code --------------------------------------------------------------

# You are welcome to discuss your project with others, but all submitted work
# must be your own. Your participation in this course is governed by the
# terms of the edX Honor Code. If your project is found to violate the terms
# of the honor code, you will receive a zero on the project, may be unenrolled
# from the course, and will be ineligible for a certificate.

# Project Due Date --------------------------------------------------------

# Submissions for the Movielens project are due one week before course close,
# on December 11, 2024, at 23:59 UTC. This allows time for peer grading to
# occur! Peer grades are due at course close, on December 18, 2024, at
# 23:59 UTC.

# Peer Feedback -----------------------------------------------------------

# You are strongly encouraged to give your peers thoughtful, specific written
# feedback in addition to the numerical grades in the rubic. Think about the
# type of feedback that would help you improve your work and offer that type
# of feedback to your fellow learners.

# If you feel your report was not fairly graded by your peers, you may report
# it in the discussion forum to ask for staff review of the report.




