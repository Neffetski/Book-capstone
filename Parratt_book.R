#############################################################
# title: "Book Prediction Capstone Project"
# author: "Steffen Parratt"
# date: "3/4/2019"

#############################################################
# OUTLINE OF FILE:
#   Project Goal
#   R Packages Installation
#   Data Gathering
#   Data Exploration and Cleaning
#   Data Preparation
#   Model Selection
#   Parameter Optimization
#   Model Training and Evaluation
#   Results
#   Conclusions

#############################################################
# PROJECT GOAL
#
# The goal is to build a book recommendation system,
# similar in spirit to the MovieLens recommendation system.
#
# The metric of success is an RMSE improvement over the naive
# approach similar in size to the improvement seen in the 
# MovieLens project. 

#############################################################
# R PACKAGES INSTALLATIONS
#
# Install the following R packages

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(utils)) install.packages("utils", repos = "http://cran.us.r-project.org")

#############################################################
# DATA GATHERING
#   The original data set was posted on Kaggle.
#   https://www.kaggle.com/zygmunt/goodbooks-10k
#   
#   There are several kernals that are posted on Kaggle that
#   address this data set. The one by Philipp Spachtholz is
#   terrific, and I have borrowed ideas, observations and code
#   snippets from his notebook. Philipp uses RecommenderLab 
#   for his modeling, while I have used the approach we 
#   covered in class. Philipp's RMSE results are slightly
#   better than mine, although we are using different
#   data sets.

#   Since Philipp's notebook was published, a corrected and
#   greatly expanded data set was published on Github at
#   https://github.com/zygmuntz/goodbooks-10k.
#   Philipp's notebook does not analyze this updated data set.
 
#   Download and unzip the data files 
    dl <- tempfile()
    
    download.file("https://github.com/zygmuntz/goodbooks-10k/releases/download/v1.0/books.zip", dl)
    books <- read_csv(unzip(dl))
    
    download.file("https://github.com/zygmuntz/goodbooks-10k/releases/download/v1.0/ratings.zip",dl)
    ratings <- read_csv(unzip(dl))
    
    download.file("https://github.com/zygmuntz/goodbooks-10k/releases/download/v1.0/book_tags.zip", dl)
    book_tags <- read_csv(unzip(dl))
    
    download.file("https://github.com/zygmuntz/goodbooks-10k/releases/download/v1.0/tags.zip", dl)
    tags <- read_csv(unzip(dl))
    
    file.remove(dl)

#############################################################
# DATA EXPLORATION AND CLEANING
#   Understand what data is in each file.
#   Decide what data is useful to keep, and what to discard.
# 
# Ratings.csv
#   This file contains user ratings for all the books
#   Each row contains a book_id, user_id, and a rating
    head(ratings)
#           book_id user_id rating
#     1:       1     314      5
#     2:       1     439      3
#     3:       1     588      5

#   Let's look at the size of our file    
    cat("Number of ratings =", length(ratings$rating))
#     Number of ratings = 5,976,479
    cat("Number of users =", length(unique(ratings$user_id)))
#     Number of users = 53,424
    cat("Number of books =", length(unique(ratings$book_id)))
#     Number of books = 10,000    

#   Let's make sure there are no NA fields in our data
    ratings <- ratings %>% filter(!is.na(book_id) |
                                  !is.na(user_id) |
                                  !is.na(rating))
    
#   Check to ensure duplicate entries were removed. In this
#   case, the same rating given to the same book by the same
#   user. (This was a problem in the old data set.)
    duplicates <- ratings %>% group_by(user_id, book_id, rating) %>% 
      mutate(n=n()) %>% filter(n > 1)
    duplicates
    # A tibble: 0 x 4
    # Groups:   user_id, book_id, rating [0]
    # … with 4 variables: user_id <dbl>, book_id <dbl>, rating <dbl>, n <int>
    
#   There are no longer duplicate entries, so comment the code below
#   ratings <- unique(ratings)
    
#   Now let's look for a similar, but different, case, a user
#   giving multiple DIFFERENT ratings for the same book.
#   (This was also an issue with the previous data set.)
    duplicates <- ratings %>% group_by(user_id, book_id) %>% 
      mutate(n=n()) %>% filter(n > 1)
    duplicates
    # A tibble: 0 x 4
    # Groups:   user_id, book_id [0]
    # … with 4 variables: user_id <dbl>, book_id <dbl>, rating <dbl>, n <int>
    
#   This problem has been corrected, so comment the code below
#    ratings <- ratings %>% group_by(user_id, book_id) %>% 
#      mutate(n=n()) %>% filter(n == 1)
    
#   Let's do a check to make sure that all ratings
#   are between 1 and 5
    rating_errors <- ratings %>% filter(rating < 1 | rating > 5)
    rating_errors
    # A tibble: 0 x 3
    # … with 3 variables: user_id <dbl>, book_id <dbl>, rating <dbl>
    # OK, there are no rating errors
    
#   Now let's take a look at the distribution of ratings...
    ratings %>% 
      ggplot(aes(x = rating, fill = factor(rating))) +
      geom_bar(color = "grey") + 
      scale_fill_brewer(palette = "Blues") + 
      guides(fill = FALSE)
    
#   As you can see from the plot, the median rating is 4
    median(ratings$rating) # 4
    
#   And the mean is 3.92
    mean(ratings$rating) # 3.919866
    
#   Now let's look at the number of ratings per user
    ratings %>% 
      group_by(user_id) %>% 
      summarize(number_of_ratings_per_user = n()) %>% 
      ggplot(aes(number_of_ratings_per_user)) + 
      geom_bar(fill = "mediumblue", color = "grey")
    
    # We can see that the number of reviews per user ranges
    # from about 20 to 200
    
#   The median number is 111 reviews per user
    user_habits <- ratings %>% group_by(user_id) %>% summarize(number_of_ratings_per_user = n())
    median(user_habits$number_of_ratings_per_user) # 111
    
#   It's doubtful how useful it is to include the preferences
#   of a user who has read few books. Let's zoom into
#   the low end of the chart and determine where we should set
#   a cutoff.
    ratings %>% 
      group_by(user_id) %>% 
      summarize(number_of_ratings_per_user = n()) %>% 
      ggplot(aes(number_of_ratings_per_user)) + 
      geom_bar(fill = "mediumblue", color = "grey") + coord_cartesian(c(0, 50))
    
    # We can see that each user has rated at least 20 books

#   Similarly, let's look at the number of ratings per book
    ratings %>% 
      group_by(book_id) %>% 
      summarize(number_of_ratings_per_book = n()) %>% 
      ggplot(aes(number_of_ratings_per_book)) + 
      geom_bar(fill = "blue", color = "grey", width = 1)
    
    ratings_per_book <- ratings %>% group_by(book_id) %>% 
      summarize(number_of_ratings_per_book = n())

#   The median ratings per book is 248    
    cat("Median rating =",median(ratings_per_book$number_of_ratings_per_book))
      # Median rating = 248
    
#   And the mean is about 600
    cat("Mean rating =", mean(ratings_per_book$number_of_ratings_per_book))   
      # Mean rating = 597.6479
    
#   Let's zoom in to the low end of the range
    ratings %>% 
      group_by(book_id) %>% 
      summarize(number_of_ratings_per_book = n()) %>% 
      ggplot(aes(number_of_ratings_per_book)) + 
      geom_bar(fill = "blue", color = "grey", width = 1) + coord_cartesian(c(0, 100))
    
# There is only one book with fewer than 10 reviews
    few_ratings_books <- ratings_per_book %>% filter( number_of_ratings_per_book < 10 )
    few_ratings_books
    # A tibble: 1 x 2
    #     book_id number_of_ratings_per_book
    #  1    7803                          8
    
#   At this point we have a clean data set from the Ratings.csv
#   file, which we now understand pretty well

# Tags.csv
#   This file contains tag_names and tag_ids.
    head(tags)
    #     tag_id tag_name
    # 1:      0        -
    # 2:      1     --1-
    # 3:      2    --10-
    # 4:      3    --12-
    
#   Tag_names are similar in spirit to "genres".
#   Tag_ids are unique identifiers for each tag_name.

#   Users assign self-chosen tag_names to books, and these
#   tag names do not need to conform to any particular
#   standard. Let's look at a few random entries...
    tags[100,2]   # 02-folklore
    tags[500,2]   # 1997
    tags[1000,2]  # 3-mistakes
    tags[5000,2]  # book-lust-to-go
    
    cat("Number of tags =", length(tags$tag_id))
    #   Number of tags = 34,252
    cat("Number of unique tag_names =", length(unique(tags$tag_name)))
    #   Number of unique tag_names = 34,252
    cat("Number of NAs in tag_names =", sum(is.na(tags$tag_name)))
    #   Number of NAs in tag_names = 0
    
# Book_tags.csv
#   This file maps each tag_id in tags.csv to a 
#   goodreads_book_id. This id number maps to the
#   book_id the ratings.csv file
    head(book_tags)
    #     goodreads_book_id tag_id  count
    # 1:                 1  30574 167697
    # 2:                 1  11305  37174
    # 3:                 1  11557  34173

#   The file also contains a "count" field, which is a code
#   for descriptors that I do not have access to. Therefore
#   I do not use this field and drop it from the data set.
    book_tags <- subset(book_tags, select = -c(count))

#   Note that one book can have many different tags...
    book_tags
    #     A tibble: 999,912 x 2
    #     goodreads_book_id   tag_id
    #             1           30574
    #             1           11305
    #             1           11557 
    #             1           8717
    #             1           33114
    #             1           11743
    
    cat("Total rows of book_tags =", nrow(book_tags))
    #   Total rows of book_tags = 999,912
    cat("Number of unique tags =", length(unique(book_tags$tag_id)))
    #   Number of unique tags = 34,252
    cat("Number of unique goodread_book_id =", length(unique(book_tags$goodreads_book_id)))
    #   Number of unique goodread_book_id = 10,000
    cat("Number of NAs in tag_names =", sum(is.na(book_tags$tag_id) | 
                                            is.na(book_tags$goodreads_book_id)))
    #   Number of NAs = 0
    
    # In fact, there are approximately 999,900 tags associated
    # with 10,000 books, which means, on average, a book has about
    # 100 "genres" tags associated with it.  
    
#   Making Sense of Tags
    
#   There are 34,252 unique tag_name fields in both tag files that 
#   represent user chosen genres. To make sense of these, we impose a
#   genres classification on this user tag_names. I use a 
#   methodology presented by Philipp Spachtholz in his notebook.
    
#   Start with the goodbook's genres classification
    genres <- str_to_lower(c("Art", "Biography", "Business", "Chick Lit", "Children's", "Christian", 
                             "Classics", "Comics", "Contemporary", "Cookbooks", "Crime", "Ebooks", 
                             "Fantasy", "Fiction", "Gay and Lesbian", "Graphic Novels", 
                             "Historical Fiction", "History", "Horror", "Humor and Comedy", "Manga", 
                             "Memoir", "Music", "Mystery", "Nonfiction", "Paranormal", "Philosophy", 
                             "Poetry", "Psychology", "Religion", "Romance", "Science", "Science Fiction", 
                             "Self Help", "Suspense", "Spirituality", "Sports", "Thriller", "Travel", 
                             "Young Adult"))
    
#   And exclude overly general classifications
    exclude_genres <- c("fiction", "nonfiction", "ebooks", "contemporary")
    genres <- setdiff(genres, exclude_genres)
    
#   Select only those standard genres that appear in the tag_name fields
    available_genres <- genres[str_to_lower(genres) %in% tags$tag_name]

#   There are 27 available genres    
    length(available_genres) # 27
    
#   And then locate the corresponding tags    
    available_tags <- tags$tag_id[match(available_genres, tags$tag_name)]

#   Which should also be 27 in number     
    length(available_tags) # 27
    
#   Using the Book_tags, we can determine the prevalence of 
#   each genres in this collection of ratings...
    tmp <- book_tags %>% 
      filter(tag_id %in% available_tags) %>% 
      group_by(tag_id) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      mutate(sumN = sum(n), percentage = n / sumN) %>%
      arrange(-percentage) %>%
      left_join(tags, by = "tag_id")
    
    tmp %>% 
      ggplot(aes(reorder(tag_name, percentage), percentage, fill = percentage)) + 
      geom_bar(stat = "identity") + coord_flip() + 
      scale_fill_distiller(palette = 'Blues') + labs(y = 'Percentage', x = 'Genre')
    
#   If we look at our book_tags after filtering for availability
    available_book_tags <- book_tags %>% 
      filter(tag_id %in% available_tags)
    
    available_book_tags
    # A tibble: 37,153 x 2
    # goodreads_book_id tag_id
    #                 1  11305
    #                 1   7457
    #                 1  22973
    #                 1  20939
    #                 1  26138
    #                 2  11305
    #                 2  22973
    #                 2  20939
    #                 2  26138

# We see that the 37,000 tags are spread over
# about 10,000 books, or 3.7 tags per book, which
# which we can summarized below.
    available_book_tags <- book_tags %>% 
      filter(tag_id %in% available_tags) %>% 
      group_by(goodreads_book_id) %>%
      summarize(n = n())
    
    available_book_tags
    # A tibble: 9,863 x 2
    #   goodreads_book_id   n
    #                 1     5
    #                 2     4
    #                 3     4
    #                 5     4
    #                 6     4
    #                 8     5
    #                10     4
    #                11     3
    #                13     4
    #                21     4
    
#   About 1.4% of our books are excluded because they
#   have a genres not within our available set.
    
#   If we look at the tags for any particular, say goodreads_book_id
#   number 1 above, we can see that a book with several tags covers
#   a lot of general subjects.
    print(tags %>% filter(tag_id == 11305) %>% select(tag_name)) 
    # fantasy
    print(tags %>% filter(tag_id == 7457) %>% select(tag_name))
    # classics
    print(tags %>% filter(tag_id == 22973) %>% select(tag_name))
    # paranormal
    print(tags %>% filter(tag_id == 20939) %>% select(tag_name))
    # mystery   
    print(tags %>% filter(tag_id == 26138) %>% select(tag_name))
    # romance   
    
#   This book (# 1) has genres covering 5 of the 27 available
#   genres. Some books have up to a dozen of the two dozen genres.
#   We need to do more work to make this more useful.
#   I experimented a bit with algorithms to get each book down to 
#   one genre, by picking the least common genre, but not very
#   successfully
    
# Books.csv
#   This file contains the meta data for each book.
#   There are 10,000 rows, one for each book in our
#   data set.
    print(nrow(books)) # 10,000
    
#   As above, we will work through and explain each 
#   of the fields, removing those that provide no
#   expected benefit.
    
#   First, let's remove the ISBN and ISBN13 which
#   add no value to our modeling. 
    books <- subset(books, select = -c(isbn, isbn13))
    
#   Second, there are four book ids listed:
#   book_id = there are 10,000 entries, which match
#     the id numbers in the Ratings.csv file
#   goodreads_book_id = match the identifier in the
#     book_tags file
#   best_book_id = is the most popular edition of a
#     given book, generally this is the same as the
#     as the goodreads_book_id, except in 241 cases.
      print(nrow(books %>% filter(goodreads_book_id != best_book_id))) #241
#   work_id = refers to a book in the abstract sense.
#     That is, it represents a generic class of all
#     editions of a book.
      
#   Which id numbers should we use?
#     book_id ties this file to the ratings.csv file,
#     and so this will be our main id number for books.
#     goodreads_book_id ties this file to the book_tags.csv
#     so we will keep this id in the file for the time
#     being. We will discard the best_book_id and the
#     work_id.
      books <- subset(books, select = -c(best_book_id, work_id))
      
#   Related fields
#     Since we removed the work_id, we can also remove
#     the related column work_ratings_count.
      books <- subset(books, select = -c(work_ratings_count, 
                                         work_text_reviews_count))
#   Irrelevant fields
#     We have no use for the image_url nor the small_image_url.
      books <- subset(books, select = -c(image_url, small_image_url))
      
#     The file lists the books original_title and title,
#     about half of which changed over time.
      print(nrow(books %>% filter(original_title != title))) # 5,314
#     I chose to use the current title
      books <- subset(books, select = -c(original_title))
      
#   This file contains the ratings in each category (1-5), as well
#   as the average. Question -- do the ratings in this file match
#   the ratings in our ratings.csv file?
    
    # Pull the averages out of the books.csv file
    books_file_summary <- books %>% select(book_id, average_rating)
    
    # Calculate the averages out of the ratings.csv file
    ratings_file_summary <- ratings %>% group_by(book_id) %>%
      summarize(rating_file_average = mean(rating))
    
    # Join the two together
    ratings_comparison <- left_join(books_file_summary, 
                                    ratings_file_summary, by= "book_id")
    # Calculate the percent difference
    ratings_comparison <- ratings_comparison %>% 
      mutate(percent_diff = 100*(1-rating_file_average/average_rating))
 
    # Plot it
    ratings_comparison %>% 
      ggplot(aes(percent_diff)) + 
      geom_density(fill = "mediumblue", color = "grey") 
    
    # The mean difference in estimates is 2.5%
    mean(ratings_comparison$percent_diff) # 2.5
    
    # The standard deviation is 2.5%
    sd(ratings_comparison$percent_diff) # 2.5
    
#   These two rating averages are close. We will use the averages
#   reported in the books.csv file for our preliminary analysis,
#   but use the ratings in the ratings.csv file for our modeling.
#   We do not the ratings_1 through _5, so we remove them.
    books <- subset(books, select = -c(ratings_1,
                                       ratings_2,
                                       ratings_3,
                                       ratings_4,
                                       ratings_5))
    
#   Explanation of remaining fields:
#   books_count is how many editions of a book have been published
#   authors and original_publication_date are self-explanatory
#   ratings_count is the number of ratings
    
#   Which of these fields influence a books average rating?
#   I borrowed this helpful analysis from Philipps notebook
    tmp <- books %>% 
      select(one_of(c("books_count","original_publication_year",
                      "ratings_count", "average_rating"))) %>% as.matrix()
    
    corrplot(cor(tmp, use = 'pairwise.complete.obs'), type = "lower")
    
    # From which we can see a slight negative correlation with 
    # the number of editions and slight positive correlation with
    # the number of ratings.
    
    # We examine these further with the following plot from Philipp
    get_cor <- function(df){
      m <- cor(df$x,df$y, use="pairwise.complete.obs");
      eq <- substitute(italic(r) == cor, list(cor = format(m, digits = 2)))
      as.character(as.expression(eq));                 
    }
    
    books %>% 
      filter(ratings_count < 1e+5) %>% 
      ggplot(aes(ratings_count, average_rating)) + stat_bin_hex(bins = 50) + 
      scale_fill_distiller(palette = "Blues") + 
      stat_smooth(method = "lm", color = "blue", size = 2) +
      annotate("text", x = 85000, y = 2.7, 
               label = get_cor(data.frame(x = books$ratings_count, y = books$average_rating)), 
               parse = TRUE, color = "blue", size = 7)
    
    # And we see the correlation between ratings_count and
    # average rating is only 0.045 and not worth including in
    # our model.
    
    # Similarly, look at the correlation with the number of editions
    books %>% filter(books_count <= 500) %>% ggplot(aes(books_count, average_rating)) + 
      stat_bin_hex(bins = 50) + scale_fill_distiller(palette = "Blues") + 
      stat_smooth(method = "lm", color = "blue", size = 2) +
      annotate("text", x = 400, y = 2.7, 
               label = get_cor(data.frame(x = books$books_count, y = books$average_rating)), 
               parse = TRUE, color = "blue", size = 7)
    
    # original_publication_year has no impact, and so we remove 
    # that from our file as well.
    books <- subset(books, select = -c(original_publication_year))
    
    # Philipp shows that there is a small correlation betwen
    # the rating and number of authors and average rating (0.075)
    # but not enough to include in our model.
    
#############################################################
# DATA PREPARATION
# 
# After analyzing the files, we prepare the data for modeling.
    
# First we create the VALIDATION data set and set it aside
    
  set.seed(1)
  val_index <- createDataPartition(y = ratings$rating, times = 1, p = 0.1, list = FALSE)
  data_set <- ratings[-val_index,]
  validation_set <- ratings[val_index,]
    
# To ensure we do not include users and books in the test set 
# that do not appear in  the training set we remove those entries 
# with the semi_join function
  validation_set <- validation_set %>% 
    semi_join(data_set, by = "book_id") %>%
    semi_join(data_set, by = "user_id") 

  # We then partition the data_set into training and test sets
  set.seed(1)
  test_index <- createDataPartition(y = data_set$rating, times = 1, p = 0.2, list = FALSE)
  train_set <- data_set[-test_index,]
  test_set <- data_set[test_index,]

  # To ensure we do not include users and books in the test set 
  # that do not appear in  the training set we remove those entries 
  # with the semi_join function
  test_set <- test_set %>% 
    semi_join(train_set, by = "book_id") %>%
    semi_join(train_set, by = "user_id") 
  
  
#############################################################
# MODEL SELECTION

# We will test the accuracy of our model with the RMSE function
  RMSE <- function(predicted_ratings, true_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
  }

# Similar to the textbook, our first model predicts the same rating for all books 
# regardless of the user
  mu <- mean(train_set$rating)
  mu # 3.919892

# If we predict all unknown ratings with mu we obtain the following RMSE:
  naive_rmse <- RMSE(mu, test_set$rating)
  naive_rmse # 0.9911228

# Let's create a results table and add our first entry
  rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

# We augment our model by adding term b_i to represent the average 
# ranking for book i. b_i is the average least square estimate of the 
# difference between Y - mu for each book.

  book_avgs <- train_set %>% 
    group_by(book_id) %>% 
    summarize(b_i = mean(rating - mu))

# Let’s see how much our prediction improves once we add b_i to our model
  predicted_ratings <- mu + test_set %>% 
    left_join(book_avgs, by='book_id') %>% 
    pull(b_i)

  model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
  model_1_rmse # 0.9538607

  rmse_results <- bind_rows(rmse_results, 
                          tibble(method="Book Effect Model", RMSE = model_1_rmse))

# Now we augment our previous model by adding the term b_u to represent 
# the average ranking for user u.
  user_avgs <- train_set %>% 
    left_join(book_avgs, by='book_id') %>%
    group_by(user_id) %>%
    summarize(b_u = mean(rating - mu - b_i))

# We can now construct predictors and see how much the RMSE improves:
  predicted_ratings <- test_set %>% 
    left_join(book_avgs, by='book_id') %>%
    left_join(user_avgs, by='user_id') %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)

  model_2_rmse <- RMSE(predicted_ratings, test_set$rating) 
  model_2_rmse # 0.8584616
  rmse_results <- bind_rows(rmse_results, 
                            tibble(method="Book + User Effects Model", 
                                   RMSE = model_2_rmse))

#############################################################
# PARAMETER OPTIMIZATION: REGULARIZATION
#
# Now we try to improve our model by adding regularization
# We choose a parameter lambda that optimizes regularization

  lambdas <- seq(0, 10, 0.25)

  rmses <- sapply(lambdas, function(l){
  
    mu <- mean(train_set$rating)
  
    b_i <- train_set %>% 
      group_by(book_id) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
  
    b_u <- train_set %>% 
      left_join(b_i, by="book_id") %>%
      group_by(user_id) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
    predicted_ratings <- test_set %>% 
      left_join(b_i, by = "book_id") %>%
      left_join(b_u, by = "user_id") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
  
    return(RMSE(predicted_ratings, test_set$rating))
  })

  plot(lambdas, rmses)
  lambda <- lambdas[which.min(rmses)]
  lambda # 4.5
  rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized Book + User Effect Model",  
                                     RMSE = min(rmses)))
  rmse_results %>% knitr::kable()
  #  |method                               |      RMSE|
  #  |:------------------------------------|---------:|
  #  |Just the average                     | 0.9911228|
  #  |Book Effect Model                    | 0.9538607|
  #  |Book + User Effects Model            | 0.8584616|
  #  |Regularized Book + User Effect Model | 0.8581094|
  
  # From this table we can that regularization adds little value

#############################################################
# MODEL TRAINING and VALIDATION
#
# Using the model and analysis above, we repeat the analyis 
# for the full data_set and validation data set.
# Below is my FINAL MODEL for grading

  mu <- mean(data_set$rating)
  lambda <- 4.5

  b_i <- data_set %>% 
    group_by(book_id) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))

  b_u <- data_set %>% 
    left_join(b_i, by="book_id") %>%
    group_by(user_id) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

  predicted_ratings <- validation_set %>% 
    left_join(b_i, by = "book_id") %>%
    left_join(b_u, by = "user_id") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)

  final_results <- RMSE(predicted_ratings, validation_set$rating)

  rmse_results <- bind_rows(rmse_results,
                          tibble(method="Validation dataset",  
                                 RMSE = final_results))

#############################################################
# FINAL RESULTS

  rmse_results %>% knitr::kable()

# |method                               |      RMSE|
#  |:------------------------------------|---------:|
#  |Just the average                     | 0.9911228|
#  |Book Effect Model                    | 0.9538607|
#  |Book + User Effects Model            | 0.8584616|
#  |Regularized Book + User Effect Model | 0.8581094|
#  |Validation dataset                   | 0.8560672|

#  Our model includes a regularized book effect parameter and 
#  regularized user effect parameter. The resulting evaluated 
#  RMSE was 0.8560672, which is well below the target of 0.87750.
  
#############################################################
# CONCLUSIONS
#
# In this project I learned how to use Kaggle.com, which was a goal of mine. 
# I learned that there is a lot of work that needs to be done examining, 
# cleaning and constructing a new data set before even thinking about building a model. 
# I applied the methodologies we learned in class to a book recommendation system, 
# similar to the MovieLens project, and built and trained a model that produced a 
# result on the validation data set that was similar in improvement to my MovieLens 
# project. I also experimented with the RecommenderLab package in both my MovieLens 
# project and in this project, although I did not include this work in this project 
# writeup because I had a bug that kept crashing my model. (RecommenderLab worked a 
# little better on my MovieLens project.)

