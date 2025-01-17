library(stopwords)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidytext)
library(stringr)
library(textclean)
library(ngram)
library(tokenizers)
library(tidyverse)
library(sentimentr)
library(caret)
library(kernlab)
library(tm)
library(SnowballC)
library(hunspell)
library(data.table)
getwd()
# How frequently do certain words appear in the dataset 
# How frequently do certain pairs of words appear in the dataset, then can move onto triplets 
# Need to do some thinking: Build expectations about the dataset to recognise unexpected things 
# Without expectations, everything looks correct!
# -------------------------- High-level Expectations for EDA -------------------------------
# I expect stopwords to be more popular than normal words. 
# I except that news will be significantly different than blog and twitter, as it is more formal and less personal.
# I expect at least 60-70% of unique words sorted to cover 50% of all word instances in the language.
# I expect foreign words to be possible, however their influence will be expected and wont damage the model
# as many foreign words used regularly in English, such as 'ballet' or 'deja vu' express concepts and ideas.
# To increase the coverage, replacing words that have similar meanings with the more common instance can potentially
# reduce the word count to cover the same number of phrases.
# After some analysis, twitter posts are typically shorter than news and blog posts, therefore this is an assumption
# for the remainder of the analysis.

url <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'

# Create a temporary file to store the downloaded ZIP archive
temp_zip <- tempfile()

# Download the ZIP file from the web
download.file(url, temp_zip, mode = "wb") # mode = "wb" for binary files

# List the contents of the ZIP file
file_list <- unzip(temp_zip, list = TRUE)
print(file_list)

twitter_file <- file_list$Name[11]
news_file <- file_list$Name[12]
blog_file <- file_list$Name[13]

twitter_con <- unz(temp_zip, twitter_file)

news_con <- unz(temp_zip, news_file)

blog_con <- unz(temp_zip, blog_file)

# Partitioning the data

set.seed(54321)

# reading lines
twitter_lines <- readLines(twitter_con, skipNul=T)
news_lines <- readLines(news_con, skipNul=T)
blog_lines <- readLines(blog_con, skipNul=T)

sum(length(twitter_lines) + length(news_lines), length(blog_lines))
# Total Length = 2,400,000

sample_chunk = 100000

twitterSamp <- sample(twitter_lines, size=sample_chunk, replace=FALSE)
newsSamp <- sample(news_lines, size=sample_chunk, replace=FALSE)
blogSamp <- sample(blog_lines, size=sample_chunk, replace=FALSE)

# Combine the sampled data
combined_data <- c(twitter_lines, news_lines, blog_lines)


# Shuffle the combined data
# Ensure reproducibility
shuffled_data <- sample(combined_data, 100000)

# Define split ratios
train_ratio <- 0.7
valid_ratio <- 0.15
test_ratio <- 0.15

# Calculate split indices
total_length <- length(shuffled_data)
train_index <- floor(total_length * train_ratio)
valid_index <- train_index + floor(total_length * valid_ratio)

# Create training, validation, and test sets
train_set <- shuffled_data[1:train_index]
valid_set <- shuffled_data[(train_index + 1):valid_index]
test_set <- shuffled_data[(valid_index + 1):total_length]

# Check sizes of the subsets
cat("Training Set Size: ", length(train_set), "\n")
cat("Validation Set Size: ", length(valid_set), "\n")
cat("Test Set Size: ", length(test_set), "\n")

close(twitter_con)
close(news_con)
close(blog_con)
unlink(temp_zip)

twitter_colour <- "#1DA1F2"
news_colour <- "#333333"
blog_colour <- "#21759B"
      

# Splitting the data
# Twitter
twitter_indices <- sample(seq_along(twitter_lines), size = 0.7 * length(twitter_lines))
twitter_remaining_indices <- setdiff(seq_along(twitter_lines), twitter_indices)
twitter_validation_indices <- sample(twitter_remaining_indices, size = 0.5 * length(twitter_remaining_indices))
twitter_test_indices <- setdiff(twitter_remaining_indices, twitter_validation_indices)

# Split the data
twitter_train <- twitter_lines[twitter_indices]
twitter_valid <- twitter_lines[twitter_validation_indices]
twitter_test <- twitter_lines[twitter_test_indices]

# News
news_indices <- sample(seq_along(news_lines), size = 0.7 * length(news_lines))
news_remaining_indices <- setdiff(seq_along(news_lines), news_indices)
news_validation_indices <- sample(news_remaining_indices, size = 0.5 * length(news_remaining_indices))
news_test_indices <- setdiff(news_remaining_indices, news_validation_indices)

# Split the data
news_train <- news_lines[news_indices]
news_valid <- news_lines[news_validation_indices]
news_test <- news_lines[news_test_indices]

# Blog
blog_indices <- sample(seq_along(blog_lines), size = 0.7 * length(blog_lines))
blog_remaining_indices <- setdiff(seq_along(blog_lines), blog_indices)
blog_validation_indices <- sample(blog_remaining_indices, size = 0.5 * length(blog_remaining_indices))
blog_test_indices <- setdiff(blog_remaining_indices, blog_validation_indices)


# Split the data
blog_train <- blog_lines[blog_indices]
blog_valid <- blog_lines[blog_validation_indices]
blog_test <- blog_lines[blog_test_indices]

# Sample the training data for analysis purposes
# sampling lines
length(twitter_train)
length(news_train)
length(blog_train)
# Total Length = 2988773

sample_chunk = 100000

twitterSamp <- sample(twitter_train, size=sample_chunk, replace=FALSE)
newsSamp <- sample(news_train, size=sample_chunk, replace=FALSE)
blogSamp <- sample(blog_train, size=sample_chunk, replace=FALSE)

close(twitter_con)
close(news_con)
unlink(temp_zip)

twitter_colour <- "#1DA1F2"
news_colour <- "#333333"
blog_colour <- "#21759B"
  
# ---------------------------------- TOKENIZATION ---------------------------------------------- 


# Profanity filtering - removing profanity and other words you do not want to predict.
# I expect twitter to be more laden with profanity than news or blogs, as this is a personal site. 
profanity_list <- unique(c(lexicon::profanity_alvarez, 
                           lexicon::profanity_arr_bad, 
                           lexicon::profanity_arr_bad, 
                           lexicon::profanity_banned, 
                           lexicon::profanity_zac_anger))


sum(profanity(twitterSamp, profanity_list = profanity_list)$profanity)/length(twitterSamp)
# 0.0234%
sum(profanity(newsSamp, profanity_list = profanity_list)$profanity)/length(newsSamp)
# 0.01201%
sum(profanity(blogSamp, profanity_list = profanity_list)$profanity)/length(blogSamp)
# 0.01700%

# While news and blog posts had a higher count of profanity, the total percentage is smaller when compared to twitter.
# This likely accounts for the respective lengths and sizes of posts for each group. 

# Now I will first tokenize and transform the case of these texts to lower before removing profanity

# Save lower-case version of each text
twitter_lower <- tolower(twitterSamp)
news_lower <- tolower(newsSamp)
blog_lower <- tolower(blogSamp)

# Removing punctuation from each text
twitter_punc <- removePunctuation(twitter_lower)
news_punc <- removePunctuation(news_lower)
blog_punc <- removePunctuation(blog_lower)

# Cleaning the lists of profanity 
clean_tweet <- lapply(twitter_punc, function(words) setdiff(words, unique(profanity_list)))
clean_news <- lapply(news_punc, function(words) setdiff(words, unique(profanity_list)))
clean_blog <- lapply(blog_punc, function(words) setdiff(words, unique(profanity_list)))


# Tokenizing the corpora's
clean_tweet <- tokenize_words(twitter_punc)
clean_news <- tokenize_words(news_punc)
clean_blog <- tokenize_words(blog_punc)
# Lists have been tokenized and profanity has been removed. Now, some preliminary analysis will be done on 
# the distribution of words and word frequencies, while also carrying out lemmatization, stopwords removal and 
# correcting mispelled words. 

# ----------------------------------------------- EDA ---------------------------------------------------------

# Count the number of words appearing in each text
# Expectations
# 1. I expect that again blog posts and news articles will have more words per sentence than twitter posts.
# 2. I expect there to be 3x to 5x more words in a news article and blog than a twitter post. 

par(mfrow=c(3,1))
hist(sapply(clean_tweet, length), main="Word distribution for Twitter Posts", xlab="Number of Words", col="#1DA1F2")
# Twitter posts contain between 3-10 most frequently and range between 0 and 30 words.
hist(sapply(clean_news, length), main="Word distribution for News Articles", xlab="Number of Words", col="#333333")
# News articles contain between 20-30 words most frequently, with some containing as many as 150.
hist(sapply(clean_blog, length), main="Word distribution for Blog posts", xlab="Number of Words", col="#21759B")
# Blog posts contain at between 0-50 words most frequently, with some containing as many as 350.

# It is correct that on average, news articles and blog posts have more words than twitter posts. Interestingly,
# The distribution is similar to the respective distributions for the number of characters. This may indicate
# that while the news and blog posts are longer, the words they use are similar.


# Average word length
# Expectations
# 1. Average word length will be 4-5 characters, which accounts for stop words and potential longer words.  

mean_word_length <- function(tokenized_word) {
  # Getting the length of each string
  word_lengths <- lapply(tokenized_word, str_length)
  
  # Get the average word length
  avg_length <- lapply(word_lengths, mean)
  
  # Return the avg_length
  return(avg_length)
}

par(mfrow=c(3,1))
hist(unlist(mean_word_length(clean_tweet)), main="Avg word length for Twitter Posts", xlab="Number of characters", col="#1DA1F2")
hist(unlist(mean_word_length(clean_news)), main="Avg word length for News Articles", xlab="Number of characters", col="#333333")
hist(unlist(mean_word_length(clean_blog)), main="Avg word length for Blog Posts", xlab="Number of characters", col="#21759B")

# Between 4-5 was correct, with news articles and blog posts having only slightly higher average word length 
# When compared to twitter posts, which contains a distribution of words less than 4 characters. Blogs has a similar
# occurrence, where there are more lower length words than higher length words, while news
# contains a high distribution of words greater than 5 in character length. 


# Investigating stop words
# 1. I expect stopwords to be as popular in each of the three posts in terms of percentages, but with higher counts
# in news and blogs. The types of stopwords used will be potentially different. For example, news may be more
# general using "we" while twitter and blogs may use "I" or "me". 

stop_words <- data.frame(word=stopwords("en", source="smart"), stringsAsFactors=FALSE)

# Stopwords count for each dataset
count_total_stopwords <- function(tokenized_list) {
  # Convert the tokenized list to a dataframe
  tokenized_df <- data.frame(word = unlist(tokenized_list), stringsAsFactors = FALSE)
  
  # Count total stop words
  stopword_count <- tokenized_df %>%
    inner_join(stop_words, by = "word") %>%
    tally() %>%
    pull(n) # Return the total count as a single number
  
  return(stopword_count)
}

# Count stopwords in each dataset
twitter_count <- count_total_stopwords(clean_tweet)
news_count <- count_total_stopwords(clean_news)
blog_count <- count_total_stopwords(clean_blog)

# Combine the results into a single data frame
stopword_summary <- data.frame(
  dataset = c("Twitter", "News", "Blog"),
  total_stopwords = c(twitter_count, news_count, blog_count)
)

# Visualize with a bar chart
ggplot(stopword_summary, aes(x = dataset, y = total_stopwords, fill = dataset)) +
  geom_col() + # Bar chart
  labs(
    title = "Comparison of Total Stop Words Across Datasets",
    x = "Dataset",
    y = "Total Stop Words"
  ) +
  theme_minimal() + # Apply a clean theme
  theme(legend.position = "none") # Remove legend

# Out of a sample of 100,000 observations from each data-set, Blogs has more stopwords than news, 
# with a total of 2.5m compared with around 1.6m for news, which is significantly more than twitter, 
# which has around 600k. This is interesting, as blogs are typically longer
# than news articles, however, it makes me question the writing style and quality, and if the difference
# in stop-words is proportional to the respective post length. 

# Distribution of stop words 

count_stopwords <- function(tokenized_list){
  # Turn the tokenized list in a dataframe
  tokenized_df <- data.frame(word = unlist(tokenized_list), stringsAsFactors=FALSE)
  
  # Filter for stop words
    tokenized_df %>%
      inner_join(stop_words, by="word") %>%
      count(word, sort = TRUE)
}

twitter_stopwords <- count_stopwords(clean_tweet)
news_stopwords <- count_stopwords(clean_news)
blog_stopwords <- count_stopwords(clean_blog)

par(mfrow=c(3,1))
# Twitter: Visualize with a bar chart
twitter_stopwords %>%
  slice_max(n, n = 10) %>% # Select the top 10 most frequent stop words
  ggplot(aes(x = reorder(word, n), y = n)) + # Reorder words based on count
  geom_col(fill = twitter_colour) + # Bar chart with blue bars
  coord_flip() + # Flip coordinates for horizontal bars
  labs(
    title = "Top 10 Most Frequent Stopwords in Twitter Dataset",
    x = "Stop Words",
    y = "Count"
  ) +
  theme_minimal()

# News: Visualize with a bar chart
news_stopwords %>%
  slice_max(n, n = 10) %>% # Select the top 10 most frequent stop words
  ggplot(aes(x = reorder(word, n), y = n)) + # Reorder words based on count
  geom_col(fill = news_colour) + # Bar chart with blue bars
  coord_flip() + # Flip coordinates for horizontal bars
  labs(
    title = "Top 10 Most Frequent Stopwords in news Dataset",
    x = "Stop Words",
    y = "Count"
  ) +
  theme_minimal()

# Blog: Visualize with a bar chart
blog_stopwords %>%
  slice_max(n, n = 10) %>% # Select the top 10 most frequent stop words
  ggplot(aes(x = reorder(word, n), y = n)) + # Reorder words based on count
  geom_col(fill = blog_colour) + # Bar chart with blue bars
  coord_flip() + # Flip coordinates for horizontal bars
  labs(
    title = "Top 10 Most Frequent Stopwords in Blog Dataset",
    x = "Stop Words",
    y = "Count"
  ) +
  theme_minimal()

# The three different groups have the same number one stopword -- "the" -- with some other popular stopwords like 
# "and", "to" and "a". Twitter's third most popular stopword is "I", and its fifth most popular is "you". 
# However, for blog "I" is the 7th most popular stopword. Compare this to news, which does not contain instances
# of "I" or "you" in its top ten most popular stopwords. My expectation was correct, that it is not as 
# prevalent for news articles to use personal language. Having this variation is good for the model, as the majority
# of mobile users will be speaking from a personal perspective. 



# Counting occurrences of unique words
# 1. I expect that twitter will have more personal and emotional unique words, whereas news will be more objective
# and factual. Blog posts will lie somewhere in between.

count_unique_words <- function(tokenized_list){
  # Turn the tokenized list in a dataframe
  tokenized_df <- data.frame(word = unlist(tokenized_list), stringsAsFactors=FALSE)
  
  # Filter for stop words
  tokenized_df %>%
    anti_join(stop_words, by="word") %>%
    count(word, sort = TRUE)
}

twitter_unique <- count_unique_words(clean_tweet)
news_unique <- count_unique_words(clean_news)
blog_unique <- count_unique_words(clean_blog)


twitter_unique %>%
  slice_max(n, n = 10) %>% # Select the top 10 most frequent stop words
  ggplot(aes(x = reorder(word, n), y = n)) + # Reorder words based on count
  geom_col(fill = twitter_colour) + # Bar chart with blue bars
  coord_flip() + # Flip coordinates for horizontal bars
  labs(
    title = "Top 10 Most Frequent Unique Words in Twitter Dataset",
    x = "Unique Words",
    y = "Count"
  ) +
  theme_minimal()

news_unique %>%
  slice_max(n, n = 10) %>% # Select the top 10 most frequent stop words
  ggplot(aes(x = reorder(word, n), y = n)) + # Reorder words based on count
  geom_col(fill = news_colour) + # Bar chart with blue bars
  coord_flip() + # Flip coordinates for horizontal bars
  labs(
    title = "Top 10 Most Frequent Unique Words in News Dataset",
    x = "Unique Words",
    y = "Count"
  ) +
  theme_minimal()

blog_unique %>%
  slice_max(n, n = 10) %>% # Select the top 10 most frequent stop words
  ggplot(aes(x = reorder(word, n), y = n)) + # Reorder words based on count
  geom_col(fill = blog_colour) + # Bar chart with blue bars
  coord_flip() + # Flip coordinates for horizontal bars
  labs(
    title = "Top 10 Most Frequent Unique Words in Blog Dataset",
    x = "Unique Words",
    y = "Count"
  ) +
  theme_minimal()

# The expectation holds true, as the twitter data-sets top four words are "love", "good", "day" and "rt" 
# (potentially: "retweet"), while the news data-set is "year", "time", "state" and "years", with "people" and 
# "back" fifth and sixth respectively. Finally, the blogs data-set contained a mix of both, with "time" as its top word, 
# with "back", "make" and "people" being second, third, and fourth respectively. While this is more similar to the 
# distribution of words in the news articles, it does contain emotional words such as "good" in fifth and "love"
# in seventh. This indicates that there is more personal messaging in blogs than news, but not as much as in the 
# twitter data-set. 


# ----------------- Building the model ------------------------------
# Taking one large corpus, cleaning it and then creating the model
tweet_clean <- lapply(twitter_train, function(words) setdiff(words, unique(profanity_list)))
news_clean <- lapply(news_train, function(words) setdiff(words, unique(profanity_list)))
blog_clean <- lapply(blog_train, function(words) setdiff(words, unique(profanity_list)))

corpora <- c(tweet_clean, 
            news_clean,  
            blog_clean)

corpus <- tolower(corpora)

randomise_order <- sample(corpus)
head(corpus)
head(randomise_order)

n_parts <- 10
part_size <- ceiling(length(randomise_order) / n_parts)

# Create indices for splitting
split_corpus <- cut(seq_along(randomise_order), breaks = n_parts, labels = FALSE)

# Split the vector into 5 parts
split_parts <- split(randomise_order, split_corpus)

# Print each part
length(split_parts[[1]])

# Split into 5 corpus
corpus_1 <- split_parts[[1]]
corpus_2 <- split_parts[[2]]
corpus_3 <- split_parts[[3]]
corpus_4 <- split_parts[[4]]
corpus_5 <- split_parts[[5]]
corpus_6 <- split_parts[[6]]
corpus_7 <- split_parts[[7]]
corpus_8 <- split_parts[[8]]
corpus_9 <- split_parts[[9]]
corpus_10 <- split_parts[[10]]

# Incrementally increasing the size of the data that the n-gram model is trained on.
# N-gram Generation

# Testing 1/5th of the entire data-set for performance
join_corpus_1 <- concatenate(corpus_1)
unigram_1 <- ngram(join_corpus_1, n=1, sep = ".!,/ ")
bigram_1 <- ngram(join_corpus_1, n=2, sep = ".!,/ ")
trigram_1 <- ngram(join_corpus_1, n=3, sep = ".!,/ ")

# Probability estimation
# unigrams
uni_1_probs <- get.phrasetable(unigram_1)
head(uni_1_probs, 10)

# bigrams
bi_1_probs <- get.phrasetable(bigram_1)
head(bi_1_probs, 10)

# trigrams
tri_1_probs <- get.phrasetable(trigram_1)
head(tri_1_probs, 20)

# Convert to data.table
unigram_probs1 <- data.table(uni_1_probs)
bigram_probs1 <- data.table(bi_1_probs)
trigram_probs1 <- data.table(tri_1_probs)

# Precompute context and next_word for each n-gram table
create_phrasetable <- function(data, n) {
  data[, `:=`(
    context = word(ngrams, 1, n - 1),
    next_word = word(ngrams, n)
  )]
  return(data[, .(context, next_word, freq, prop)])
}

phrase_2 <- create_phrasetable(unigram_probs1, 1)
phrase_3 <- create_phrasetable(bigram_probs1, 2)
phrase_4 <- create_phrasetable(trigram_probs1, 3)


# Combine all phrases into one table
phrasetable <- rbindlist(list(phrase_2, phrase_3, phrase_4))
setorder(phrasetable, -freq)


# Predicting the next word
predict_next_word <- function(context, phrasetable) {
  context <- tolower(context)
  # Back-off loop
  while (nchar(context) > 0) {
    # Check if context exists in phrasetable
    result <- phrasetable[context == ..context, .(next_word)]
    if (nrow(result) > 0) {
      return(result$next_word[1]) # Return the most frequent next word
    }
    # Perform back-off by removing the last word
    context <- word(context, 1, str_count(context, "\\S+") - 1)
  }
  return(NA) # Return NA if no prediction is found
}

rm(news_bi_texts, filtered_bi, filtered_blog, filtered_four, filtered_text, filtered_tri, 
   combined_bi, combined_four, combined_text, combined_tri, bigram_blog, bigram_blog_probs,
   bigram_news, bigram_news_probs, bigrams_news_ten, bigram_probabilities, bigram_tweet,
   bigram_tweet_probs, blog_bi_list, blog_df, corpus_bi, corpus_four, corpus_tokenized, corpus_tri,
   misspelled, misspelled_proc, n_blog, n_news, n_tweet, news_bi_list, news_df, news_tri_list, )

# Potential removal
# rm(trigram_values, tweet_bi_texts, tweet_tri_texts, tweet_uni_texts, quad_gram_values, twitter_indices, 
#   twitter_remaining_indices, twitter_validation_indices, twitter_test_indices, news_indices,
#   news_remaining_indicies, news_validation_indices, news_test_indices, blog_indicies, blog_remaining_indices,
#  blog_validation_indices, blog_test_indices)


# Evaluation


# Prediction


# Model Refinement