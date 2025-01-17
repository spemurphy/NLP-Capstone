library(dplyr)
library(stringr)

n_tweet <- lapply(twitterSamp, function(words) setdiff(words, unique(profanity_list)))
n_news <- lapply(newsSamp, function(words) setdiff(words, unique(profanity_list)))
n_blog<- lapply(blogSamp, function(words) setdiff(words, unique(profanity_list)))

tweet_lower <- tolower(n_tweet)
news_lower <- tolower(n_news)
blog_lower <- tolower(n_blog)

## Cleaning test
test_corp <- lapply(test_set, function(words) setdiff(words, unique(profanity_list)))
test_lower <- tolower(test_corp)


## ------------------ combinining --------------------------
# concatenate characters 
corpus <- c(tweet_lower, blog_lower, news_lower)


## ----------- Creating ngram model ---------------------------
## ------ bigram --------------
# Filtering out words less than 2
corpus_bi <- tibble(text = corpus) %>%
  filter(str_count(text, "\\S+") >= 2)

filtered_bi <- corpus_bi$text

combined_bi <- paste(filtered_bi, collapse = " ")

# Tokenize into bigrams for the entire text
bigrams_text <- ngram(unlist(combined_bi), n = 2, sep = ".!,/ ")
bigram_values <- get.ngrams(bigrams_text)

## ---------- trigram------------
# Filtering out words less than 3
corpus_tri <- tibble(text = corpus) %>%
  filter(str_count(text, "\\S+") >= 3)

filtered_tri <- corpus_tri$text

combined_tri <- paste(filtered_tri, collapse = " ")

# Tokenize into trigrams for the entire text
trigrams_text <- ngram(combined_tri, n = 3, sep = ".!,/ ")
trigram_values <- get.ngrams(trigrams_text)


## ----- guadgram --------
# Filtering out words less than 4
corpus_four <- tibble(text = corpus) %>%
  filter(str_count(text, "\\S+") >= 4)

filtered_four <- corpus_four$text

combined_four <- paste(filtered_four, collapse = " ")

# Tokenize into trigrams for the entire text
quad_grams_text <- ngram(combined_four, n = 4, sep = ".!,/ ")
quad_gram_values <- get.ngrams(quad_grams_text)

## ----- pentagram --------
# Filtering out words less than 4
corpus_five <- tibble(text = corpus) %>%
  filter(str_count(text, "\\S+") >= 5)

filtered_five <- corpus_five$text

combined_five <- paste(filtered_five, collapse = " ")

# Tokenize into trigrams for the entire text
five_grams_text <- ngram(combined_five, n = 5, sep = ".!,/ ")
five_gram_values <- get.ngrams(five_grams_text)
## --------- phrasetables ---------------
# Calculate probabilities for bigrams
bigram_probabilities <- get.phrasetable(bigrams_text)
cat("\nBigram Probabilities:\n")
print(head(bigram_probabilities,20))

# Calculate probabilities for trigrams
trigram_probabilities <- get.phrasetable(trigrams_text)
cat("\nTrigram Probabilities:\n")
print(head(trigram_probabilities,20))

# Calculate probabilities for four-grams
quad_gram_probabilities <- get.phrasetable(quad_grams_text)
cat("\nQuad-gram Probabilities:\n")
print(head(quad_gram_probabilities,20))

# Calculate probabilities for five-grams
five_gram_probabilities <- get.phrasetable(five_grams_text)
cat("\nFive-gram Probabilities:\n")
print(head(five_gram_probabilities,20))

## ------------ for prediction --------------------
# Merging the data
phrase_2 <- biigram_probabilities %>%
  mutate(context = word(biigram_probabilities$ngrams, 1),
         next_word = word(bigram_probabilities$ngrams, 2)) %>%
  select(context, next_word, freq, prop)

phrase_3 <- trigram_probabilities %>%
  mutate(context = word(trigram_probabilities$ngrams, 1, 3-1),
         next_word = word(trigram_probabilities$ngrams, 3)) %>%
  select(context, next_word, freq, prop)

phrase_4 <- quad_gram_probabilities %>%
  mutate(context = word(quad_gram_probabilities$ngrams, 1, 4-1),
         next_word = word(quad_gram_probabilities$ngrams, 4)) %>%
  select(context, next_word, freq, prop)

phrase_5 <- five_gram_probabilities %>%
  mutate(context = word(quad_gram_probabilities$ngrams, 1, 5-1),
         next_word = word(quad_gram_probabilities$ngrams, 5)) %>%
  select(context, next_word, freq, prop)

phrasetable <- rbind(phrase_2, phrase_3, phrase_4)
sum(phrasetable$prop) #3

phrasetable$context <- as.factor(phrasetable$context)
phrasetable$next_word <- as.factor(phrasetable$next_word)

features <- data.frame(
  context = as.numeric(phrasetable$context), # Encode context as numeric
  freq = phrasetable$freq,
  prop = phrasetable$prop
)
labels <- phrasetable$next_word

library(caret)
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(labels, p = 0.75, list = FALSE)
trainData <- features[trainIndex, ]
trainLabels <- labels[trainIndex]
testData <- features[-trainIndex, ]
testLabels <- labels[-trainIndex]

trainData$context <- as.numeric(trainData$context)
trainLabels <- as.factor(trainLabels)

# If the dataset is large, sample a manageable subset
if (nrow(trainData) > 10000) {
  sampled_indices <- sample(1:nrow(trainData), size = 10000)
  trainData <- trainData[sampled_indices, ]
  trainLabels <- trainLabels[sampled_indices]
}

# Ensure consistent factor levels in the target variable
trainLabels <- droplevels(trainLabels)
trainLabels <- str_replace_all(trainLabels, "[^[:alnum:] ]", "")
# Remove empty labels
trainLabels <- trainLabels[trainLabels != ""]

# Or replace empty labels with a placeholder
trainLabels[trainLabels == ""] <- "unknown"

# Ensure test labels match the levels of train labels
testLabels <- factor(testLabels, levels = levels(trainLabels))

trainData <- trainData[1:length(trainLabels), ]

# Verify the alignment after adjustment
cat("Rows in trainData after adjustment: ", nrow(trainData), "\n")
cat("Length of trainLabels: ", length(trainLabels), "\n")

trainLabels <- as.factor(trainLabels)

train_control <- trainControl(method="none")
# Train k-NN model with reduced tuneLength
knn_model <- train(
  x = trainData,
  y = trainLabels,
  method = "knn",
  tuneGrid = expand.grid(k = 3),
  trControl = train_control# Reduced for faster computation and less memory usage
)

# Print model details
print(knn_model)

testDataSubset <- testData[1:1000, ]

# Try predicting with the subset of data
predictions <- predict(knn_model, testDataSubset)
df <- data.frame(Actual = testDataSubset$next_word, Predicted = predictions)
