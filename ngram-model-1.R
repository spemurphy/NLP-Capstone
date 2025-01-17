library(dplyr)
library(stringr)
library(tidyr)

# Example large text corpus (train_set) with 350,000 entries
# Assuming `train_set` is a character vector with text data
corpus <- tolower(train_set)                # Convert to lowercase
corpus <- gsub("[[:punct:]]", " ", corpus)  # Remove punctuation
corpus <- gsub("[[:digit:]]", " ", corpus)  # Remove numbers
corpus <- gsub("\\s+", " ", corpus)         # Remove extra whitespaces
corpus <- trimws(corpus)                    # Trim leading/trailing spaces
corpus <- gsub("[^\x01-\x7F]", "", corpus)  # Remove non-ASCII characters


# Tokenize the corpus into words
tokenize <- function(text) {
  unlist(strsplit(text, "\\s+"))
}
tokens <- unlist(lapply(corpus, tokenize))


create_transition_matrix <- function(tokens, context_length = 1) {
  n <- length(tokens)
  
  # Ensure enough tokens to create n-grams
  if (n <= context_length) {
    return(data.frame(ngram = character(0), next_word = character(0), freq = integer(0), prob = numeric(0)))
  }
  
  # Generate n-grams and their corresponding next words
  ngrams <- sapply(1:(n - context_length), function(i) {
    paste(tokens[i:(i + context_length - 1)], collapse = " ")
  })
  next_words <- tokens[(context_length + 1):n]
  
  # Create frequency and probability table
  transitions <- data.frame(ngram = ngrams, next_word = next_words)
  transition_matrix <- transitions %>%
    group_by(ngram, next_word) %>%
    summarize(freq = n(), .groups = "drop") %>%
    group_by(ngram) %>%
    mutate(prob = freq / sum(freq)) %>%
    ungroup() %>%
    arrange(ngram, desc(prob))
  
  return(transition_matrix)
}


# Generate transition matrices
max_context_length <- 3
transition_matrices <- lapply(1:max_context_length, function(context_length) {
  create_transition_matrix(tokens, context_length)
})
names(transition_matrices) <- paste0("context_", 1:max_context_length)

saveRDS(transition_matrices, file="nlpApp/data/transition_matrices.rds")