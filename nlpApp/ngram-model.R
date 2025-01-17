library(dplyr)
library(stringr)
library(tidyr)
library(quanteda)
library(tokenizers)

tokenize <- function(text) {
  unlist(strsplit(text, "\\s+"))
}

predict_next_word <- function(context, transition_matrices, max_context_length = 3) {
  # Tokenize the input context
  context_tokens <- tokenize(context)
  
  # Iterate from max_context_length down to 1
  for (context_length in seq(max_context_length, 1)) {
    if (length(context_tokens) >= context_length) {
      # Extract the last context_length words from the context
      current_context <- paste(tail(context_tokens, context_length), collapse = " ")
      
      # Search for the context in the corresponding transition matrix
      predictions <- transition_matrices[[paste0("context_", context_length)]] %>%
        filter(ngram == current_context) %>%
        arrange(desc(prob))
      
      # Return the most probable next word if found
      if (nrow(predictions) > 0) {
        return(predictions$next_word[1])
      }
    }
  }
  
  # Return NA if no prediction is found
  return(NA)
}

# Test case
# context <- "I went to"
# predicted_word <- predict_next_word(context, transition_matrices, max_context_length = 3)
# cat("Next word prediction for context '", context, "': ", predicted_word, "\n", sep = "")
