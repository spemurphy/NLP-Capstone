library(shiny)
library(dplyr)
library(stringr)
library(tidyr)
library(quanteda)
library(tokenizers)

source("ngram-model.R") # Adjust the path to the model script

# Relative path to the transition matrices file
transition_matrices <- readRDS("data/transition_matrices.rds")


# Define server logic required to draw a histogram
function(input, output, session) {
  result <- eventReactive(input$enter, {
    predict_next_word(input$box1, transition_matrices, max_context_length=3)
  })
    
  output$text1 <- renderText({
    result()
    })
}


