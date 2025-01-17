library(shiny)


# Define UI for application that draws a histogram
fluidPage(
    # Application title
    titlePanel("Next Word Prediction App"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h1("Watch me predict your phrase"),
            textInput("box1", "Enter your phrase here"),
            actionButton("enter", "Enter")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2("Prediction: "),
            h4(textOutput("text1"))
        )
    )
) 
