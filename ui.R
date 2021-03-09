 
library(shiny)
library(DT)

# Define UI for application that compares two artists lyrics
shinyUI(fluidPage(

    # Application title
    titlePanel("Artist Lyrics Analyser"),

    # Sidebar with a slider input for number of albums
    sidebarLayout(
        sidebarPanel(
            selectInput ("artist", "Choose an artist", choices = artists),
            sliderInput("no_songs","Number of songs:",min = 1, max = 5, value = 2),
            actionButton("update","Analyze")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h5('Average number of words in lyrics : '),
            withSpinner(textOutput('text10')),
            withSpinner(plotOutput("plot"))
            ),
    )
))