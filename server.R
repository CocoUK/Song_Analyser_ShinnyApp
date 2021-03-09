#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(wordcloud)
 


# Define server logic 
 function(input, output, session) {
     rv <- reactiveValues(new_Word = NULL)
     

     # Display avg number of lyrics
     output$text10 = renderText({
             rv$new_Word <- getSongLyrics(input$artist, input$no_songs)
             paste0(round(mean(rv$new_Word$word_count),2))
             })
     
     #Make the wordcloud 
     output$plot <- renderPlot({
         artist_wordcloud(rv$new_Word)
     })
     
 }
