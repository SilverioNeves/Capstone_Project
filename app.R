# Captsone Project - Data Science Specialization
# Silvério Neves
# December 2018
# Shiny Application 

# required libraries
library(shiny)
library(tm)
library(stringr)
library(stylo)
library(dplyr)

# functions and data required
source("project_functions.R")


# UI Application
ui <- fluidPage(
   
   # Application title
   titlePanel("Suggest Next Word Application"),
   
   # Sidebar with a slider input for the text
   sidebarLayout(
      sidebarPanel(
              textInput(inputId="text_in", label = "Write some words to  suggest next one"),
              actionButton("generate", "Click Me to suggest words" )
      ),
      
      # Show suggested words
      mainPanel(
         p("Here are the suggested words (from 0 to 5. NA = No word)"),
         span(textOutput('text_out_1'), style = "color:red"),
         span(textOutput('text_out_2'), style = "color:blue"),
         span(textOutput('text_out_3'), style = "color:green"),
         span(textOutput('text_out_4'), style = "color:brown"),
         span(textOutput('text_out_5'), style = "color:gray")
      )
   )
)

# Sever part
server <- function(input, output, session) {
 
        observeEvent(input$generate, {
                pred <- predictnw(input$text_in)
                output$text_out_1 <- renderText(pred[1])
                if ( length(pred) > 1)
                        output$text_out_2 <- renderText(pred[2])
                        output$text_out_3 <- renderText(pred[3])
                        output$text_out_4 <- renderText(pred[4])
                        output$text_out_5 <- renderText(pred[5])
                
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

