# server.R ####
# Coursera Data Science Capstone Project (https://www.coursera.org/course/dsscapstone)
# Shiny server script
# 2016-01-23

# Libraries and options ####
source('functions.R')

library(shiny)

# Define application ####

shinyServer(function(input, output) {

# Reactive statement for prediction function when user input changes ####
    prediction =  reactive( {
        
        # Get input
        inputText = input$text
        input12 =  clean_input(inputText)
        #input2 =  clean_input(inputText)[2, ]
        nSuggestion = input$slider
        
        # Predict
        prediction = nextWord(input12,nSuggestion)
    })

# Output data table ####
output$table = renderDataTable(prediction()[,c(3,5)],
                            option = list(pageLength = 5,
                                        lengthMenu = list(c(5, 10, 100), c('5', '10', '100')),
                                        #columnDefs = list(list(visible = F, targets = 1)),
                                        searching = F
                                        )
                                )

# Output word cloud ####
wordcloud_rep = repeatable(wordcloud)
output$wordcloud = renderPlot(
                        wordcloud_rep(
                            prediction()$prediction,
                            prediction()$frequency,
                            colors = brewer.pal(8, 'Dark2'),
                            scale=c(4, 0.5),
                            max.words = 300
                            )
                    )



#output markdown
output$documentation <- renderText( 
  readLines("Documentation.html")  
)

#output repo
output$repo <- renderText("the repo can be found here:")
})