# ui.R ####
# 
# Shiny UI script
# 2019-02-12

# Libraries  ####
library(shiny)

#get rid of warnings
tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)

# Define the app definition

  fluidPage(
    #title
    titlePanel("English word predictor"),
  
    # Sidebar with input, slider and output 
      
      sidebarPanel(
        
        # Text input
        textInput("text", label = ('Please enter some text'), value = 'one or more words'),
        
        # Number of words slider input
       sliderInput('slider',
                    'Maximum number of words',
                    min = 0,  max =20,  value = 5
        ),
      
      #debug text output
      #textOutput("input$text"),
        
        # Table output
        dataTableOutput('table')
        ),
        
      # Mainpanel ####
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("WordCloud",plotOutput("wordcloud")),
                    tabPanel("Documentation",htmlOutput("documentation")),
                    tabPanel("GitRepository",textOutput("repo"))
        )
         
      ) 
    )
  
 
    
    
    
    
    
    





