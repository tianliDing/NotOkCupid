#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tm)
library(wordcloud)
library(memoise)
library(ggvis)

# Define UI for application that draws a histogram

ui <- navbarPage(
    fluid = TRUE,
    "NotOkCupid",
    tabPanel("Component 1",
             titlePanel("NotOkCupid"),
             sidebarLayout(
             sidebarPanel(
                 sliderInput("bins",
                             "Number of bins:",
                             min = 18,
                             max = 69,
                             value = 30),
                 selectInput("yv", "Y-axis:",
                             c("INCOME" = "income", "HEIGHT" = "height")),
             ),
                 
             mainPanel(
                 plotOutput("distPlot"),
             )),
    ), 
   
     tabPanel("Component 2",
             titlePanel("NotOkCupid"),
             sidebarLayout(
             sidebarPanel(
             sliderInput("bins",
                         "Number of bins:",
                         min = 18,
                         max = 69,
                         value = 30),
             selectInput("yv", "Y-axis:",
                         c("INCOME" = "income", "HEIGHT" = "height")),
             selectInput("xv", "X-axis:",
                         c("AGE" = "age", "HEIGHT" = "height")),
             ),
    
    mainPanel(
        plotOutput("relationship"),
    )),
     ),
    
    tabPanel("Component 3",
             titlePanel("NotOkCupid"),
             sidebarLayout(
             sidebarPanel(
                 sliderInput("freq",
                             "Minimum Frequency:",
                             min = 1,  max = 50, value = 15),
                 sliderInput("max",
                             "Maximum Number of Words:",
                             min = 1,  max = 300,  value = 100)  
             ),
          
    mainPanel(
        plotOutput("plot"),
             )),
    ),

    
    tabPanel("Component 4",
             titlePanel("DATING EXPLORER"),
             fluidRow(
                 column(3,
                        wellPanel(
                            h4("Filter"),
                            sliderInput("Height", "your ideal Height",
                                        43, 95, 0, step = 10),
                            sliderInput("Income", "What is your preferred income", 0, 1000000, 0,
                                        step = 100000),
                            selectInput("Offspring_1", "do you want to have a already borned kids",
                                        c("YES", "NO")
                            ),),
                        wellPanel(
                            selectInput("xvar", "X-axis variable", c('Orientation',"Drugs","Age"), selected = "Orientation"),
                            selectInput("yvar", "Y-axis variable", c('Orientation',"Drugs","Age"), selected = "Drugs"),
                        )
                 ),
                 column(9,
                        ggvisOutput("plot1"),
                        wellPanel(
                            span("Number of cupid selected:",
                                 textOutput("n_cupid")
                            )
                        )
                 )
             )
    )
)

# The list of valid books

MyCupid = read.csv("MyCupid.csv")
book = MyCupid[,11]
books = book[! is.na(book)]


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- MyCupid[,2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })

    output$relationship <- renderPlot({

        if(input$xv == "age"){
            x <- MyCupid[,2]
        }else if(input$xv == "height"){
            x <- MyCupid[,9]
        }
        if(input$yv == "income"){
            y<- MyCupid[,10]
        }else if(input$yv == "height"){
            y<- MyCupid[,9]
        }
           # MyCupid[, c(x, y), drop = FALSE]
        plot(x,y)
    })

}


# Run the application 
shinyApp(ui = ui, server = server)

