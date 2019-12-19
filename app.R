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
library(shiny)
library(shinyWidgets)
library(dplyr)
library(shinythemes)

# helper variables
MyCupid = read.csv("MyCupid.csv")
varss = list("Job","Ethnicity")
# data cleaning
#CupidDf <- MyCupid %>%
    #mutate(cupid_name = row.names(MyCupid)) %>%
    #select(cupid_name, age, sex, height)
CupidDf <- MyCupid %>%
    mutate(cupid_name = row.names(MyCupid)) %>%
    #select(cupid_name, age, height, offspring_1, sign,body_type,drinks,drugs,status,smokes)
select(cupid_name, age, sex, height, offspring_1, sign,body_type,drinks,drugs,status,smokes)
# Define UI for application

ui <- navbarPage(
    fluid = TRUE,
    theme = shinytheme("paper"),
    "NotOkCupid",
##### ==================================== About Page ======================================    
    tabPanel("Story Board",
             fluidRow(
                 column(6,
                        wellPanel(
                        includeMarkdown("test.md")
                 ),
                 )
             )
    ),
##### ==================================== Demographic1 ======================================    
    navbarMenu("GET TO KNOW YOUR POTENTIAL MATCHES!",
    tabPanel("Age, Height and Income",
             titlePanel("Age, Height and Income"),
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
                 column(8, align="center",
                        plotOutput("distPlot"),
                 )
                 
             )),
             setBackgroundImage(src = "http://static.adweek.com/adweek.com-prod/wp-content/uploads/2018/01/dtf-hed-2017.jpg")
    ), 
    
    ##### ==================================== Demographic2 ======================================    
    tabPanel("Ethnicity and Job",
             titlePanel("Ethnicity and Job"),
             
                 selectInput("selection", "Choose a Variable:",
                             choices = varss),
                 actionButton("update", "Change"),
                 hr(),
                 sidebarPanel(
                     sliderInput("freq",
                                 "Minimum Frequency:",
                                 min = 1,  max = 50, value = 15),
                     sliderInput("max",
                                 "Maximum Number of Words:",
                                 min = 1,  max = 300,  value = 100)  
                 ),
                 mainPanel(
                     plotOutput("plot", height = "800px", width = "800px"),
                 ),
    )
    ),
    
##### ==================================== Relationship ====================================== 

tabPanel("Component 3",
         titlePanel("RELATIONSHIP"),
         fluidRow(
             column(3,
                    wellPanel(
                        h4("Filter"),
                        selectInput("sex_3", "your prefered sex:",
                                    c("male" = "m", "female" = "f")),
                        sliderInput("height2_3", "your ideal height range (cm):",
                                    109, 242,c(150,190), step = 1),
                        sliderInput("income_3", "your preferred income range (annual salary):", 0, 1000000, c(0,400000),
                                    step = 10000)
                    ),
                    wellPanel(
                        selectInput("xv4", "X-axis variable", c("orientation" = "orientation","drugs"="drugs","drink"="drinks", "smoke"="smokes")),
                        selectInput("yv4", "Y-axis variable", c("Age"="age","Orientation" = "orientation","Drugs"="drugs"))
                    )
             ),
             column(9,
                    wellPanel(
                        span("Number of cupid selected for you:",
                             textOutput("n_Cupid")
                        )
                    ),
                    wellPanel(
                        plotOutput("filterPlot")
                    )
             )
         )
),
                 
##### ==================================== Dating ======================================
  
    tabPanel("Component 4",
             titlePanel("DATING EXPLORER"),
             fluidRow(
                 column(3,
                        wellPanel(
                         textInput("name",
                                   "YOUR Destiny"),
                         selectInput("sign",
                                     "sign",
                                     choices = c("NO PREFERENCE", CupidDf$sign)
                         ),
                         selectInput("body_type",
                                     "Your preferred body type",
                                     choices = c("NO PREFERENCE", CupidDf$body_type)
                         ),
                         selectInput("status",
                                     "Your preferred status",
                                     choices = c("NO PREFERENCE", CupidDf$status)
                         ),
                         selectInput("drinks",
                                     "What kind of drinking frequency can you accept?",
                                     choices = c("NO PREFERENCE", CupidDf$drinks)
                         ),
                         selectInput("drugs",
                                     "What kind of drugs habit can you accept?",
                                     choices = c("NO PREFERENCE", CupidDf$drugs)
                         ),
                         selectInput("smokes",
                                     "Do you accept smoking behavior, if so what kind of smoking status can you accept?",
                                     choices = c("NO PREFERENCE", CupidDf$smokes)
                         ),
                         selectInput("offspring_1",
                                     "Do you want Kids?",
                                     choices = c("NO PREFERENCE", CupidDf$offspring_1)
                         )
                        )
                     ),
                     column(9,
                         wellPanel(
                            DT::dataTableOutput("table"),
                            plotOutput("plot4")
                         )
                     )
                 )
             )
    
)

### wordcloud helper
getTermMatrix <- memoise(function(var) {
    
    if (var=="Job"){
        text <- readLines( file("Jobs.csv") )
    } else {
        text <- readLines( file("ethnicity.csv") )
    }
    
    myCorpus = Corpus(VectorSource(text))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
    
    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    
    m = as.matrix(myDTM)
    
    sort(rowSums(m), decreasing = TRUE)
})

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
##### ==================================== Component2 ======================================    
    # Define a reactive expression for the document term matrix
    terms <- reactive({
        # Change when the "update" button is pressed...
        input$update
        # ...but not for anything else
        isolate({
            withProgress({
                setProgress(message = "Processing corpus...")
                getTermMatrix(input$selection)
            })
        })
    })
    
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
        v <- terms()
        wordcloud_rep(names(v), v, scale=c(9,0.5),
                      min.freq = input$freq, max.words=input$max,
                      colors=brewer.pal(8, "Dark2"))
    })
    
##### ==================================== Component3 ======================================
    output$n_Cupid <- renderText({
        MyCupid = MyCupid[input$sex_3 == MyCupid$sex,]
        MyCupid = MyCupid[MyCupid$height2 >= input$height2_3[1],]
        MyCupid = MyCupid[MyCupid$height2 <= input$height2_3[2],]
        MyCupid = MyCupid[MyCupid$income >= input$income_3[1],]
        MyCupid = MyCupid[MyCupid$income <= input$income_3[2],]
        x = nrow(MyCupid)
        print(x)
    })
    
    output$filterPlot <- renderPlot({
        MyCupid = MyCupid[input$sex_3 == MyCupid$sex,]
        MyCupid = MyCupid[MyCupid$height2 >= input$height2_3[1],]
        MyCupid = MyCupid[MyCupid$height2 <= input$height2_3[2],]
        MyCupid = MyCupid[MyCupid$income >= input$income_3[1],]
        MyCupid = MyCupid[MyCupid$income <= input$income_3[2],]
        if(input$xv4 == "orientation"){
            x <- MyCupid[,15]
        }else if(input$xv4 == "drugs"){
            x <- MyCupid[,6]
        }else if(input$xv4 == "drinks"){
            x <- MyCupid[,5]
        }else if(input$xv4 == "smokes"){
            x <- MyCupid[,20]
        }
        
        if(input$yv4 == "orientation"){
            y<- MyCupid[,15]
        }else if(input$yv4 == "drugs"){
            y<- MyCupid[,6]
        }else if(input$yv4 == "age"){
            y <- MyCupid[,2]
        }
        plot(x,y,main="The Relationships", ylab=input$yv4, xlab=input$xv4, col="lightpink2")
    })
    
##### ==================================== Component4 ======================================
    reactiveDf <- reactive({
        if (input$name == "" &
            input$offspring_1 == "") {
            return(CupidDf)
        }
        
        if (input$name != "") {
            CupidDf <- CupidDf %>%
                filter(
                    grepl(input$name, cupid_name, ignore.case = TRUE)
                )
            
        }
        
        if (input$offspring_1 != "") {
            CupidDf <- CupidDf %>%
                filter(
                    offspring_1 == input$offspring_1
                )
            
        }
        
        return(CupidDf)
    })
    conditional <- function(condition, success) {
        if (condition) success else TRUE
    }
    
    reactiveDf <- reactive({
        CupidDf %>%
            filter(
                conditional(input$name != "", grepl(input$name, cupid_name, ignore.case = TRUE)),
                conditional(input$sign != "NO PREFERENCE", sign == input$sign),
                conditional(input$body_type != "NO PREFERENCE", body_type == input$body_type),
                conditional(input$drinks != "NO PREFERENCE", drinks == input$drinks),
                conditional(input$drugs != "NO PREFERENCE", drugs == input$drugs),
                conditional(input$smokes != "NO PREFERENCE", smokes == input$smokes),
                conditional(input$status != "NO PREFERENCE", status == input$status),
                conditional(input$offspring_1 != "NO PREFERENCE", offspring_1 == input$offspring_1)
            )
    })
    
    output$table <- DT::renderDataTable({
        reactiveDf()
    })
    

}


# Run the application 
shinyApp(ui = ui, server = server)

