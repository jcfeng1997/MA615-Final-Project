library(shiny)
library(tidyverse)
library(lubridate)
library(bizdays)
library(tidyquant)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

getSymbols(c("AMZN","SQ","PANW","GTBIF"),from="2020-07-01",
           to="2020-12-01",warnings = FALSE,auto.assign = TRUE)

appCSS <-
   ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 10px 10px 10px; }
  "

# Define UI for application
ui <- fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "Jiachen Feng's Portfolio",
    
    # Header
    div(id = "header",
        h1("Jiachen Feng's Portfolio"),
        strong( 
            span("Created by "),
            a("Jiachen Feng", href = "https://github.com/jcfeng1997"),
            HTML("&bull;"),
            span("Details on"),
            a("GitHub", href = "https://github.com/jcfeng1997/MA615-Final-Project"),
            HTML("&bull;"))
    ),
    
    # Sidebar 
    sidebarPanel(
        dateInput("dateinput", label = h3("Since 2020/07/01-"), value = "2020-11-30",min = "2020-07-01",max = "2020-11-30"),
        hr(),
        fluidRow(column(3, verbatimTextOutput("date"))),
        br(),  
        helpText(strong("Jiachen picked 4 stocks as his portfolio.",
                 "They are:",
                 "Amazon, Square, Palo Alto Networks and Green Thumb Industries."),
        br(),
        br(),
        br(),
        img(src = "amazon.png", height = 36, width = 144),
        br(),
        img(src = "square.png", height = 72, width = 144),
        br(),
        img(src = "paloalto.png", height = 72, width = 144),
        br(),
        br(),
        img(src = "greenthumb.png", height = 72, width = 144),
        ),
    ), 
    # MainPanel
    mainPanel(
        tabsetPanel(
            tabPanel("Stocks(Daily)",
              br(),
              textOutput("introduction1"),
              tags$head(tags$style("#introduction1{color: darkblue; font-size: 20px; font-style: italic;}")),
              br(),
              h3("Amazon"),
              br(),
              plotOutput("stocksamzn"),
              br(),
              h3("Square"),
              br(),
              plotOutput("stockssq"),
              br(),
              h3("Palo Alto Networks"),
              br(),
              plotOutput("stockspanw"),
              br(),
              h3("Green Thumb Industries"),
              br(),
              plotOutput("stocksgtbif")
              ),
            
            tabPanel("Stocks(Weekly)",
              br(),  
              textOutput("introduction2"),
              tags$head(tags$style("#introduction2{color: darkblue; font-size: 20px; font-style: italic;}")),
              h3("Amazon"),
              br(),
              plotOutput("stocksamzn1"),
              br(),
              h3("Square"),
              br(),
              plotOutput("stockssq1"),
              br(),
              h3("Palo Alto Networks"),
              br(),
              plotOutput("stockspanw1"),
              br(),
              h3("Green Thumb Industries"),
              br(),
              plotOutput("stocksgtbif1")
            ),
       
            tabPanel("Value",
              helpText('Jiachen has an initial position of $250000 in cash starting on 1 July 2020.'),
              br(),
              textOutput("amznvalue"),
              br(),
              textOutput("sqvalue"),
              br(),
              textOutput("panwvalue"),
              br(),
              textOutput("gtbifvalue"),
              br(),
              textOutput("totalvalue"),
              tags$head(tags$style("#totalvalue{color: darkblue; font-size: 16px; font-style: bold-italic;}")),
              br(),
              helpText(strong('Good Job! Jiachen. This is your first step in investing!!'))
              )
    )
    )
)
   
# Define Server for application
server <- function(input, output) {
    
    
    amzninput <- reactive({
        subset <- paste('2020-07-01::',input$dateinput,sep = "")
        chartSeries(to.daily(AMZN), theme=chartTheme('white'),subset = subset)
    })
    
    sqinput <- reactive({
        subset <- paste('2020-07-01::',input$dateinput,sep = "")
        chartSeries(to.daily(SQ), theme=chartTheme('white'),subset = subset)
    })
    
    panwinput <- reactive({
        subset <- paste('2020-07-01::',input$dateinput,sep = "")
        chartSeries(to.daily(PANW), theme=chartTheme('white'),subset = subset)
    })
    
    gtbifinput <- reactive({
        subset <- paste('2020-07-01::',input$dateinput,sep = "")
        chartSeries(to.daily(GTBIF), theme=chartTheme('white'),subset = subset)
    })
    
    amzninputweek <- reactive({
        subset <- paste('2020-07-01::',input$dateinput,sep = "")
        chartSeries(to.weekly(AMZN), theme=chartTheme('white'),subset = subset)
    })
    
    sqinputweek <- reactive({
        subset <- paste('2020-07-01::',input$dateinput,sep = "")
        chartSeries(to.weekly(SQ), theme=chartTheme('white'),subset = subset)
    })
    
    panwinputweek <- reactive({
        subset <- paste('2020-07-01::',input$dateinput,sep = "")
        chartSeries(to.weekly(PANW), theme=chartTheme('white'),subset = subset)
    })
    
    gtbifinputweek <- reactive({
        subset <- paste('2020-07-01::',input$dateinput,sep = "")
        chartSeries(to.weekly(GTBIF), theme=chartTheme('white'),subset = subset)
    })
    
    amznvalue <- reactive({
        amzn <- data.frame(date=index(AMZN),AMZN)
      
        for (i in 1:nrow(amzn)){
            indexToShow <- 0
            if (input$dateinput == amzn[i,1]){
                indexToShow <- i
            }
            else if (input$dateinput< amzn[i,1]){
                indexToShow <- i - 1
            }
            
            if (indexToShow != 0){
              
                amzncurrent <- amzn[indexToShow,]$AMZN.Close*(100000/2878.7)
                break
            }
            
        }
        textamzn <- paste('As of this date, the value of Amazon stock owned by Jiachen is $',amzncurrent,sep = " ")
        textamzn
    })
    
    sqvalue <- reactive({
        sq <- data.frame(date=index(SQ),SQ)
        
        for (i in 1:nrow(sq)){
            indexToShow <- 0
            if (input$dateinput == sq[i,1]){
                indexToShow <- i
            }
            else if (input$dateinput< sq[i,1]){
                indexToShow <- i - 1
            }
            
            if (indexToShow != 0){
                sqcurrent <- sq[indexToShow,]$SQ.Close*(50000/115.9)
                break
            }
            
        }
        textsq <- paste('As of this date, the value of Square stock owned by Jiachen is $',sqcurrent,sep = " ")
        textsq
    })
    
    panwvalue <- reactive({
        panw <- data.frame(date=index(PANW),PANW)
        
        for (i in 1:nrow(panw)){
            indexToShow <- 0
            if (input$dateinput == panw[i,1]){
                indexToShow <- i
            }
            else if (input$dateinput< panw[i,1]){
                indexToShow <- i - 1
            }
            
            if (indexToShow != 0){
                panwcurrent <- panw[indexToShow,]$PANW.Close*(50000/229.36)
                break
            }
            
        }
        textpanw <- paste('As of this date, the value of Palo Alto Networks stock owned by Jiachen is $',panwcurrent,sep = " ")
        textpanw
    })
    
    gtbifvalue <- reactive({
        gtbif <- data.frame(date=index(GTBIF),GTBIF)
        
        for (i in 1:nrow(gtbif)){
            indexToShow <- 0
            if (input$dateinput == gtbif[i,1]){
                indexToShow <- i
            }
            else if (input$dateinput< gtbif[i,1]){
                indexToShow <- i - 1
            }
            
            if (indexToShow != 0){
                gtbifcurrent <- gtbif[indexToShow,]$GTBIF.Close*(50000/10.19)
                break
            }
            
        }
        textgtbif <- paste('As of this date, the value of Green Thumb Industries stock owned by Jiachen is $',gtbifcurrent,sep = " ")
        textgtbif
    })
    
    totalvalue <- reactive({
        amzn <- data.frame(date=index(AMZN),AMZN)
        sq <- data.frame(date=index(SQ),SQ)
        panw <- data.frame(date=index(PANW),PANW)
        gtbif <- data.frame(date=index(GTBIF),GTBIF)
        
        for (i in 1:nrow(amzn)){
            indexToShow <- 0
            if (input$dateinput == gtbif[i,1]){
                indexToShow <- i
            }
            else if (input$dateinput< gtbif[i,1]){
                indexToShow <- i - 1
            }
            
            if (indexToShow != 0){
                totalcurrent <- amzn[indexToShow,]$AMZN.Close*(100000/2878.7)+
                    sq[indexToShow,]$SQ.Close*(50000/115.9)+
                    panw[indexToShow,]$PANW.Close*(50000/229.36)+
                    gtbif[indexToShow,]$GTBIF.Close*(50000/10.19)
                    
                break
            }
            
        }
        texttotal1 <- paste('As of this date, the total value of the four stocks owned by Jiachen is $',
                           totalcurrent,sep = " ")
        texttotal2 <- paste('After these days, the assets increased $',totalcurrent-250000,sep = " ")
        texttotal <- paste(texttotal1,texttotal2,sep = ". ")
        texttotal
    })
    
    output$introduction1 <- renderText({
      "Here's the stock price chart(Daily Chart) of 4 stocks selected by Jiachen."
    })    
    output$introduction2 <- renderText({
      "Here's the stock price chart(Weekly Chart) of 4 stocks selected by Jiachen."
    }) 

    output$stocksamzn <- renderPlot({
        print(amzninput())
    })
    
    output$stockssq <- renderPlot({
        print(sqinput())
    })
    
    output$stockspanw <- renderPlot({
        print(panwinput())
    })
    
    output$stocksgtbif <- renderPlot({
        print(gtbifinput())
    })
    
    output$stocksamzn1 <- renderPlot({
        print(amzninputweek())
    })
    
    output$stockssq1 <- renderPlot({
        print(sqinputweek())
    })
    
    output$stockspanw1 <- renderPlot({
        print(panwinputweek())
    })
    
    output$stocksgtbif1 <- renderPlot({
        print(gtbifinputweek())
    })
    
    output$amznvalue <- renderText({
        print(amznvalue())
    })
    output$sqvalue <- renderText({
        print(sqvalue())
    })
    output$panwvalue <- renderText({
        print(panwvalue())
    })
    output$gtbifvalue <- renderText({
        print(gtbifvalue())
    })
    output$totalvalue <- renderText({
        print(totalvalue())
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
