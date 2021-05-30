#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(RMariaDB)
library(quantmod)
library(dplyr)
library(shiny)

db = 'PriceDB'
host = '192.168.4.1'
user = 'flavio'
pw = '27182818'

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("symbol2DB"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("yahoo_symbol", "Symbols:", value = "AAPL"),
            dateRangeInput(
                "dates",
                "Date range",
                start = "2020-01-01",
                end = as.character(Sys.Date())
            ),
            selectInput(
                "chart_type_yahoo",
                "Chart type:",
                c("candlesticks", "line"),
                selected = "candlesticks",
                multiple = FALSE
            ),
            selectInput(
                "subset_yahoo",
                "Subsets:",
                c(
                    "last 1 days",
                    "last 3 days",
                    "last 1 weeks",
                    "last 2 weeks",
                    "last 3 weeks",
                    "last 1 months",
                    "last 3 months",
                    "last 6 months",
                    "last 1 years",
                    "last 3 years",
                    "last 5 years",
                    "last 10 years",
                    "last 15 years",
                    "last 20 years",
                    "last 30 years"
                ),
                selected = "last 30 years",
                multiple = FALSE
            ),
            selectInput(
                "my_period_yahoo",
                "Chart period:",
                c("none", "daily", "weekly"),
                selected = "none",
                multiple = FALSE
            ),
            selectInput(
                "chart_teme",
                "Chart theme:",
                c("white", "black"),
                selected = "white",
                multiple = FALSE
            ),
            actionButton("goButton", "Go!", class = "btn-success")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput("plot"), hr(),
                  plotOutput("distPlot"))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Yahoo world ----------------------------------------------------------
    
    dataInput_yahoo <- reactive({
        if (input$my_period_yahoo == "none") {
            na.omit(
                getSymbols(
                    input$yahoo_symbol,
                    src = "yahoo",
                    from = input$dates[1],
                    to = input$dates[2],
                    auto.assign = FALSE
                )
            )
        }#none
        else if (input$my_period_yahoo == "daily") {
            na.omit(to.daily(
                getSymbols(
                    input$yahoo_symbol,
                    src = "yahoo",
                    from = input$dates[1],
                    to = input$dates[2],
                    auto.assign = FALSE
                )
            ))
        }#daily
        else if (input$my_period_yahoo == "weekly") {
            na.omit(to.weekly(
                getSymbols(
                    input$yahoo_symbol,
                    src = "yahoo",
                    from = input$dates[1],
                    to = input$dates[2],
                    auto.assign = FALSE
                )
            ))
        }#weekly
        
        
        
    })
    
    # getSymbols(Symbols = symbolvalue, warnings = FALSE,src='yahoo',from='1990-01-01' ,return.class='xts' )
    
    
    
    
    output$plot <- renderPlot({
        
        # Take a dependency on input$goButton. This will run once initially,
        # because the value changes from NULL to 0.
        input$goButton
        
        
        y <- dataInput_yahoo()
        chartSeries(
            y,
            name = input$yahoo_symbol,
            subset = input$subset_yahoo,
            theme = chartTheme(input$chart_teme),
            type =  input$chart_type_yahoo ,
            TA = c(addVo())
        )
        
        
        xa <- getSymbols(
            input$yahoo_symbol,
            src = "yahoo",
            from = input$dates[1],
            to = input$dates[2],
            auto.assign = FALSE ,
            return.class = 'xts'
        )
        ###
        #MyTickerName <- .Last.value # keep them
        ###
        x <- data.frame(xa)
        x <- na.omit(x)
        
        #debug
        #tail(x)
        #names(x)[0] <- "date"
        x$date<-rownames(x)
        
        names(x)[1] <- "o"
        names(x)[2] <- "h"
        names(x)[3] <- "l"
        names(x)[4] <- "c"
        names(x)[5] <- "v"
        names(x)[6] <- "a"
        
        write.csv(x, "data.csv", row.names = FALSE)
 
         
        ### mariaDB ###
        db <-
            dbConnect(
                RMariaDB::MariaDB(),
                user = user,
                password = pw,
                dbname = db,
                host = host
            )
        #dbWriteTable(storiesDb, value = sampleGardenData, row.names = FALSE, name = "tbl_newspaper_search_results", append = TRUE )
        dbWriteTable(
            db,
            value = x,
            row.names = FALSE,
            name = input$yahoo_symbol,
            overwrite = TRUE
        )
        dbDisconnect(db)
        ###
        rm(x)
        ###
        
        
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
