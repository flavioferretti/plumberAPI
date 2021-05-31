#!/usr/bin/env Rscript
#
## load libraries ##====================================================
library(RMariaDB)
library(quantmod)
library(PerformanceAnalytics)
library(shiny)
options(shiny.port = 8082)
options(shiny.host = "0.0.0.0")
## load lib_flv
source("http://192.168.4.1/~flavio/lib/lib_flv.R")
# MariaDB parameters
db='PriceDB'
host='192.168.4.1'
user='flavio'
pw='27182818'

## Read tables from pricedb
my_query="show TABLES" 
list_of_symbols <- ReadParamOnMyDB(my_query, user, pw, db, host, "")

### Go Shiny App ####################################################### 
shinyApp(

# User interface -------------------------------------------------------
ui <- fluidPage(
  titlePanel("Flavio StudioChartist"),

  sidebarLayout(
    sidebarPanel(
      helpText("Select a symbol to examine. "),
      selectInput("symbol", "Symblos:", list_of_symbols[,1],selected = "EURUSDmt4", multiple = FALSE),        
      helpText("Total retrieved data lenght: "),verbatimTextOutput("sample_lenght"), 
      numericInput("bars", "Number of bar in chart:", 72, min = 20, max = 10000),
      selectInput("chart_type", "Chart type:", c("candlesticks","line"),selected = "line", multiple = FALSE), 
      selectInput("subset", "Subsets:", c("last 1 days","last 3 days","last 1 weeks","last 1 months","last 3 months","last 6 months","last 1 years","last 3 years","last 5 years","last 10 years"),selected = "last 1 months", multiple = FALSE), 
      br(),
      checkboxInput("studies", "Activate Studies", value = FALSE),
      selectInput("stg_type", "Strategy type:", c("sma crossover","sma enanched","bbands"),selected = "sma enanched", multiple = FALSE), 
      helpText("Indicators parameters: "),
      sliderInput("sma_fast_period", "SMA fast Period:",  min = 5, max = 100, value = 14, step = 1 ),
      sliderInput("sma_slow_period", "SMA slow Period:",  min = 10, max = 200, value = 50, step = 10 ),
      sliderInput("bbands_period", "Bollinger bands Period:",  min = 5, max = 100, value = 20, step = 5 ),

    ),

    mainPanel(plotOutput("plot"),br(),plotOutput("perf_plot"))
  )
),

# Server logic ---------------------------------------------------------
server <- function(input, output) {

  dataInput <- reactive({
    getSymbols.MySQL(input$symbol,env = globalenv(), 
					 user = user, 
					 password = pw, 
					 dbname = db, 
					 port = 3306, 
					 host = host, 
					 auto.assign = FALSE )
  })

  #tail symbol
  my_tailored_symbol <- reactive({  tail(dataInput(),input$bars)  })
  
  #display sample_lenght
  output$sample_lenght <- reactive({  nrow(dataInput())  })
  
  output$plot <- renderPlot({
      
	if(input$studies == FALSE)
    {
		chartSeries(my_tailored_symbol(), name=input$symbol, subset = input$subset,
			        theme = chartTheme('white'),type =  input$chart_type , 
			        TA=c(addVo() ))  
    } else 
      {
		
		if(input$stg_type == "sma enanched")
		{
			print(chartSeries(my_tailored_symbol(), theme = chartTheme("white"), subset = input$subset, 
	                up.col = "green", dn.col = "red", type =  input$chart_type,
	                TA = NULL, name = paste(input$symbol,input$stg_type)))
	        #get indi
		    sma_fast <- SMA(Cl(na.omit(my_tailored_symbol())),n=input$sma_fast_period)
		    sma_slow <- SMA(Cl(na.omit(my_tailored_symbol())),n=input$sma_slow_period)
		    # get signal  		     
			my_signal <- ifelse( sma_fast > sma_slow & Cl(na.omit(my_tailored_symbol())) > sma_fast ,  1,
			 		     ifelse( sma_fast < sma_slow & Cl(na.omit(my_tailored_symbol())) < sma_fast , -1,0)) 		     
			# returns
			my_returns = ROC(Cl( my_tailored_symbol() ))*my_signal
			# Remove not available values from the returns
			my_returns <- na.omit(my_returns)
			
			# Calculate the Portfolio
			my_portfolio <- exp(cumsum(my_returns))
		    
		    print(addTA(sma_fast, col="red", on = 1))
		    print(addTA(sma_slow, col="blue", on = 1))
		    print(addTA(my_signal, col="black", yrange = c(-2, 2)))
		    print(addTA(my_portfolio, col="blue" ))
        }
        if(input$stg_type == "bbands")
		{
			print(chartSeries(my_tailored_symbol(), theme = chartTheme("white"), subset = input$subset, 
	                up.col = "green", dn.col = "red", type =  input$chart_type,
	                TA = NULL, name = paste(input$symbol,input$stg_type)))
	        #get indi
	        my_bb_up <- BBands(HLC(na.omit(my_tailored_symbol())), n=input$bbands_period, maType = SMA)[,3]
	        my_bb_dn <- BBands(HLC(na.omit(my_tailored_symbol())), n=input$bbands_period, maType = SMA)[,1] 

		    # elaborazione signal 
			my_signal <- ifelse( Cl(na.omit(my_tailored_symbol())) >  my_bb_up   , 1,
						 ifelse( Cl(na.omit(my_tailored_symbol())) <  my_bb_dn   , -1,0))   	
						 	     
			# returns
			my_returns = ROC(Cl( my_tailored_symbol() ))*my_signal
			# Remove not available values from the returns
			my_returns <- na.omit(my_returns)
			
			# Calculate the Portfolio
			my_portfolio <- exp(cumsum(my_returns))
		    
		    print(addTA(my_bb_up, col="purple", on = 1))
		    print(addTA(my_bb_dn, col="purple", on = 1))
		    print(addTA(my_signal, col="black", yrange = c(-2, 2)))
		    print(addTA(my_portfolio, col="purple" ))
        }
        if(input$stg_type == "sma crossover")
		{
			print(chartSeries(my_tailored_symbol(), theme = chartTheme("white"), subset = input$subset, 
	                up.col = "blue", dn.col = "black", type =  input$chart_type,
	                TA = NULL, name = paste(input$symbol,input$stg_type)))
	        #get indi
		    sma_fast <- SMA(Cl(na.omit(my_tailored_symbol())),n=input$sma_fast_period)
		    sma_slow <- SMA(Cl(na.omit(my_tailored_symbol())),n=input$sma_slow_period)
		    # get signal  		     
			my_signal <- ifelse(lag(sma_fast)>lag(sma_slow) & lag(sma_fast,2)<lag(sma_slow,2) ,1,
                         ifelse(lag(sma_fast)<lag(sma_slow) & lag(sma_fast,2)>lag(sma_slow,2) ,-1,0))
			
			# returns
			my_returns = ROC(Cl( my_tailored_symbol() ))*my_signal
			# Remove not available values from the returns
			my_returns <- na.omit(my_returns)
			
			# Calculate the Portfolio
			my_portfolio <- exp(cumsum(my_returns))
		    
		    print(addTA(sma_fast, col="red", on = 1))
		    print(addTA(sma_slow, col="green", on = 1))
		    print(addTA(my_signal, col="black", yrange = c(-2, 2)))
		    print(addTA(my_portfolio, col="red" ))
        }
        
	  }#studies
  
  
  }) #renderPlot
  	  
  
  output$perf_plot <- renderPlot({ 
	  
	if(input$studies == TRUE)
    {
	  if(input$stg_type == "sma enanched")
	  {
		  #get indi
	      sma_fast <- SMA(Cl(na.omit(my_tailored_symbol())),n=input$sma_fast_period)
	      sma_slow <- SMA(Cl(na.omit(my_tailored_symbol())),n=input$sma_slow_period)
	      # get signal  		     
		  my_signal <- ifelse( sma_fast > sma_slow & Cl(na.omit(my_tailored_symbol())) > sma_fast ,  1,
		 		       ifelse( sma_fast < sma_slow & Cl(na.omit(my_tailored_symbol())) < sma_fast , -1,0)) 		     
		  # returns
		  my_returns = ROC(Cl(na.omit(my_tailored_symbol())))*my_signal
		  # Remove not available values from the returns
		  my_returns <- na.omit(my_returns)
		  charts.PerformanceSummary(my_returns, main = paste(input$symbol,input$stg_type," Performance Summary Chart") , col="blue")
      }
      if(input$stg_type == "bbands")
	  {
		  #get indi
	      my_bb_up <- BBands(HLC(na.omit(my_tailored_symbol())), n=input$bbands_period, maType = SMA)[,3]
	      my_bb_dn <- BBands(HLC(na.omit(my_tailored_symbol())), n=input$bbands_period, maType = SMA)[,1] 
	      # elaborazione signal 
		  my_signal <- ifelse( Cl(na.omit(my_tailored_symbol())) >  my_bb_up   , 1,
					   ifelse( Cl(na.omit(my_tailored_symbol())) <  my_bb_dn   , -1,0)) 		     
 		  # returns
 		  my_returns = ROC(Cl(na.omit(my_tailored_symbol())))*my_signal
 		  # Remove not available values from the returns
 		  my_returns <- na.omit(my_returns)
 		  charts.PerformanceSummary(my_returns, main = paste(input$symbol,input$stg_type," Performance Summary Chart") , col="purple")
       }
       if(input$stg_type == "sma crossover")
	  {
		  #get indi
		  sma_fast <- SMA(Cl(na.omit(my_tailored_symbol())),n=input$sma_fast_period)
	      sma_slow <- SMA(Cl(na.omit(my_tailored_symbol())),n=input$sma_slow_period)
		  # get signal  		     
		  my_signal <- ifelse(lag(sma_fast)>lag(sma_slow) & lag(sma_fast,2)<lag(sma_slow,2) ,1,
                       ifelse(lag(sma_fast)<lag(sma_slow) & lag(sma_fast,2)>lag(sma_slow,2) ,-1,0))		     
 		  # returns
 		  my_returns = ROC(Cl(na.omit(my_tailored_symbol())))*my_signal
 		  # Remove not available values from the returns
 		  my_returns <- na.omit(my_returns)
 		  charts.PerformanceSummary(my_returns, main = paste(input$symbol,input$stg_type," Performance Summary Chart") , col="red")
       }
	}  
  })#renderPlot perf_plot
  
  
}#server

)#shinyApp


 


