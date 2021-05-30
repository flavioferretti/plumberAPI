#!/usr/bin/env Rscript
#
## load libraries ##====================================================
library(RMariaDB)
library(quantmod)
library(shiny)
options(shiny.port = 8080)
options(shiny.host = "0.0.0.0")
library(shinythemes)

## load lib_flv
source("lib_flv.R")

# MariaDB parameters
db='PriceDB'
host='192.168.4.1'
user='flavio'
pw='27182818'

## Read tables from pricedb
my_query="show TABLES" 
list_of_symbols <- ReadParamOnMyDB(my_query, user, pw, db, host, "")


# User interface -------------------------------------------------------
ui <- fluidPage(theme = shinytheme("superhero"),
#ui <- fluidPage(
  titlePanel("Flavio StudioChartist 0.1.1"),

  sidebarLayout(
    sidebarPanel(
      helpText("Select data source. "),
      selectInput("data_source", "Data source:", c("mysql","yahoo","alphavantage"),selected = "mysql", multiple = FALSE), 
      
      conditionalPanel(
			  condition = "input.data_source == 'mysql'", #MySQL
	          helpText("Select a symbol to examine. "),
			  selectInput("symbol", "Symbols:", list_of_symbols[,1],selected = "EURUSDftp", multiple = FALSE),        
			  helpText("Total retrieved data lenght: "),verbatimTextOutput("sample_lenght"), 
			  numericInput("bars", "Number of bar in chart:", 100, min = 20, max = 100000),
			  selectInput("chart_type", "Chart type:", c("candlesticks","line"),selected = "candlesticks", multiple = FALSE), 
			  selectInput("subset", "Subsets:", c("last 1 days","last 3 days","last 1 weeks","last 2 weeks","last 3 weeks","last 1 months","last 3 months","last 6 months","last 1 years","last 3 years","last 5 years","last 10 years","last 15 years","last 20 years","last 30 years"),selected = "last 3 months", multiple = FALSE), 
			  selectInput("my_period", "Chart period:", c("none","daily","weekly"),selected = "none", multiple = FALSE) 
	  ),
      
      conditionalPanel(
			  condition = "input.data_source == 'yahoo'", #YAHOO
	          helpText("Select a symbol to examine. "),
			  textInput("yahoo_symbol", "Symbols:", value="AAPL"),        
			  dateRangeInput("dates",
                     "Date range",
                     start = "2020-01-01",
                     end = as.character(Sys.Date())),
			  selectInput("chart_type_yahoo", "Chart type:", c("candlesticks","line"),selected = "candlesticks", multiple = FALSE), 
			  selectInput("subset_yahoo", "Subsets:", c("last 1 days","last 3 days","last 1 weeks","last 2 weeks","last 3 weeks","last 1 months","last 3 months","last 6 months","last 1 years","last 3 years","last 5 years","last 10 years","last 15 years","last 20 years","last 30 years"),selected = "last 30 years", multiple = FALSE), 
			  selectInput("my_period_yahoo", "Chart period:", c("none","daily","weekly"),selected = "none", multiple = FALSE) 
	  ),
	  
	  conditionalPanel(
			  condition = "input.data_source == 'alphavantage'", #alphavantage
	          helpText("Select a symbol to examine. "),
			  textInput("av_symbol", "Symbols:", value="IBM"),  
			  selectInput("output_size", "Output size:", c("compact","full"),selected = "compact", multiple = FALSE),        
			  helpText("Total retrieved data lenght: "),verbatimTextOutput("sample_lenght_av"), 
			  numericInput("bars_av", "Number of bar in chart:", 100, min = 20, max = 100000),
			  selectInput("chart_type_av", "Chart type:", c("candlesticks","line"),selected = "candlesticks", multiple = FALSE), 
			  selectInput("subset_av", "Subsets:", c("last 1 days","last 3 days","last 1 weeks","last 2 weeks","last 3 weeks","last 1 months","last 3 months","last 6 months","last 1 years","last 3 years","last 5 years","last 10 years","last 15 years","last 20 years","last 30 years"),selected = "last 3 months", multiple = FALSE), 
			  selectInput("my_period_av", "Chart period:", c("daily","weekly","monthly","intraday"),selected = "daily", multiple = FALSE),
			  selectInput("my_interval_av", "Chart interval:", c("1min","5min","15min","30min","60min"),selected = "1min", multiple = FALSE)  
	  ),
	  
	  selectInput("chart_teme", "Chart theme:", c("white","black"),selected = "white", multiple = FALSE), 
	  selectInput("studies", "Studies:", c("OFF","ON"),selected = "OFF", multiple = FALSE),
	  conditionalPanel(
			  condition = "input.studies == 'ON'",
			  selectInput("studies_hist", "Display Histogram", c("OFF","ON"),selected = "OFF", multiple = FALSE),
			  checkboxInput("studies_SMA", "SMA", value = FALSE), 
			  sliderInput("sma_period", "SMA Period:",  min = 3, max = 200, value = 14, step = 1 ),
			  checkboxInput("studies_SMA2", "SMA #2", value = FALSE), 
			  sliderInput("sma_period2", "SMA Period #2:",  min = 3, max = 200, value = 50, step = 1 ),
			  checkboxInput("studies_MACD", "MACD", value = FALSE),
			  checkboxInput("studies_BBands", "BBands", value = FALSE),
			  sliderInput("vol_sma_period", "Volumes SMA Period:",  min = 0, max = 100, value = 0, step = 1 ),
	          numericInput("vol_sma_factor", "Volumes SMA chart factor:", 10, min = 100, max = 1000000,  value = 1000 )
      ) 
	),#sidebarPanel
    

    mainPanel(
				plotOutput("plot"),hr()  ,
				conditionalPanel(
					condition = "input.studies_hist == 'ON'",
					plotOutput("hist")
				)
	)
  )#sidebarLayout
)#fluidPage

# Server logic ---------------------------------------------------------
server <- function(input, output, session) {
 
# MySQL world ----------------------------------------------------------
  dataInput <- reactive({
    
    if(input$data_source == "mysql"){
		
	     if(input$my_period == "none"){
	     getSymbols.MySQL(input$symbol,env = globalenv(), 
						 user = user, 
						 password = pw, 
						 dbname = db, 
						 port = 3306, 
						 host = host, 
						 auto.assign = FALSE )
	     }##none
	     else if(input$my_period == "daily"){ 
	     
	     to.daily(getSymbols.MySQL(input$symbol,env = globalenv(),
											 return.class = 'xts',
	                                         user = user,
	                                         password = pw,
	                                         dbname = db,
	                                         port = 3306,
	                                         host = host,
	                                         auto.assign = FALSE ))
	     }#daily
	     else if(input$my_period == "weekly"){ 
	     to.weekly(getSymbols.MySQL(input$symbol,env = globalenv(),
											 return.class = 'xts',
	                                         user = user,
	                                         password = pw,
	                                         dbname = db,
	                                         port = 3306,
	                                         host = host,
	                                         auto.assign = FALSE ))
	     }#weekly
	     
	}#data_source=mysql
    
  })

  #tail symbol
  my_tailored_symbol <- reactive({ 
  		if(input$data_source == "mysql")na.omit( tail(dataInput(),input$bars) ) 
  
  })
  
  #display sample_lenght
  output$sample_lenght <- reactive({  nrow(dataInput())  })

# Yahoo world ----------------------------------------------------------

  dataInput_yahoo <- reactive({
    
    if(input$data_source == "yahoo"){
         
         if(input$my_period_yahoo == "none"){ 
	         na.omit(getSymbols(input$yahoo_symbol, src = "yahoo",
				        from = input$dates[1],
				        to = input$dates[2],
				        auto.assign = FALSE)	)		        
	     }#none
	     else if(input$my_period_yahoo == "daily"){ 
	         na.omit(to.daily(getSymbols(input$yahoo_symbol, src = "yahoo",
				        from = input$dates[1],
				        to = input$dates[2],
				        auto.assign = FALSE)	))		        
	     }#daily
	     else if(input$my_period_yahoo == "weekly"){ 
	         na.omit(to.weekly(getSymbols(input$yahoo_symbol, src = "yahoo",
				        from = input$dates[1],
				        to = input$dates[2],
				        auto.assign = FALSE)	))		        
	     }#weekly
	
	}#data_source=yahoo

  })

# Alphavanyage world ---------------------------------------------------
  dataInput_av <- reactive({
    
    if(input$data_source == "alphavantage"){
		
	   getSymbols(input$av_symbol, src = "av", api.key="IKEVJ3ZLBO67VEVM", env = globalenv(),
				        return.class = "xts",
						periodicity = input$my_period_av,
						adjusted = FALSE,
						interval = input$my_interval_av,
						output.size = input$output_size)   
   
		# workaround to avoid chartseries xts error
		a <- get(input$av_symbol)
		print(a) 			
	     
	}#data_source=alphavantage
    
  })

  #tail symbol_av
  my_tailored_symbol_av <- reactive({ 
  		if(input$data_source == "alphavantage")na.omit( tail(dataInput_av(),input$bars_av) ) 
  
  })
  
  #display sample_lenght_av
  output$sample_lenght_av <- reactive({  nrow(dataInput_av())  })
 

##-- Plot World --------------------------------------------------------
  output$plot <- renderPlot({
      
	 
	 if(input$data_source == "mysql"){ #----------- MySQL CHART --------
				 chartSeries(my_tailored_symbol(), name=input$symbol, subset = input$subset,
					theme = chartTheme(input$chart_teme),type =  input$chart_type , 
					TA=c(addVo() )) 
				
				 if(input$studies == "ON" & input$studies_SMA == TRUE){
					print( addSMA(n = input$sma_period, on = 1, col = "red") )
	             }
	             if(input$studies == "ON" & input$studies_SMA2 == TRUE){
					print( addSMA(n = input$sma_period2, on = 1, col = "blue") )
	             }
	             if(input$studies == "ON" & input$studies_MACD == TRUE){
					print( addMACD() )
	             }
	             if(input$studies == "ON" & input$studies_BBands == TRUE){
					print( addBBands() )
	             }
	             if(input$studies == "ON" & input$vol_sma_period >= 2){
					Volume_SMA_chart  	<-  SMA(Vo(my_tailored_symbol()),n=input$vol_sma_period) / input$vol_sma_factor
					print(addTA(Volume_SMA_chart$SMA, on = 2, col = "blue" ))
	             }
	 }
	 if(input$data_source == "yahoo"){ #----------- Yahoo CHART --------
				 chartSeries(dataInput_yahoo(), name=input$yahoo_symbol, subset = input$subset_yahoo,
					theme = chartTheme(input$chart_teme),type =  input$chart_type_yahoo , 
					TA=c(addVo() )) 
			     
			     if(input$studies == "ON" & input$studies_SMA == TRUE){
					print( addSMA(n = input$sma_period, on = 1, col = "red") )
	             }
	             if(input$studies == "ON" & input$studies_SMA2 == TRUE){
					print( addSMA(n = input$sma_period2, on = 1, col = "blue") )
	             } 
	             if(input$studies == "ON" & input$studies_MACD == TRUE){
					print( addMACD() )
	             }
	             if(input$studies == "ON" & input$studies_BBands == TRUE){
					print( addBBands() )
	             }
	             if(input$studies == "ON" & input$vol_sma_period >= 2){
					Volume_SMA_chart  	<-  SMA(Vo(dataInput_yahoo()),n=input$vol_sma_period) / input$vol_sma_factor
					print(addTA(Volume_SMA_chart$SMA, on = 2, col = "blue" ))
	             }
	 }
	 if(input$data_source == "alphavantage"){ #----------- ALPHA CHART --------
				 chartSeries(my_tailored_symbol_av(), name=input$av_symbol, subset = input$subset_av,
					theme = chartTheme(input$chart_teme),type =  input$chart_type_av , 
					TA=c(addVo() )) 
			     
			     if(input$studies == "ON" & input$studies_SMA == TRUE){
					print( addSMA(n = input$sma_period, on = 1, col = "red") )
	             }
	             if(input$studies == "ON" & input$studies_SMA2 == TRUE){
					print( addSMA(n = input$sma_period2, on = 1, col = "blue") )
	             } 
	             if(input$studies == "ON" & input$studies_MACD == TRUE){
					print( addMACD() )
	             }
	             if(input$studies == "ON" & input$studies_BBands == TRUE){
					print( addBBands() )
	             }
	             if(input$studies == "ON" & input$vol_sma_period >= 2){
					mktdata <- my_tailored_symbol_av()
					Volume_SMA_chart  	<-  SMA(mktdata[,5],n=input$vol_sma_period) / input$vol_sma_factor
					print("-----------------------")
					#print(mktdata[,5])
					print(Volume_SMA_chart)
					print(addTA(Volume_SMA_chart[,1], on = 2, col = "blue" ))
	             }
	 }
  
  }) #renderPlot

  output$hist <- renderPlot({
  
	  if(input$studies == "ON" & input$studies_hist == "ON"){
		  
		  if(input$data_source == "mysql")hist(Cl(my_tailored_symbol()), main = input$symbol, col = topo.colors(16), xlab="Close price")
		  else if(input$data_source == "yahoo")hist(Cl(dataInput_yahoo()), main = input$yahoo_symbol, col = topo.colors(16), xlab="Close price")
		  else if(input$data_source == "alphavantage")hist(Cl(my_tailored_symbol_av()), main = input$av_symbol, col = topo.colors(16), xlab="Close price")
	     
	  }
  
  })
}#server

### Go Shiny App ####################################################### 
shinyApp(ui, server)


 


