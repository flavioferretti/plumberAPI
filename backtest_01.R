#!/usr/bin/env Rscript
#
#options(warn=0)
#
# WD need to be change between different hosts
setwd("/home/flavio/Downloads")
# args stuff
args = commandArgs(trailingOnly = TRUE)
# test if there is at least one argument: if not, return an error
if (length(args) == 0) {
  stop("arg[1]=( Symbol p.es GOLDmt4  ) arg[2]=(data tail p.es 1200) )",
       call. = FALSE)
}

## load libraries ##====================================================
library(quantmod)
library(PerformanceAnalytics) # performance and risk management
#library(ssh)
## load flv_lib
source("http://192.168.4.1/~flavio/lib/lib_flv.R")

## chart param
my_chart_zoom = 'last 1 weeks'

## Downloading Stock Ticker Data from  MariaDB ##=======================
# getSymbols.MySQL parameters
db = 'PriceDB'
param_db = 'parameters'
host = '192.168.4.1'
user = 'flavio'
pw = '27182818'
#getSymbols.MySQL("AAPL",env = globalenv(), user = user, password = pw, dbname = db, port = 3306, host = host)
getSymbols.MySQL(
  args[1],
  env = globalenv(),
  user = user,
  password = pw,
  dbname = db,
  port = 3306,
  host = host
)
MyTickerName <- .Last.value # keep them
####====================================================================

########################################################################
## BT/OPTIMIZATION PARAMETERS ##========================================
STG_NAME = "bbands"
ResultSet = 12
MinimumProfitFactor 		=   1.4
OptimizationMinimumReturns 	=   1.0 #%
OptimizationMaximumDD 		=  10.0 #%


## prepare data #=======================================================
SYMBL <- na.omit(get(MyTickerName))
print(paste(
  "> Ticker Name:",
  MyTickerName,
  " Total sample lenght:",
  nrow(SYMBL)
))
#SYMBL <- tail(SYMBL, 500)
SYMBL <- tail(SYMBL, as.numeric(args[2]))
####====================================================================
big_portfolio <-
  big_returns <- trace_in_sample <- i <- j <- k <- l <-  NULL
opt_pass_counter = 0
SMA_RANGE      = c(seq(from = 10, to = 100, by = 10))
SMA_RANGE_SLOW = c(seq(from = 1, to = 2, by = 1))
ATR_RANGE      = c(seq(from = 1, to = 1, by = 1))
ATR_LAG_RANGE  = c(seq(from = 1, to = 1, by = 1))





for (i in SMA_RANGE)
{
  for (j in SMA_RANGE_SLOW)
  {
    #if (i < j)
      for (k in ATR_RANGE)
      {
        for (l in ATR_LAG_RANGE)
        {
          ################# STG ##################################################
          
          #get indi
	        my_bb_up <- BBands(HLC(na.omit(SYMBL)), n=i, maType = SMA)[,3]
	        my_bb_dn <- BBands(HLC(na.omit(SYMBL)), n=i, maType = SMA)[,1] 

		    # elaborazione signal 
			my_signal <- ifelse( Cl( SYMBL ) >  my_bb_up   , 1,
						 ifelse( Cl( SYMBL ) <  my_bb_dn   , -1,0))   	
						 	     
	 
		  tail(my_signal)
          ################## REPORTING ###########################################
          # returns
          my_returns = ROC(Cl(SYMBL)) * my_signal
          # Remove not available values from the returns
          my_returns <- na.omit(my_returns)
          # Profit Factor
          profits <- sum(my_returns[which(my_returns > 0)]) #profits
          losses  <- sum(my_returns[which(my_returns < 0)]) #losses
          pf = round(profits / (losses * -1), digits = 2)
          # DDs
          DDs <- findDrawdowns(my_returns)
          maxDDx = round(min(DDs$return) * 100 * -1 , digits = 3)
          # Calculate the Portfolio
          my_portfolio <- exp(cumsum(my_returns))
          
          #eq
          my_eq <-
            round((as.numeric(tail(
              my_portfolio, n = 1L
            )) - 1) * 100 , digits = 2)
          
          # Pct win
          total_trades_number <-
            length(my_returns[which(my_returns  != 0)])
          win_trades_number	<-
            length(my_returns[which(my_returns  > 0)])
          win_trades_pct      <-
            round(win_trades_number / total_trades_number * 100 , digits = 2)
          
          ### select only the bests
          if (my_eq >= OptimizationMinimumReturns
              & maxDDx <= OptimizationMaximumDD
              & pf >= MinimumProfitFactor)
          {
            opt_pass_counter = opt_pass_counter + 1
            
            # chart
            png(
			  file = paste0(STG_NAME, "_", MyTickerName,"_",opt_pass_counter,"_chart.png"),
			  width = 1280,
			  height = 800
			)
	            chartSeries(SYMBL, theme = chartTheme("white"),  
		                up.col = "green", dn.col = "red", type =  "candlesticks",
		                TA = NULL, name = MyTickerName)
	            print(addTA(my_bb_up, col="purple", on = 1))
		    print(addTA(my_bb_dn, col="purple", on = 1))
		    print(addTA(my_signal, col="black", yrange = c(-2, 2)))
		    print(addTA(my_portfolio, col="purple" ))  
		    dev.off()              
            
            trace_current <-
              data.frame(
                opt_pass_counter,
                i,
                j,
                k,
                l,
                my_eq,
                maxDDx,
                pf,
                total_trades_number,
                win_trades_pct
              )
            trace_in_sample <- rbind(trace_in_sample , trace_current)
            
            #current set stats
            chart_info <-
              paste(
                STG_NAME,
                opt_pass_counter,
                i,
                j,
                k,
                l,
                "trades:",
                total_trades_number,
                "Wins %:",
                win_trades_pct,
                "Returns %:",
                my_eq,
                "MaxDD %:",
                maxDDx,
                "PF:",
                pf
              )
            print(chart_info)
            
            #big_returns
            colnames(my_returns) <-  paste(opt_pass_counter)
            big_returns <- cbind(big_returns, my_returns)
            
            #big_portfolio
            colnames(my_portfolio) <-  paste(opt_pass_counter)
            big_portfolio <- cbind(big_portfolio, my_portfolio)
            
          }
          
          
        }#i
      }#j
  }#k
}#l

####################### Colnames NEED TO BE CHANGE if indexes i,j,k,.. changes
# trace_in_sample colnames
colnames(trace_in_sample) <-
  c(
    "opt_pass_counter",
    "sma",
    "sma_slow",
    "atr",
    "atr_lag",
    "equity",
    "dd",
    "pf",
    "trades" ,
    "win_pct"
  )

# remove duplicates entry form trace_in_sample
no_duplicate_cols <- c("equity", "dd", "pf")
trace_in_sample <-
  trace_in_sample[!duplicated(trace_in_sample[, no_duplicate_cols]),]

# Sort/Head trace operations
trace_in_sample <-
  trace_in_sample[order(trace_in_sample$equity, decreasing = TRUE), ]
trace_in_sample <- head(trace_in_sample, ResultSet)
print(trace_in_sample)

#printable returns / printable_portfolios - selected by winner trace_in_sample$opt_pass_counter -
printable_returns <- printable_portfolios <- NULL
for (x in trace_in_sample$opt_pass_counter)
{
  # select returns
  printable_returns <- cbind(printable_returns, big_returns[, x])
  #select portfolios
  printable_portfolios <-
    cbind(printable_portfolios, big_portfolio[, x])
}

## trace_in_sample ## - Optimization Cloud plot ----------------------------------
#CHARTS
par(family = "Arial")
png(
  file = paste0(STG_NAME, "_", MyTickerName, "_OptTrace.png"),
  width = 1280,
  height = 800
)
par(family = "Arial")
matplot(
  trace_in_sample$equity,
  bg = c("RoyalBlue1"),
  type = c("p"),
  pch = 21:25,
  col = 1:3 ,
  xlab = "n opt pass.",
  ylab = "returns" ,
  main = paste0(STG_NAME, "_", MyTickerName, " Optimization Cloud")
)
text(
  trace_in_sample$equity ,
  labels = trace_in_sample$opt_pass_counter ,
  cex = 0.7 ,
  pos = 3
)
abline(h = OptimizationMinimumReturns, col = "red")
dev.off()


#
png(
  file = paste0(STG_NAME, "_", MyTickerName, "_PerfSummary.png"),
  width = 1280,
  height = 800
)
par(family = "Arial")
charts.PerformanceSummary(printable_returns,
                          main = paste(STG_NAME, "_", MyTickerName, "PerformanceSummary"))
dev.off()
#
detach(package:quantmod, unload = TRUE)
png(
  file = paste0(STG_NAME, "_", MyTickerName, "_RetChart.png"),
  width = 1280,
  height = 800
)
par(family = "Arial")
matplot(
  printable_returns ,
  type = "b",
  pch = 15:18,
  col = rainbow(16),
  main = paste(STG_NAME, "_", MyTickerName, "winners Returns")
)
legend(
  "bottomleft",
  inset = 0.01,
  legend = colnames(printable_returns),
  col = rainbow(16),
  pch = 15:18,
  bg = ("white"),
  horiz = F
)
dev.off()
#
png(
  file = paste0(STG_NAME, "_", MyTickerName, "_PortfoliosChart.png"),
  width = 1280,
  height = 800
)
par(family = "Arial")
matplot(
  printable_portfolios ,
  type = "b",
  pch = 15:18,
  col = rainbow(16),
  main = paste(STG_NAME, "_", MyTickerName, "winners Portfolio")
)
legend(
  "bottomleft",
  inset = 0.01,
  legend = colnames(printable_portfolios),
  col = rainbow(16),
  pch = 15:18,
  bg = ("white"),
  horiz = F
)
dev.off()
