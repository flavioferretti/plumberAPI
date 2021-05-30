#!/usr/bin/env Rscript
db='PriceDB'
host='192.168.4.1'
user='flavio'
pw='27182818'
library(RMariaDB)
library(quantmod)
library(dplyr)
### ############# ############### `EURUSD=X` ############# ############# ####
if (length(args)==0) {
  stop("arg[1]=( Symbol p.es EURUSD=X ) arg[2]=( DB Table p.es EURUSDy )", call.=FALSE)
}  

#symbol=

getSymbols(Symbols = args[1],warnings = FALSE,src='yahoo',from='1990-01-01' ,return.class='xts' )
MyTickerName <- .Last.value # keep them
###
x<-data.frame(`MyTickerName`)
x<-na.omit(x)

#debug
#tail(x[1])

x$date<-rownames(x)
rownames(x)<-NULL
### change column names for comatibility with getSymbols()
x <- x %>%
  rename(
    date = x[,0],
    o 	 = x[,1],
    h 	 = x[,2],
    l 	 = x[,3],
    c 	 = x[,4],
    v 	 = x[,5],
    a 	 = x[,6],
  )
###

head(x)
tail(x)
### mariaDB ###
db <- dbConnect(RMariaDB::MariaDB(), user=user, password=pw, dbname=db, host=host)
dbWriteTable(db, value = x, row.names = FALSE, name = args[2], overwrite = TRUE  )
dbDisconnect(db)
###
rm(x)
###
