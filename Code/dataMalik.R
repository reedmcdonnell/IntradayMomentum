#Clear workspace
rm(list = ls())

#Source files
source('./libraries.R')
source('./functions.R')

#List files
files = list.files(path = '../../SourceData/', pattern = '*.csv')
for(file in files) {
  print(file)
  
  data <- read.csv(paste0('../../SourceData/', file), stringsAsFactors = FALSE) %>% data.table()
  data[, X0 := NULL]
  names(data) <- c('Ticker', 'Date', 'TIME', 'Size', 'Price')
  data[, DATE := as.Date(as.character(Date), format = '%Y%m%d')]
  
  #Create date column
  data[, Date := as.POSIXct(paste0(DATE, ' ', TIME))]
  
  temp <- loadData(data)
  temp
  
}