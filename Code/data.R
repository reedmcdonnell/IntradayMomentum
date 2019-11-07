###Edit data sourced - should be done on their end
data <- read.csv('../../SourceData/price.csv', stringsAsFactors = FALSE) %>% data.table()
names(data) <- c('Ticker', 'Date', 'TIME', 'Price', 'Size')
data[, Date := as.Date(Date, format = '%d-%m-%Y')]

#Remove seconds from time
data[, TIME := paste0(substr(TIME, 0, 6), '00')]

#Create date columns
data[, Date := as.POSIXct(paste0(Date, ' ', TIME))]
data[, DATE := as.Date(Date)]

#Write final data used in process
save(data, file = '../FinalData/prices.Rda')