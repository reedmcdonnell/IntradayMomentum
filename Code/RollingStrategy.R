#Clear Workspace
rm(list = ls())

#Source files
source('./libraries.R')
source('./functions.R')

ticker <- 'MSFT'

#Load Data
load('../Data/master.Rda')
data <- master[Ticker == ticker]
rm(master)
data[, Ticker := NULL]

#Master data file
data <- na.omit(data)
data <- data[order(Date)]

#Date subset
data <- data[Date %between% c('2005-01-01', '2015-01-01')]

#Check Date Difference
data[, DayDiff := Date - shift(Date)]
#Check for monday -> 3 difference
data[, Weekday := wday(Date)]

#Remove gaps
data <- data[DayDiff == 1 | (Weekday == 2 & DayDiff == 3)]

#Lookback 1 trading year
k <- 252

final <- data.table()

for(i in k:nrow(data)) {
  date <- as.character(data[i]$Date)
  close <- data[i]$Close
  
  temp <- data[(i-k):i, ]
  #Apply regression predict value for next day
  formula <- paste("R13 ~", paste(setdiff(retCols, 'R13'), collapse = " + "))
  model <- lm(formula, data = temp)
  
  yActual <- data[i + 1, 'R13']
  yPredict <- predict(model, data[i + 1])
  
  toInsert <- data.table(t(c(date, yActual, yPredict, close)))
  names(toInsert) <- c('Date', 'YActual', 'YPredict', 'Close')
  if(nrow(final) < 1) {
    final <- toInsert
  } else {
    final <- rbind(final, toInsert)
  }
}

final <- as.data.table(final)
final[, Date := unlist(Date)]
final[, YActual := unlist(YActual)]
final[, YPredict := unlist(YPredict)]
final[, Close := unlist(Close)]
final <- as.data.table(final)
final[, Date := as.Date(Date)]


#Long/Short Strategy
#Predict value
final[, Strat1Ret := ifelse(YPredict > 0, YActual, -1 * YActual)]
final[, Strat1CumRet := cumprod(1 + Strat1Ret)]

#Random Strategy
final[, RandomSignal := rbinom(.N, 1, .5)]
final[, RandomRet := ifelse(RandomSignal == 1, YActual, -1 * YActual)]
final[, RandomCumRet := cumprod(1 + RandomRet)]

#Benchmark Buy & Hold Strategy
#final[, BenchmarkRet := (Close - shift(Close)) / shift(Close)]
#final[is.na(BenchmarkRet), BenchmarkRet := 0]
#final[, BenchmarkCumRet := cumprod(1 + BenchmarkRet)]

#Benchmark Hold R13 Strategy
final[, BenchmarkRet2 := YActual]
final[is.na(BenchmarkRet2), BenchmarkRet2 := 0]
final[, BenchmarkCumRet2 := cumprod(1 + BenchmarkRet2)]


#Plot strategies
plot <- ggplot() +
  #geom_line(data = final, aes(x = Date, y = BenchmarkCumRet, colour = 'Benchmark Buy&Hold Strategy')) + 
  geom_line(data = final, aes(x = Date, y = Strat1CumRet, colour = 'Long/Short Strategy')) + 
  geom_line(data = final, aes(x = Date, y = RandomCumRet, colour = 'Random Strategy')) + 
  geom_line(data = final, aes(x = Date, y = BenchmarkCumRet2, colour = 'Benchmark R13 Strategy')) + 
  xlab('Dates') + ylab('Return') + ggtitle(paste0(ticker, ' Strategy Return'))
plot(plot)

data[, Rets := (Close - shift(Close)) / Close]

#Plot histogram of excess returns
histogram(na.omit(data$Rets), 'SPY', 'Excess Returns')



