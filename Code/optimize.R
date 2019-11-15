#Clear Workspace
rm(list = ls())

#Source files
source('./libraries.R')
source('./functions.R')

load('../Data/master.Rda')

#Master data file
master <- na.omit(master)
master <- master[order(Date, Ticker)]

#Date subset
master <- master[Date %between% c('2005-01-01', '2015-01-01')]

#Remove >1 day gaps (includes monday)
master[, DayDiff := Date - shift(Date), by = 'Ticker']
master[, Weekday := wday(Date)]
master <- master[DayDiff == 1 | (Weekday == 2 & DayDiff == 3)]

#Lookback 1 trading year
k <- 252

#Dates
dates <- unique(master$Date)
dates <- dates[dates >= '2007-01-01']
final <- data.table()

#date <- as.Date('2007-01-10')

portfolio_returns = function(x) {
  port.returns = 0
  
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + returns[,i] * x[i]
  }
  
  return (port.returns)
}

sharpe = function(x) {
  port.returns = portfolio_returns(x)
  
  return (mean(port.returns)/sqrt(var(port.returns)))
  
}

constraint = function(x) {
  boundary_constr = (sum(x)-1)**2   # "sum x = 1" constraint
  
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr + 
      max(c(0,x[i]-1))**2 +  # "x <= 1" constraint
      max(c(0,-x[i]))**2     # "x >= 0" constraint
  }
  
  return (boundary_constr)
}

obj = function(x) {
  # We want the maximum Sharpe ratio, so we multiply it by
  # -1 to fit an optimization problem
  
  return (-sharpe(x)+100*constraint(x))
}

for(date in dates) {
  start_time <- Sys.time()
  
  #Print date
  date <- as.Date(date)
  print(date)
  
  tickers <- master[Date == date]$Ticker
  
  #Subset lookback period
  temp <- master[Date %between% c(date - k, date)]
  #Only include tickers that have enough observations
  temp[, Count := .N, by = Ticker]
  temp <- temp[Count > 126]
  
  tickers <- intersect(tickers, unique(temp$Ticker))
  
  temp <- temp[Ticker %in% tickers]
  
  if(nrow(temp) == 0) {
    next
  }
  
  #Returns
  df <- dcast(temp, Date ~ Ticker, value.var = 'R13')
  df <- na.omit(df)
  returns <- df[, -'Date']
  returns <- as.data.frame(returns)
  
  #weights
  x <- rep(1/(length(df[, -'Date']) - 1), length(df[, -'Date']) - 1)
  
  ga_res = ga(
    # Tell the genetic algorithm that the 
    # weights are real variables
    type="real-valued", 
    
    # "ga" function performs maximization, so we must
    # multiply the objective function by -1
    function(x){-obj(x)}, 
    
    # x_i >= 0
    lower = rep(0,ncol(returns)), 
    
    # x_i <= 1
    upper = rep(1,ncol(returns)), 
    
    # Maximum number of iterations 
    maxiter = 50000, 
    
    # If the maximum fitness remains the same for 50
    # consecutive transactions, stop the algorithm
    run=50, 
    
    # Exploit multi-core properties of your CPU
    parallel=TRUE
    
    # We want to see the partial results of the process
    # while it performs
    #,monitor=TRUE
    
    # Seed useful for replicating the results
    ,seed=1
    
    ,monitor = FALSE
  )
  
  sol = as.vector(summary(ga_res)$solution)
  
  yBench <- mean(master[Date == date & Ticker %in% tickers]$R13)
  
  close <- mean(master[Date == date & Ticker %in% tickers]$Close)
  
  yPort <- sol %*% master[Date == date & Ticker %in% tickers]$R13
  
  toInsert <- data.table(t(c(as.character(date), yBench, yPort, close)))
  names(toInsert) <- c('Date', 'YBench', 'YPort', 'Close')
  if(nrow(final) < 1) {
    final <- toInsert
  } else {
    final <- rbind(final, toInsert)
  }
  end_time <- Sys.time()
  print(end_time - start_time)
}

final[, Date := as.Date(Date)]
cols <- c('YBench', 'YPort', 'Close')
final[, (cols) := list(as.numeric(YBench), as.numeric(YPort), as.numeric(Close)) ]

#Long/Short Strategy
#Predict value
final[, Strat1Ret := YPort]
final[, Strat1CumRet := cumprod(1 + Strat1Ret)]

#Random Strategy
final[, RandomSignal := rbinom(.N, 1, .5)]
final[, RandomRet := ifelse(RandomSignal == 1, yBench, -1 * yBench)]
final[, RandomCumRet := cumprod(1 + RandomRet)]

#Benchmark Buy & Hold Strategy
#final[, BenchmarkRet := (Close - shift(Close)) / shift(Close)]
#final[is.na(BenchmarkRet), BenchmarkRet := 0]
#final[, BenchmarkCumRet := cumprod(1 + BenchmarkRet)]

#Benchmark Hold R13 Strategy
final[, BenchmarkRet2 := YBench]
final[, BenchmarkCumRet2 := cumprod(1 + BenchmarkRet2)]

#Plot strategies
plot <- ggplot() +
  #geom_line(data = final, aes(x = Date, y = BenchmarkCumRet, colour = 'Benchmark Buy&Hold Strategy')) + 
  geom_line(data = final, aes(x = Date, y = Strat1CumRet, colour = 'Long/Short Strategy')) + 
  geom_line(data = final, aes(x = Date, y = RandomCumRet, colour = 'Random Strategy')) + 
  geom_line(data = final, aes(x = Date, y = BenchmarkCumRet2, colour = 'Benchmark R13 Strategy')) + 
  xlab('Dates') + ylab('Return') + ggtitle(paste0('Portfolio', ' Strategy Return'))
plot(plot)



