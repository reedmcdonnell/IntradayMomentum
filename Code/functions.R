#FUNCTIONS
retCols <- unlist(lapply(seq(13), function(x) paste0('R', x)))
volCols <- unlist(lapply(seq(13), function(x) paste0('V', x)))

#Source libraries
source('./libraries.R')

#Plots histogram for x vector with title and variable title
#TODO: Set y dimensions?
histogram <- function(x, title, varTitle) {
  std <- sd(x)
  avg <- mean(x)
  binWdth <- (max(x) - min(x)) / 100
  qplot(x,
        geom="histogram",
        binwidth=binWdth,
        xlim=c(avg - 3 * std, avg + 3 * std),
        main=title, 
        xlab=varTitle,  
        fill=I("blue"), 
        col=I("red"), 
        alpha=I(.2)) + annotate("text", label = paste0('Mean: ', signif(avg, 4), ', Std: ', signif(std, 4), '\n', 
                                                       'Kurt: ', signif(kurtosis(x), 4), ' Skew: ', signif(skewness(x), 4)),
                                x = avg - 2 * std, y = 200, size = 4, colour = "red")
}

#loadData:
#Inputs:
# ticker: String refering to ticker data set of form 'ticker'.Rda
#         .Rda data file contains minute price history with size for ticker
#Ouput:
# dataframe: Date -> R1, R2, ... R13, V1, V2, ... V13, Vol(avg daily vol), 
#                    inVol(avg daily vol except Vol13), Open, Close
loadData <- function(minuteData) {
  minuteData <- minuteData[order(Date)]
  
  #Remove rows that don't have continuous price history - 391 days
  numOfDailyObs <- length(seq(as.POSIXct("2000-01-01 9:30:00"), as.POSIXct("2000-01-01 16:00:00"), by='min'))
  minuteData <- minuteData[, if (.N == numOfDailyObs) .SD, by = (DATE)]
  
  #Replace 9:30 Price with previous Close Price
  minuteData[, c('Open', 'Close') := list(first(Price), last(Price)), by = DATE]
  minuteData[, Price2 := shift(Price)]
  minuteData[hour(Date) == 9 & minute(Date) == 30, Price := Price2]
  minuteData <- minuteData[, Price2 := NULL]
  minuteData <- na.omit(minuteData)
  minuteData <- minuteData[, if (.N == numOfDailyObs) .SD, by = (DATE)]
  
  #Calculate minute returns by day
  minuteData[, Ret := Price / shift(Price) - 1, by = DATE]
  minuteData <- na.omit(minuteData)
  minuteData[, Ret := shift(Ret, n =-1)]
  
  #Group by 30 minute buckets and calculate return and volatility
  bucketData <- minuteData[, .(DATE = first(DATE), TIME = first(TIME), Open = first(Open), Close = first(Close),
                               Ret = (tail(cumprod(1 + Ret), 1) - 1), Vol = sd(Ret), Size = sum(Size)), 
                           by = cut(Date, '30 min')]
  names(bucketData)[1] <- 'Date'
  bucketData$Date <- as.POSIXct(bucketData$Date)
  
  #Create time group number
  bucketData[, Bucket := .GRP, by = TIME]
  
  retData <- dcast(bucketData, DATE ~ Bucket, value.var = 'Ret')
  #retData[, `14` := NULL]
  names(retData) <- c('DATE', retCols)
  
  volData <- dcast(bucketData, DATE ~ Bucket, value.var = 'Vol')
  #volData[, ncol(volData) := NULL]
  names(volData) <- c('DATE', volCols)
  dates <- volData$DATE
  #Log transformation for volatility: REMOVED BECAUSE NO IMPROVMENT IN REGRESSION FIRST ORDER VOL TERMS
  #volData <- volData[, lapply(.SD, FUN = function(x) log(x)), .SDcols = setdiff(colnames(volData), "DATE")]
  volData$DATE <- dates
  #Average daily vol
  volData[, Vol := rowMeans(.SD), by = 'DATE']
  #Average daily vol not including R13
  volData[, inVol := rowMeans(.SD), .SDcols = setdiff(colnames(volData), c('DATE', 'V13')), by = 'DATE']
  
  sizeData <- dcast(bucketData, DATE ~ Bucket, value.var = 'Size')
  sizeData[, Size := Reduce(`+`, .SD), by = DATE]
  sizeData <- sizeData[, .(DATE, Size)]
  
  openClose <- unique(bucketData[, .(DATE, Open, Close)])
  
  data <- Reduce(function (x, y) merge(x, y, by = c('DATE')), list(retData, volData, sizeData, openClose))
  names(data)[1] <- 'Date'
  
  return(data)
}

#Save master.Rda
buildRdas <- function() {
  print('Building Rda files from source csvs...')
  #master <- data.frame() %>% data.table()
  
  #List files
  files = list.files(path = '../../SourceData/', pattern = '*.csv')
  for(i in 424:length(files)) {
    file <- files[i]
    out <- tryCatch({
      ticker <- strsplit(file, '\\.csv')[[1]]
      print(ticker)
      data <- read.csv(paste0('../../SourceData/', file), stringsAsFactors = FALSE) %>% data.table()
      data[, X0 := NULL]
      names(data) <- c('Ticker', 'Date', 'TIME', 'Size', 'Price')
      data[, DATE := as.Date(as.character(Date), format = '%Y%m%d')]
      
      #Create date column
      data[, Date := as.POSIXct(paste0(DATE, ' ', TIME))]
      
      result <- loadData(data)
      
      #filePath <- paste0('../../SourceData/', ticker, '.Rda' )
      
      save(result, file = paste0('../../SourceData/', ticker, '.Rda' ))
      rm(result)
      
    },
    error = function(cond) {
      print(paste0('ERROR: ', file))
    })
    # if(nrow(master) < 1) {
    #   master <- result
    # } else {
    #   master <- rbind(master, result)
    # }
    rm(out)
  }
  print('Done!')
  #save(master, file = '../Data/master.Rda')
}

buildMasterFile <- function() {
  print('Building Rda master file...')
  master <- data.frame() %>% data.table()
  
  #List files
  files = list.files(path = '../../SourceData/', pattern = '*.Rda')
  for(file in files) {
    ticker <- strsplit(file, '\\.Rda')[[1]]
    print(ticker)
    load(paste0('../../SourceData/', file))
    result[, Ticker := ticker]
    if(nrow(master) < 1) {
      master <- result
    } else {
      master <- rbind(master, result)
    }
  }
  
  save(master, file = '../Data/master.Rda' ) 
  
  print('Done!')
}

#Loop through all source data - NOTE: Session -> Set Working Directory './'
getTickers <- function() {
  tickers <- c()
  
  files = list.files(path = '../ConsolidatedData2/', pattern = '.Rda')
  for(file in files) {
    ticker <- strsplit(file, '\\.')[[1]][1]
    tickers <- c(tickers, ticker)
  }
  
  return(tickers)
}

#Given data frame
#Returns coefR1 coefR5 pValR1 pValR5 adjRSquared residualStdError
#Note: isLookback dictates whether the complete history backwards from a date is used
#      or if date minus lookback data is used
rollRegressFunc <- function(df, isLookback) {
  formula <- paste("R13 ~", paste(setdiff(retCols, 'R13'), collapse = " + "))
  model <- lm(formula, data = df)
  modelSummary <- summary(model)
  coeffs <- modelSummary$coefficients[, 1]
  
  coeffs <- t(coeffs[-c(1)]) %>% data.table()
  coeffs[, Type := 'Coeff']
  
  pVals <- t(coeftest(model, vcov = vcovHC(model))[-1, 4]) %>% data.table()
  names(pVals) <- inVars
  pVals[, Type := 'PVal']
  
  regVars <- rbind(pVals, coeffs)
  regVars[, Date := as.character(max(df$Date))]
  
  #P-Val for F-Statistic
  FStatPVal <- pf(modelSummary$fstatistic[1],modelSummary$fstatistic[2],modelSummary$fstatistic[3])
  
  modelPerf <- data.frame(Date = as.character(max(df$Date)), AdjRSquared = modelSummary$adj.r.squared, 
                          ResidualStdErr = modelSummary$sigma, FStatPVal = FStatPVal, Lookback = isLookback, 
                          stringsAsFactors = FALSE) %>% data.table()
  
  return(list(regVars, modelPerf))
}

#master dataframe input
#Ouput: time-series of statistics of input variables and fitted model performance by ticker
getRollingRegressionData <- function(data, bestVars, ticker) {
  #250 trading days a year * 3 years
  lookback <- 250*3
  
  rollRegCols <- c('Date', bestVars, 'Type')
  rollRegVars <- data.frame()
  for (c in rollRegCols) rollRegVars[[c]] <- as.character()
  rollRegVars <- setDT(rollRegVars)
  rollRegVars[, Lookback := as.logical()]
  
  rollModelPerf <- data.frame(Date = as.character(), AdjRSquared = as.numeric(), 
                              ResidualStdErr = as.numeric(), FStatPVal = as.numeric(),
                              Lookback = as.logical(), stringsAsFactors = FALSE)
  
  #Apply rolling regression
  for(date in data[Date >= min(Date) + lookback, Date]) {
    date <- as.Date(date)
    temp <- rollRegressFunc(data[Date <= date & Date > date - lookback], isLookback = TRUE, bestVars)
    rollRegVars <- rbind(rollRegVars, temp[[1]])
    rollModelPerf <- rbind(rollModelPerf, temp[[2]])
    temp <- rollRegressFunc(data[Date <= date], isLookback = FALSE, bestVars)
    rollRegVars <- rbind(rollRegVars, temp[[1]])
    rollModelPerf <- rbind(rollModelPerf, temp[[2]])
  }
  
  rollRegVars[, Date := as.Date(Date)]
  rollRegVars[, Lookback := as.logical(Lookback)]
  rollRegVars[, (bestVars) := lapply(.SD, as.numeric), .SDcols = bestVars]
  rollModelPerf[, Date := as.Date(Date)]
  
  #Check for too many tickers here...
  
  rollRegVars[, Ticker := ticker]
  rollModelPerf[, Ticker := ticker]
  
  return(list(rollRegVars, rollModelPerf))
}

#Get most significant columns according to Newey-West
#Ignores intercept, needs alpha to enter significance level
getBestVars <- function(data, alphaToEnter) {
  #R13 = B0 + B1*R1 + B2*R2 + ... + B12*R12
  formula <- paste("R13 ~", paste(setdiff(retCols, 'R13'), collapse = " + "))
  model <- lm(formula, data = data)
  
  ## Newey West Robust T-statistic
  #Note: This will not effect R^2 or coefficients - provides adjusted t-statistic
  neweyWest <- coeftest(model, vcov = vcovHC(model))
  
  bestVars <- neweyWest[, 4]
  bestVars <- bestVars[bestVars < alphaToEnter] #5% significance
  if(length(bestVars) < 1) {
    return(c('R1'))
  }
  bestVars <- names(bestVars)
  bestVars <- bestVars[bestVars != "(Intercept)"]
  
  return(bestVars)
}