library(bfast) ##BFAST
library(terra)
library(gtools)
library(sf)
library(bcp) ## Bayesian Change Point v4.0.3
library(Rbeast) ## Bayesian Estimator of Abrupt and Seasonal Trends
library(CausalImpact) ## Bayesian Structural Time Series
library(zoo)
library(lubridate)

replace_na_with_mean <- function(df, obsFreq) {
  ## cast obsFreq to numeric
  obsFreq = as.numeric(obsFreq)
  
  for (i in 1:ncol(df)) {
    na_indices <- which(is.na(df[, i]))
    for (j in na_indices) {
      if (j < (obsFreq +1)) {
        df[j, i] <- mean(c(df[j + obsFreq, i], df[j + (obsFreq*2), i]), na.rm = T) 
      } else if (j >= (obsFreq +1) & j <= (obsFreq*2)){  
        df[j, i] <- mean(c(df[j - obsFreq, i], df[j + obsFreq, i],df[j + (obsFreq*2), i]), na.rm = T)
      } else if (j < nrow(df) - (obsFreq +1) & j > nrow(df) - (obsFreq*2)) {
        df[j, i] <- mean(c(df[j - obsFreq, i], df[j + obsFreq, i],df[j + (obsFreq*2), i]), na.rm = T)
      } else if (j > nrow(df) - (obsFreq +1)) {
        df[j, i] <- mean(c(df[j - obsFreq, i], df[j - (obsFreq*2), i]))
      } else {
        df[j, i] <- mean(c(df[j - (obsFreq*2), i], df[j - obsFreq, i], df[j + obsFreq, i],df[j + (obsFreq*2), i]), na.rm = T)
      }
    }
  }
  return(df)
}

make_plot = function(method, file, ancFiles = NULL, restDate = NULL, obsFreq){
  
  ## read the CSV data
  data = read.csv(file)
  
  ## getting date range and filling missing dates with NA
  dates = as.Date(data$system.time_start, format = "%b %e, %Y")
  #print(month(dates[length(dates)]))
  
  allDates = seq(from = dates[1], to = dates[length(dates)], by = "month" )
  
  summerDates = allDates[month(allDates) %in% seq(6,9)]
  #print(summerDates)
  
  dateNA = setdiff(summerDates, dates)
  #print(missingDates)
  
  ## Insert rows for missing dates
  for (date in dateNA){
    print(date)
    newdate = format(as.Date(date), "%b %e, %Y")
    print(newdate)
    data[nrow(data) +1,] = c(newdate, NA)
  }
  
  ## sort the df
  data = data[order(as.Date(data$system.time_start, format = "%b %e, %Y")),]
  rownames(data) = 1:nrow(data)
  
  ## cast to numeric dtype
  data$mesic = as.numeric(data$mesic)
  
  ## cast obsFreq to numeric
  obsFreq = as.numeric(obsFreq)
  
  ## apply the interpolation function to fill NAs
  data_fill = replace_na_with_mean(data, obsFreq)
  ## include a 'year' variable
  data_fill$year = format(as.Date(data_fill$system.time_start, format = "%b %e, %Y"), "%Y")
  
  print(data_fill)
  
  if(method == "BCP"){
    
    ## univariate
    results_bcp = bcp(data_fill$mesic)
    outplot = plot(results_bcp, main = "BCP output - Mesic probabilities of change",
                   xlab = "Date", xaxlab = data_fill$year )
    
  }
  
  if(method == "BFAST"){
    ## create a time series object
    data_ts = ts(data_fill$mesic, frequency = obsFreq) 
    
    ## apply the BFAST function
    fit = bfast(data_ts, h = 0.15, season = "harmonic")
    outplot = plot(fit, ANOVA = T, main = "BFAST output", xaxt = "n")
  }
  
  if(method == "BEAST"){
    start_date = as.Date(data_fill$date[1], format = "%b %e, %Y")
    results_beast = beast(data_fill$mesic, start = start_date, deltat = (obsFreq/12), dump.ci = T)
    
    outplot = plot(results_beast, interactive = F) 
  }
  
  
  if(method == "BSTS"){
    pred_count = length(ancFiles) ## for multiple predictors - come back to this 
    
    for(i in pred_count){
      ## make a variable with the values
    } 
    ## hard code drought for now
    drought = read.csv(ancFiles)
    print(drought)
    
    ## remove 2016 to have corresponding values with the time series
    drought$year = as.numeric(format(as.Date(drought$system.time_start, format = "%b %e, %Y"), "%Y"))
    #drought_no2016 = subset(drought, year != 2016)
    
    ## Include PDSI as a predictor
    data_bsts_time =  zoo(cbind(data_fill$mesic, drought[,2]), 
                          as.Date(data_fill$system.time_start,format = "%b %e, %Y")) ## includes missing data period
    
    print(data_bsts_time)
    
    ### define pre and post restoration period 
    restIndex = which(data_fill$year == restDate)[obsFreq]
    #print(restDate)
    print(restIndex)
    
    pre_period_date = as.Date(c(index(data_bsts_time[1]), index(data_bsts_time[restIndex])),format = "%Y-%m-%d")
    post_period_date = as.Date(c(index(data_bsts_time[restIndex + 1]), index(data_bsts_time[length(data_fill[,1])])), format = "%Y-%m-%d") 
    
    print(pre_period_date, post_period_date)
    
    impact_time = CausalImpact(data_bsts_time, pre_period_date, post_period_date, 
                               model.args = list(nseasons = obsFreq))

    outplot = tryCatch(
      
      {plot(impact_time, c("original", "pointwise")) +
      ggtitle("BSTS output") +
      ylab("Mesic Vegetation Area (% of valley bottom)") +
      xlab("Date")#+
      #theme(axis.title = element_text(size = 10))
      }, error = function(msg){
        return("Please check the restoration date")
      }
    )
  }
  
  return(outplot)
}


make_sum = function(method, file, ancFiles = NULL, restDate = NULL, obsFreq){

  ## read the CSV data
  data = read.csv(file)
  
  ## getting date range and filling missing dates with NA
  dates = as.Date(data$system.time_start, format = "%b %e, %Y")
  #print(month(dates[length(dates)]))
  
  allDates = seq(from = dates[1], to = dates[length(dates)], by = "month" )
  
  summerDates = allDates[month(allDates) %in% seq(6,9)]
  #print(summerDates)
  
  dateNA = setdiff(summerDates, dates)
  #print(missingDates)
  
  ## Insert rows for missing dates
  for (date in dateNA){
    print(date)
    newdate = format(as.Date(date), "%b %e, %Y")
    print(newdate)
    data[nrow(data) +1,] = c(newdate, NA)
  }
  
  ## sort the df
  data = data[order(as.Date(data$system.time_start, format = "%b %e, %Y")),]
  rownames(data) = 1:nrow(data)
  
  ## cast to numeric dtype
  data$mesic = as.numeric(data$mesic)
  
  ## cast obsFreq to numeric
  obsFreq = as.numeric(obsFreq)
  
  ## apply the interpolation function to fill NAs
  data_fill = replace_na_with_mean(data, obsFreq)
  ## include a 'year' variable
  data_fill$year = format(as.Date(data_fill$system.time_start, format = "%b %e, %Y"), "%Y")
  
  print(data_fill)
  
  if(method == "BCP"){
    
    ## univariate
    results_bcp = bcp(data_fill$mesic)
    dates = data_fill$system.time_start
    probs = results_bcp$posterior.prob
    df = cbind(dates, probs)
    df = as.data.frame(df)
    colnames(df) = c("date", "postProb")
    
    outdf = df[order(df$postProb, decreasing = T),]
    outdf = head(outdf, 3)
    outdates = as.list(outdf$date)
    outprobs = as.list(outdf$postProb)
    outsum = paste0("The highest probability of change (", outprobs[1],") occurred in ", outdates[1], ".")
    
    print(outsum) 
    
  }
  
  if(method == "BFAST"){
    ## create a time series object
    data_ts = ts(data_fill$mesic, frequency = obsFreq) 
    
    ## apply the BFAST function
    fit = bfast(data_ts, h = 0.15, season = "harmonic")
    print(summary(fit)) ## fit$Time returns the breakpoint - translate to date?
    
    dates = data_fill$system.time_start
    #print(dates[fit$Time][1])
    print(dates[fit$Time])
    if(is.na(dates[fit$Time][1]) == TRUE){
      outsum = paste0("No breakpoints detected")
    }
    else{outsum = paste0("A breakpoint was detected at ", dates[fit$Time],".")}
  }
  
  if(method == "BEAST"){
    start_date = as.Date(data_fill$date[1])
    results_beast = beast(data_fill$mesic, start = start_date, deltat = obsFreq/12, dump.ci = T)
    
    print(results_beast$trend$cp) ## points in time where tcp occurs
    print(results_beast$trend$cpPr)
    print(results_beast$season$cp)
    print(results_beast$season$cpPr)
    #outsum = summary(results_beast)## figure out something useful here - https://rdrr.io/cran/Rbeast/man/beast.html
    
    if(is.numeric(results_beast$trend$ncp_median) == TRUE){
      numtrend = results_beast$trend$ncp_median
    }
    else{numtrend = 0}
    
    if(is.numeric(results_beast$season$ncp_median) == TRUE){
      numseas = results_beast$season$ncp_median
    }
    else{numseas = 0}
    
    outsum = paste0("Number of trend changepoints: ", numtrend,
                    "\nNumber of seasonal changepoints: ", numseas)
    
  }

  if(method == "BSTS"){
    pred_count = length(ancFiles) ## for multiple predictors - come back to this 
    
    for(i in pred_count){
      ## make a variable with the values
    } 
    ## hard code drought for now
    drought = read.csv(ancFiles)
    print(drought)
    
    ## remove 2016 to have corresponding values with the time series
    drought$year = as.numeric(format(as.Date(drought$system.time_start, format = "%b %e, %Y"), "%Y"))
    
    ## Include PDSI as a predictor
    data_bsts_time =  zoo(cbind(data_fill$mesic, drought[,2]), 
                          as.Date(data_fill$system.time_start,format = "%b %e, %Y")) ## includes missing data period
    
    print(data_bsts_time)
    
    ### define pre and post restoration period 
    restIndex = which(data_fill$year == restDate)[obsFreq]
    #print(restDate)
    print(restIndex)

    pre_period_date = as.Date(c(index(data_bsts_time[1]), index(data_bsts_time[restIndex])),format = "%Y-%m-%d")
    post_period_date = as.Date(c(index(data_bsts_time[restIndex + 1]), index(data_bsts_time[length(data_fill[,1])])), format = "%Y-%m-%d") 
    
    print(pre_period_date, post_period_date)
    
    impact_time = CausalImpact(data_bsts_time, pre_period_date, post_period_date, 
                               model.args = list(nseasons = obsFreq))
    
    summary(impact_time) 
    ## short version of the summary$report
    tempsum = strsplit(impact_time$report, split = ". ", fixed = T)[[1]][1:6]
    outsum = paste0(tempsum[1], ". ", tempsum[2], ". ", tempsum[3], 
                    ". ", tempsum[4], ". ", tempsum[5], ". ", tempsum[6], ". ") 
    print(outsum)
  }
  
  return(outsum)
}

