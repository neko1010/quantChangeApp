library(bfast) ##BFAST
library(terra)
library(gtools)
library(sf)
library(bcp) ## Bayesian Change Point v4.0.3
library(Rbeast) ## Bayesian Estimator of Abrupt and Seasonal Trends
library(CausalImpact) ## Bayesian Structural Time Series
library(zoo)

## function to interpolate with vals from same month in preceding and following years
replace_na_with_mean <- function(df) {
  for (i in 1:ncol(df)) {
    na_indices <- which(is.na(df[, i]))
    for (j in na_indices) {
      if (j == 1) {
        df[j, i] <- df[j + 4, i]
      } else if (j == nrow(df)) {
        df[j, i] <- df[j - 4, i]
      } else {
        df[j, i] <- mean(c(df[j - 4, i], df[j + 4, i]))
      }
    }
  }
  return(df)
}

make_sum = function(method, file, ancFiles = NULL, restDate = NULL){
#make_plot = function(method, file, date = NULL){
  ## read the CSV data
  data = read.csv(file)
  #data = df
  
  ## Insert a row for July 2019
  data[nrow(data) +1,] = c("July 1, 2019", NA)
  
  ## sort the df
  data = data[order(as.Date(data$system.time_start, format = "%b %e, %Y")),]
  rownames(data) = 1:nrow(data)
  
  ## cast to numeric dtype
  data$mesic = as.numeric(data$mesic)
  
  ## apply the interpolation function to fill NAs
  data_fill = replace_na_with_mean(data)
  
  if(method == "BCP"){
    ## include a 'year' variable
    data_fill$year = format(as.Date(data_fill$system.time_start, format = "%b %e, %Y"), "%Y")
    
    ## univariate
    results_bcp = bcp(data_fill$mesic)
    outsum = results_bcp$posterior.prob
    outplot = plot(results_bcp, main = "BCP output - Mesic probabilities of change",
         xlab = "Date", xaxlab = data_fill$year ) ## FIGURE 3
    
  }
  
  if(method == "BFAST"){
    ## create a time series object
    data_ts = ts(data_fill$mesic, frequency = 4) ## update to be flexible to # of obs/cycle
    
    ## apply the BFAST function
    fit = bfast(data_ts, h = 0.15, season = "harmonic")
    outsum = summary(fit)
    outplot = plot(fit, ANOVA = T, main = "BFAST output", xaxt = "n")
  }
  
  if(method == "BEAST"){
    start_date = as.Date(data_fill$date[1])
    deltat = 4/12
    #results_beast = beast(data_fill$mesic, start = start_Date, deltat = "3 months", dump.ci = T)
    results_beast = beast(data_fill$mesic, start = as.Date('2004-6-1'), deltat = "3 months", dump.ci = T)
    #outplot = plot(results_beast, interactive = T) 
    
    outsum = c(results_beast$trend, results_beast$season)
    #outplot = plot(results_beast, interactive = F) ## MAKE DIMS BIGGER 
  }
  
    
  if(method == "BSTS"){
    pred_count = length(ancFiles)
    
    for(i in pred_count){
      ## make a variable with the values
    } 
    ## hard code pdsi for now
    pdsi = read.csv(ancFiles)
    
    ## Include PDSI as a predictor
    #data_bsts=  cbind(data_fill$mesic, pdsi_no2016$pdsi)
    data_bsts_time =  zoo(cbind(data_fill$mesic, pdsi$pdsi), 
                          as.Date(data_fill$system.time_start,format = "%b %e, %Y")) ## includes missing data period
    
   # ## define pre and post restoration periods - MAKE DYNAMIC!
   # pre_period = c(1,32)
   # post_period = c(33, 64)
    
    ### define pre and post restoration period
    index = which(data$Year == date)[4]
    
    pre_period = c(1,index)
    post_period = c(index + 1, length(data_bsts_time))
    
    ## with date
    rest_date = date
    
    pre_period_date = as.Date(c("Jun 1, 2004", "Sep 1,2011"), format = "%b %e, %Y")
    post_period_date = as.Date(c("Jun 1, 2012", "Sep 1,2020"), format = "%b %e, %Y")
    
    #impact = CausalImpact(data_bsts, pre_period, post_period, model.args = list(nseasons = 4))
    ## MAKE nseasons selectable!!!!
    impact_time = CausalImpact(data_bsts_time, pre_period_date, post_period_date, 
                               model.args = list(nseasons = 4))
    
    #summary(impact) 
    outsum = summary(impact_time) ## nearly identical results AND includes period of missing data...
    
    outplot = plot(impact_time) 
    }
  
  return(outsum)
}

make_plot = function(method, file, ancFiles = NULL, restDate = NULL){
  #make_plot = function(method, file, date = NULL){
  ## read the CSV data
  data = read.csv(file)
  #data = df
  
  ## Insert a row for July 2019
  data[nrow(data) +1,] = c("July 1, 2019", NA)
  
  ## sort the df
  data = data[order(as.Date(data$system.time_start, format = "%b %e, %Y")),]
  rownames(data) = 1:nrow(data)
  
  ## cast to numeric dtype
  data$mesic = as.numeric(data$mesic)
  
  ## apply the interpolation function to fill NAs
  data_fill = replace_na_with_mean(data)
  
  ## include a 'year' variable
  data_fill$year = format(as.Date(data_fill$system.time_start, format = "%b %e, %Y"), "%Y")
  if(method == "BCP"){
    
    ## univariate
    results_bcp = bcp(data_fill$mesic)
    outsum = summary(results_bcp)
    outplot = plot(results_bcp, main = "BCP output - Mesic probabilities of change",
                   xlab = "Date", xaxlab = data_fill$year ) ## FIGURE 3
    
  }
  
  if(method == "BFAST"){
    ## create a time series object
    data_ts = ts(data_fill$mesic, frequency = 4) ## update to be flexible to # of obs/cycle
    
    ## apply the BFAST function
    fit = bfast(data_ts, h = 0.15, season = "harmonic")
    outsum = summary(fit)
    outplot = plot(fit, ANOVA = T, main = "BFAST output", xaxt = "n")
  }
  
  if(method == "BEAST"){
    start_date = as.Date(data_fill$date[1])
    deltat = 4/12
    #results_beast = beast(data_fill$mesic, start = start_Date, deltat = "3 months", dump.ci = T)
    results_beast = beast(data_fill$mesic, start = as.Date('2004-6-1'), deltat = "3 months", dump.ci = T)
    #outplot = plot(results_beast, interactive = T) 
    
    outsum = summary(results_beast)
    outplot = plot(results_beast, interactive = F) ## MAKE DIMS BIGGER 
  }
  
  
  if(method == "BSTS"){
    pred_count = length(ancFiles) ## for multiple predictors - come back to this 
    
    for(i in pred_count){
      ## make a variable with the values
    } 
    ## hard code pdsi for now
    pdsi = read.csv(ancFiles)
    print(pdsi)
    
    ## remove 2016 to have corresponding values with the time series
    pdsi$year = as.numeric(format(as.Date(pdsi$system.time_start, format = "%b %e, %Y"), "%Y"))
    pdsi_no2016 = subset(pdsi, year != 2016)
    
    ## Include PDSI as a predictor
    #data_bsts=  cbind(data_fill$mesic, pdsi_no2016$pdsi)
    data_bsts_time =  zoo(cbind(data_fill$mesic, pdsi_no2016$pdsi), 
                          as.Date(data_fill$system.time_start,format = "%b %e, %Y")) ## includes missing data period
    
    print(data_bsts_time)
    # ## define pre and post restoration periods - MAKE DYNAMIC!
    #pre_period = c(1,32)
    #post_period = c(33, 64)
    
    ### define pre and post restoration period - MAKE DYNAMIC
    restIndex = which(data_fill$year == restDate)[4]
    #print(restDate)
    print(restIndex)

    #pre_period_date = as.Date(c("2004-06-01", "2012-09-01"), format = "%Y-%m-%d")
    #post_period_date = as.Date(c("2013-06-01", "2020-09-01"), format = "%Y-%m-%d")

    pre_period_date = as.Date(c(index(data_bsts_time[1]), index(data_bsts_time[restIndex])),format = "%Y-%m-%d")
    post_period_date = as.Date(c(index(data_bsts_time[restIndex + 1]), index(data_bsts_time[length(data_fill[,1])])), format = "%Y-%m-%d") 
    
    print(pre_period_date, post_period_date)
    
    #impact = CausalImpact(data_bsts, pre_period, post_period, model.args = list(nseasons = 4))
    ## MAKE nseasons selectable!!!!
    impact_time = CausalImpact(data_bsts_time, pre_period_date, post_period_date, 
                               model.args = list(nseasons = 4))
    
    summary(impact_time) 
    outsum = summary(impact_time) 
    
    outplot = plot(impact_time) 
  }
  
  return(outplot)
}
