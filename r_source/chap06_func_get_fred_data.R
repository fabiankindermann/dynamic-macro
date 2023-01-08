#######################################
### A function to pull multiple data series from FRED
#######################################

# load fredr library
library(fredr)

# In order to be able to download data from FRED, you have to go to
#     https://fred.stlouisfed.org/docs/api/api_key.html
# Create an account and request an API key for downloading data. Then 
# uncomment the statement above and insert your API key.
# All data series available in FRED can be investigated at 
#     https://fred.stlouisfed.org.

#fredr_set_key("type-you-key")

# a function to pull multiple data series from FRED
get_fred_data <- function(series, names, start_date, end_date, freq) {
  
  # extract first data series
  tdat   <- fredr(series_id = series[1],
                  observation_start = as.Date(start_date),
                  observation_end = as.Date(end_date),
                  frequency = freq
  )
  
  # start a new data frame and add first series
  data <- data.frame(date = tdat$date)
  data[names[1]] <- tdat$value
  
  # now iterate over all series and add them
  for(i in 2:length(series)) {
    print(paste("Loading series", series[i]))
    tdat   <- fredr(series_id = series[i],
                    observation_start = as.Date(start_date),
                    observation_end = as.Date(end_date),
                    frequency = freq
    )
    data[names[i]] <- tdat$value
  }
  
  if(freq == "q") {
    data["quarter"] <- c(1:length(unlist(data[names[1]]))) 
  } else if(freq == "a") {
    data["year"] <- c(1:length(unlist(data[names[1]]))) 
  } else {
    data["month"] <- c(1:length(unlist(data[names[1]]))) 
  }
  
  # return the final data frame
  return(data)
}