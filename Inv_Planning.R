library(xts)
library(readxl)
library(plyr)
require(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forecast)
# importFrom(dplyr,"%>%")

checkData <- function(data){
  stopifnot(dim(data)[2] == 16,
            is.factor(data$Author),
            is.factor(data$Isbn),
            is.numeric(data$Ean),
            is.factor(data$Title),
            is.factor(data$Pub.Date),
            is.factor(data$Medium),
            is.factor(data$Ans.Code),
            is.integer(data$Available),
            is.integer(data$Consignment.On.Hand),
            is.integer(data$Reserved.Stock),
            is.integer(data$Sets.Made.Up),
            is.integer(data$Month),
            is.integer(data$Gross.Units),
            is.integer(data$Return.Units),
            is.factor(data$Prev.Edition.Isbn),
            is.numeric(data$Prev.Edition.Ean))
}

cleanImportedData <- function(data){
  # prepares imported data for analysis
  
  valid_column_names <- make.names(names=names(data), unique=TRUE, allow_ = TRUE)
  names(data) <- valid_column_names
  
  data$Title <- gsub("..", ".", make.names(data$Title, allow_ = TRUE), fixed = TRUE)
  data$Author <- gsub("..", ".", make.names(data$Author, allow = TRUE), fixed = TRUE)
  data$Ean <- as.factor(as.character(data$Ean))
  data$Prev.Edition.Ean <- as.factor(as.character(data$Prev.Edition.Ean))
  data$Pub.Date <- as.Date(as.character(data$Pub.Date), format = "%m/%d/%Y")
  # data$Month <- as.Date(as.character(data$Month), format = "%Y%m")
  
  data$Month = as.Date(paste(as.character(data$Month), "01", sep = ""), format = "%Y%m%d")
  
  data <- unique(data)  # Eliminate duplicates
  
  return(data)
}



createTitleDF <- function(data){
  # creates a title metadata dataframe
  
  title.data = data %>% dplyr::select(Author, Isbn, Ean, Title, Pub.Date, Medium, Ans.Code, Available, Consignment.On.Hand, Reserved.Stock, Sets.Made.Up, Prev.Edition.Isbn, Prev.Edition.Ean) %>%
    distinct(Author, Isbn, Ean, Title, Pub.Date, Medium, Ans.Code, Available,
             Consignment.On.Hand, Reserved.Stock, Sets.Made.Up, Prev.Edition.Isbn, Prev.Edition.Ean)
}

createTSDF <- function(data){
  # Creates a time series data frame from the source data. This DF does not contain
  # any forecasted information.
  
  demand = data %>% dplyr::select(Month, Isbn, Gross.Units) 
  
  demand = tidyr::spread(demand, key = Isbn, value = Gross.Units)
  
  # Get the earliest month in order to align dates
  
  earliest.date = as.Date(min(demand$Month))
  
  rownames(demand) <- demand$Month
  demand3 = demand[,-1] # Remove the Month column since it's now rownames
  
  # Get the earliest month
  demand4 = ts(demand3, start = c(year(earliest.date), month(earliest.date)), frequency = 12)
  
  return(demand4)
  
}

previous.edition.isbn <- function(isbn, title.df){
  # Returns the previous edition ISBN.  Takes ISBN as a string, and a data frame
  # as an input and returns the previous ISBN as a string, or NA if no previous
  # ISBN is available.
  result <- as.character(title.df[title.df$Isbn == isbn, "Prev.Edition.Isbn"])
  if (result == "")
  {result = NA}
  
  return(result)
}

sum.edition.data <- function(isbn, time.series.data, title.data){
  # Takes an ISBN as a string and two data frames: title data and title
  # time series as inputs and returns a new time series with the summed
  # values from that ISBN and its previous edition. If there is no previous edition, 
  # it returns the data from the first edition only
  
  # Get ISBN data
  title.1 <- time.series.data[,isbn]
  
  # Find previous edition
  isbn.prev <- previous.edition.isbn(isbn, title.data) # Get previous edition
  
  # If there's no previous edition return the first title's data
  
  if (is.na(isbn.prev)) {
    return(title.1)
  }
  # Fail if previous edition has no data (not yet implemented)
  
  # Get previous edition data
  title.2 <- time.series.data[,isbn.prev]
  
  # Combine the two titles
  title.1 <- zoo::na.trim(title.1, sides = "both", is.na = "all") # trim NAs
  title.2 <- zoo::na.trim(title.2, sides = "both", is.na = "all") # trim NAs
  
  temp = merge.zoo(title.1, title.2)
  temp[is.na(temp)] <- 0
  title.3 = temp[,1] + temp[,2]
  title.3 = ts(title.3, start = start(title.3), frequency = 12)
  title.3[title.3 < 0] <- 0
  
  return(title.3)
}

n.month.forecast <- function(isbn, time.series.data, horizon){
  # Generates a forecast over the specified horizon for a particular isbn.
  # Takes an isbn as a string, a time series dataframe (to look up the data),
  # and the specified horizon.  Returns a single column dataframe of the
  # forecast
  
  ts1 = time.series.data[, isbn] # get title demand
  
  # if there's more than 24 months of data, generate forecast,
  # else generate a zero forecast to drive a manual review.
  # This needs to generate the forecast to see if it's flat.
  # If it's flat, replace it with a zero forecast to drive
  # a manual review.
  if(sum(!is.na(ts1)) < 25){  
    ts1.temp = ts(rep(0,length(ts1)), start = start(ts1), frequency = frequency(ts1))
    ts1.fcst = forecast(ts1.temp, h=horizon)$mean
  }
  else{
    ts1.fcst = forecast(ts1, h=horizon)$mean}
  
  # return(ts1.fcst)
  
  # Try returning a dataframe instead of an ts object
  ts1.fcst.df <- data.frame(Y = coredata(ts1.fcst))
  rownames(ts1.fcst.df) <- as.Date(as.yearmon(time(ts1.fcst)))
  colnames(ts1.fcst.df) <- as.character(isbn)
  return(ts1.fcst.df)

}

build.forecast.DF <- function(title.data, time.series.data){
  # Build df of forecasts along with 3-15 month accumulations. Save to rds and CSV,
  # returns the forecast df.
  
  
  test_titles = head(title.data$Isbn) # Select a subset of titles to run

  
  title.forecasts <- llply(test_titles, n.month.forecast, time.series.data, 15)
  title.forecasts <- do.call(cbind, title.forecasts)
  
  dates <- rownames(title.forecasts)
  isbns <- colnames(title.forecasts)
  title.forecasts <- data.frame(t(title.forecasts))
  colnames(title.forecasts) <- dates

  # Add cumulative forecast quantities. Note that titles are in rows!
  
  title.forecasts <- title.forecasts %>% mutate(three = rowSums(.[1:3])) %>%
    mutate(six = rowSums(.[1:6])) %>%
    mutate(nine = rowSums(.[1:9])) %>%
    mutate(twelve = rowSums(.[1:12])) %>%
    mutate(fifteen = rowSums(.[1:15]))
  
  rownames(title.forecasts) <- isbns
  
  saveRDS(title.forecasts, "forecasts.rds")
  write.csv(file="Title_forecasts.csv", x=title.forecasts)
  return(title.forecasts)
}
