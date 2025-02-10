#' Get Quotes for specified tickers in List form
#'
#' Enter tickers for real time or delayed quotes returned as a list
#'
#' Quotes may be delayed depending on agreement with Schwab. If the
#' account is set up for real-time quotes then this will return real-time.
#' Otherwise the quotes will be delayed.
#'
#' @param tickers One or more tickers
#' @param output indication on whether the data should be returned as a list or
#'   df. The default is 'df' for data frame, anything else would be a list.
#' @inheritParams schwab_accountData
#'
#' @return a list or data frame with quote details for each valid ticker
#'   submitted
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get stored refresh token
#' refreshToken = readRDS('/secure/location/')
#'
#' # generate a new access token
#' accessToken = schwab_auth_accessToken('consumerKey', refreshToken)
#'
#' # Pass one or more tickers as a vector
#' # accessToken is optional once it is set
#' quoteSPY = schwab_priceQuote('SPY')
#' quoteList = schwab_priceQuote(c('GOOG','TSLA'), output = 'list', accessToken)
#'
#' }
schwab_priceQuote = function(tickers = c('AAPL','MSFT'), output = 'df', accessTokenList=NULL) {

  # Check output desired and pass to helper function
  if (output != 'df') {
    # If not data frame, assume list
    quotes = schwab_quote_list(tickers, accessTokenList)
  } else {
    # If df then return data frame
    quotes = schwab_quote_df(tickers, accessTokenList)
  }

  return(quotes)
}





#' Get price history for a multiple securities
#'
#' Open, Close, High, Low, and Volume for one or more securities
#'
#' Pulls price history for a list of security based on the parameters that
#' include a date range and frequency of the interval. Depending on the
#' frequency interval, data can only be pulled back to a certain date. For
#' example, at a one minute interval, data can only be pulled for 30-35 days.
#' Prices are adjusted for splits but not dividends.
#'
#' PLEASE NOTE: Large data requests will take time to pull back because of the
#' looping nature. The 'Schwab API' does not allow bulk ticker request, so this
#' is simply running each ticker individually. For faster and better historical
#' data pulls, try the 'Tiingo API' or 'FMP Cloud API'
#'
#' @param tickers a vector of tickers - no more than 15 will be pulled. for
#'   bigger requests, split up the request or use the 'Tiingo API', 'FMP Cloud API',
#'   or other free data providers
#' @param startDate the Starting point of the data
#' @param endDate the Ending point of the data
#' @param freq the frequency of the interval. Can be daily, 1min, 5min, 10min,
#'   15min, or 30min
#' @inheritParams schwab_accountData
#'
#'
#' @return a tibble of historical price data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Set the access token and a provide a vector of one or more tickers
#' refreshToken = readRDS('/secure/location/')
#' accessToken = schwab_auth_accessToken(refreshToken, 'consumerKey')
#' tickHist5min = schwab_priceHistory(c('TSLA','AAPL'), freq='5min')
#'
#' # The default is daily. Access token is optional once it's been set
#' tickHistDay = schwab_priceHistory(c('SPY','IWM'), startDate = '1990-01-01')
#'
#' }
schwab_priceHistory = function(tickers=c('AAPL','MSFT'),startDate=Sys.Date()-30,endDate=Sys.Date(),
                              freq=c('daily','1min','5min','10min','15min','30min'),
                              accessTokenList=NULL){

  # Limit request to first 15 tickers
  if (length(tickers)>15) {
    tickers = tickers[1:15]
    warning('More than 15 tickers submitted. Only the first 15 tickers were pulled from the list of tickers.')
  }

  if (missing(freq)) freq='daily'

  # Loop through all tickers and collapse into a single data frame
  allTickers = dplyr::bind_rows(lapply(tickers,function(x)
                                schwab_history_single(ticker = x,
                                                      startDate,
                                                      endDate,
                                                      freq,
                                                      accessTokenList=accessTokenList)))

  # Return all tickers in a data frame
  allTickers
}


############### =============================
############### =============================
############### =============================


# ----------- Helper function
# Get pricing data for a single ticker
schwab_history_single = function(ticker='AAPL',startDate=Sys.Date()-30,endDate=Sys.Date(),
                                freq=c('daily','1min','5min','10min','15min','30min'),
                                extended_hours = FALSE, accessTokenList=NULL){

  # Get access token from options if one is not passed
  accessToken = schwab_accessToken(accessTokenList)

  # Set Variable to NULL to pass check()
  date_time <- volume <- NULL

  # Adjust dates to support conversion to numeric time
  startDate = as.Date(startDate)+lubridate::days(1)
  endDate = as.Date(endDate)+lubridate::days(1)

  # Set to non scientific notation and Reset options on exit
  old <- options()
  on.exit(options(old))
  options(scipen=999)

  # Set Variables for URL
  if (missing(freq)) freq='daily'
  startDateMS = as.character(as.numeric(lubridate::as_datetime(startDate, tz='America/New_York'))*1000)
  endDateMS = as.character(as.numeric(lubridate::as_datetime(endDate, tz='America/New_York'))*1000)
  urlticker =  toupper(urltools::url_encode(ticker))
  # Set URL specific parameters
  if (freq=='daily') {
    # If daily, plug in ticker and date in numeric format
    PriceURL = paste0('https://api.schwabapi.com/marketdata/v1/pricehistory',
                      '?symbol=',urlticker,'&periodType=month&frequencyType=daily',
                      '&startDate=',startDateMS,'&endDate=',endDateMS)
  } else {
    # If not daiy, pass frequency and date in numeric format
    PriceURL = paste0('https://api.schwabapi.com/marketdata/v1/pricehistory',
                      '?symbol=',urlticker,'&periodType=day&frequency=',gsub('min','',freq),
                      '&startDate=',startDateMS,'&endDate=',endDateMS,
                      '&needExtendedHoursData=',extended_hours)
  }

   # Send request
  tickRequest = httr::GET(PriceURL,schwab_headers(accessToken))

  # Confirm status code of 200
  schwab_status(tickRequest)

  # Extract pricing data from request
  tickHist <- httr::content(tickRequest, as = "text")
  tickHist <- jsonlite::fromJSON(tickHist)
  tickHist <- tickHist[["candles"]]

  # If no data was pulled, exit the request
  if (methods::is(tickHist, "list")) return()
  tickHist$ticker = ticker
  tickHist$date_time = lubridate::as_datetime(tickHist$datetime/1000, tz='America/New_York')
  tickHist$date = as.Date(tickHist$date_time)
  tickHist = dplyr::select(tickHist,ticker,date,date_time,open:volume)

  # Return pricing data as a tibble
  return(dplyr::as_tibble(tickHist))
}


# ----------- Helper function
# Get quote as a list
schwab_quote_list = function(tickers = c('AAPL','SPY'), accessTokenList=NULL, indicative = FALSE) {

  # Get access token from options if one is not passed
  accessToken = schwab_accessToken(accessTokenList)

  # Create URL for all the tickers
  quoteURL = base::paste0('https://api.schwabapi.com/marketdata/v1/quotes?symbols=',
                          toupper(urltools::url_encode(paste0(tickers,collapse = ','))),
                          '&indicative=',indicative)
  quotes =  httr::GET(quoteURL,schwab_headers(accessToken))

  # Confirm status code of 200
  schwab_status(quotes)

  # Return content of quotes
  return(httr::content(quotes))
}

# ----------- Helper function
# get quotes as a tibble
schwab_quote_df = function(tickers = c('AAPL','SPY'),accessTokenList=NULL) {
  quote.tradeTime  <- NULL
  # Get list of quotes
  quoteList = schwab_quote_list(tickers,accessTokenList)

  # Return data frame from list
  dplyr::bind_rows(lapply(quoteList,data.frame)) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(quote_datetime = lubridate::as_datetime(quote.tradeTime/1000, tz='America/New_York'))

}


