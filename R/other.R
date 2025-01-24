#' Get Options Expiration Chain
#'
#' Search an Option Chain for a specific ticker
#'
#' Return a list containing two data frames. The first is the underlying data
#' for the symbol. The second item in the list is a data frame that contains the
#' options chain for the specified ticker.
#'
#' @inheritParams schwab_orderDetail
#' @param ticker underlying ticker for the options chain
#'
#' @return a list of 2 data frames - underlying and options chain
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Pull all option contracts expiring over the next 6 months
#' # with 5 strikes above and below the at-the-money price
#' schwab_optionChain(ticker = 'SPY',
#'              strikes = 5,
#'              endDate = Sys.Date() + 180)
#'
#' }
schwab_optionExpiration = function(ticker, accessTokenList=NULL) {

  # Get access token from options if one is not passed
  accessToken = schwab_accessToken(accessTokenList)
  optionURL = base::paste0('https://api.schwabapi.com/marketdata/v1/expirationchain?symbol=',ticker)
  options =  httr::GET(optionURL,schwab_headers(accessToken))
  # Confirm status code of 200
  schwab_status(options)

  exp_list <- httr::content(options)
  return(dplyr::bind_rows(lapply(exp_list$expirationList,as.data.frame)))
}


#' Get Options Chain
#'
#' Search an Option Chain for a specific ticker
#'
#' Return a list containing two data frames. The first is the underlying data
#' for the symbol. The second item in the list is a data frame that contains the
#' options chain for the specified ticker.
#'
#' @param ticker underlying ticker for the options chain
#' @param strikes the number of strikes above and below the current strike
#' @param inclQuote set TRUE to include pricing details (will be delayed if
#'   account is set for delayed quotes)
#' @param startDate the start date for expiration (should be greater than or
#'   equal to today). Format: yyyy-mm-dd
#' @param contractType can be 'ALL', 'CALL', or 'PUT'
#' @param endDate the end date for expiration (should be greater than or equal
#'   to today). Format: yyyy-mm-dd
#' @inheritParams schwab_accountData
#'
#' @return a list of 2 data frames - underlying and options chain
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Pull all option contracts expiring over the next 6 months
#' # with 5 strikes above and below the at-the-money price
#' schwab_optionChain(ticker = 'SPY',
#'              strikes = 5,
#'              endDate = Sys.Date() + 180)
#'
#' }
schwab_optionChain = function(ticker, strikes = 10, inclQuote = TRUE,
                              startDate = Sys.Date()+1,endDate = Sys.Date() + 360,
                              contractType = c('ALL','CALL','PUT'),
                              accessTokenList = NULL) {

  # Get access token from options if one is not passed
  accessToken = schwab_accessToken(accessTokenList)

  # Set value to NULL to pass check()
  daysToExpiration <- NULL

  if (missing(contractType)) contractType='ALL'


  # Create URL
  optionURL = base::paste0('https://api.schwabapi.com/marketdata/v1/chains?symbol=',ticker,
                           '&contractType=',toupper(contractType),
                           '&strikeCount=',strikes,
                           '&includeUnderlyingQuote=',inclQuote,
                           '&fromDate=',startDate,
                           '&toDate=',endDate)
  options =  httr::GET(optionURL,schwab_headers(accessToken))
  # Confirm status code of 200
  schwab_status(options)

  # Parse Data
  jsonOptions <- httr::content(options, as = "text",encoding = 'UTF-8')
  jsonOptions <- jsonlite::fromJSON(jsonOptions)

  # Extract underlying data
  underlying = data.frame(jsonOptions$underlying) %>%
    dplyr::as_tibble()
  # Extract PUT data
  if(toupper(contractType) %in% c('PUT','ALL')){
    puts =  dplyr::bind_rows(lapply(jsonOptions$putExpDateMap,dplyr::bind_rows)) %>%
      dplyr::mutate(expireDate = Sys.Date() + lubridate::days(daysToExpiration))
  } else {
    puts = NULL
  }
  # Extract CALL data
  if(toupper(contractType) %in% c('CALL','ALL')){
    calls = dplyr::bind_rows(lapply(jsonOptions$callExpDateMap,dplyr::bind_rows)) %>%
      dplyr::mutate(expireDate = Sys.Date() + lubridate::days(daysToExpiration))
  } else {
    calls = NULL
  }
  # Bind Put and Call data into a single data frame
  fullChain = dplyr::bind_rows(puts,calls) %>%
    dplyr::as_tibble()

  returnVal = list(underlying = underlying, fullChain = fullChain)

  returnVal
}


#' Search for all Transaction types
#'
#' Can pull trades as well as transfers, dividend reinvestment, interest, etc.
#' Any activity associated with the account.
#'
#' @inheritParams schwab_orderDetail
#' @param startDate Transactions after a certain date. Will not pull back
#'   transactions older than 1 year. format yyyy-mm-dd
#' @param endDate Filter transactions that occurred before a certain date.
#'   format yyyy-mm-dd
#' @param transType Filter for a specific Transaction type. No entry will return
#'   all types. For example: TRADE, CASH_IN_OR_CASH_OUT, CHECKING, DIVIDEND,
#'   INTEREST, OTHER
#'
#' @return a jsonlite data frame of transactions
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Access Token must be set using schwab_auth_accessToken
#' # Transactions for the last 5 days
#' schwab_transactSearch(account_number = 987654321,
#'                 startDate = Sys.Date()-days(5))
#'
#' }
schwab_transactSearch = function(account_number, startDate = Sys.Date()-30,
                           endDate = Sys.Date(), transType = 'TRADE',
                           accessTokenList = NULL){

  # Get access token from options if one is not passed
  account_number_hash = schwab_act_hash(account_number, accessTokenList)
  accessToken = schwab_accessToken(accessTokenList)

  # Construct URL
  st_timestamp <- urltools::url_encode(format(startDate, "%Y-%m-%dT%H:%M:%OS3Z"))
  ed_timestamp <- urltools::url_encode(format(endDate+2, "%Y-%m-%dT%H:%M:%OS3Z"))
  transactURL = paste0('https://api.schwabapi.com/trader/v1/accounts/',account_number_hash,
                       '/transactions?startDate=',st_timestamp,
                       '&endDate=',ed_timestamp,'&types=',toupper(transType))
  # Make GET request for transactions
  searchTransact = httr::GET(transactURL,schwab_headers(accessToken),encode='json')

  # Confirm status code of 200
  schwab_status(searchTransact)

  # Parse Data
  jsonTransact <- httr::content(searchTransact, as = "text",encoding = 'UTF-8')
  jsonTransact <- jsonlite::fromJSON(jsonTransact)

  dplyr::as_tibble(jsonTransact)
}



#' Get Market Hours
#'
#' Returns a list output for current day and specified market that details the
#' trading window
#'
#' @inheritParams schwab_accountData
#' @param marketType The asset class to pull:
#'   'EQUITY','OPTION','BOND','FUTURE','FOREX'. Default is EQUITY
#'
#' @param date Current or future date to check hours
#'
#' @return List output of times and if the current date is a trading day
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Access Token must be set using schwab_auth_accessToken
#' # Market hours for the current date
#' schwab_marketHours()
#' schwab_marketHours('2020-06-24', 'OPTION')
#'
#' }
schwab_marketHours = function(marketType = c('EQUITY','OPTION','BOND','FUTURE','FOREX'),
                              date = Sys.Date(),
                              accessTokenList = NULL){

  # Get access token from options if one is not passed
  accessToken = schwab_accessToken(accessTokenList)

  # Create URL for market
  if (missing(marketType)) marketType='EQUITY'
  marketURL = paste0('https://api.schwabapi.com/marketdata/v1/markets?markets=',marketType,
                     '&date=',date)

  # Make Get Request using token
  marketHours = httr::GET(marketURL, schwab_headers(accessToken), encode='json')

  # Confirm status code of 200
  schwab_status(marketHours)

  # Return raw content - market hours in list form
  return(httr::content(marketHours))
}


#' Get ticker details
#'
#' Get identifiers and fundamental data for a specific ticker
#'
#' @inheritParams schwab_accountData
#' @param tickers valid ticker(s) or symbol(s)
#'
#' @return data frame of ticker details
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Details for Apple
#' schwab_symbolDetail('AAPL')
#'
#' }
schwab_symbolDetail = function(tickers = c('AAPL','SPY'), accessTokenList = NULL) {

  # Get access token from options if one is not passed
  accessToken = schwab_accessToken(accessTokenList)

  # Construct URL
  tickerURL = paste0('https://api.schwabapi.com/marketdata/v1/instruments?symbol=',
                     paste0(tickers, collapse = '%2C'),'&projection=fundamental')

  # Make Get Request using token
  tickerDet = httr::GET(tickerURL, schwab_headers(accessToken))

  # Confirm status code of 200
  schwab_status(tickerDet)

  # Get Content
  tickCont = httr::content(tickerDet)
  if (length(tickCont)==0) stop('Ticker not valid')

  fund = dplyr::bind_rows(lapply(tickCont$instruments,as.data.frame))


  # Return data as a data frame
  return(fund)
}

