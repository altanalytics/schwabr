#' Get Details for a Single Order
#'
#' Pass an order ID and Account number to get details such as status, quantity,
#' ticker, executions (if applicable), account, etc.
#'
#' @param orderId A valid Schwab Order ID
#' @param account_number A Schwab account number associated with the Access Token
#'
#' @inheritParams schwab_accountData
#'
#' @return list of order details
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get stored refresh token
#' refreshToken = readRDS('/secure/location/')
#'
#' # generate a new access token
#' accessTokenList = schwab_auth3_accessToken('AppKey', 'AppSecret', refreshToken)
#'
#' # Get order details for a single order
#' # Passing Access Token is optional once it's been set
#' schwab_orderDetail(orderId = 123456789, account_number = 987654321)
#'
#' }
schwab_orderDetail = function(orderId, account_number, accessTokenList=NULL) {

  # Get access token from options if one is not passed
  account_number_hash = schwab_act_hash(account_number, accessTokenList)
  accessToken = schwab_accessToken(accessTokenList)

  # Get Order Details
  orderURL = paste0('https://api.schwabapi.com/trader/v1/accounts/',account_number_hash,'/orders/',orderId)
  orderDetails = httr::GET(orderURL,schwab_headers(accessToken))

  # Confirm status code of 200
  schwab_status(orderDetails)

  # Return order details in list form
  httr::content(orderDetails)

}

#' Cancel an Open Order
#'
#' Pass an Order ID and Account number to cancel an existing open order
#'
#' @inheritParams schwab_orderDetail
#'
#' @return order API URL. Message confirming cancellation
#' @export
#'
#' @examples
#'  \dontrun{
#'
#' schwab_cancelOrder(orderId = 123456789, account_number = 987654321)
#'
#' }
schwab_cancelOrder =  function(orderId,account_number,accessTokenList=NULL){

  # Get access token from options if one is not passed
  account_number_hash = schwab_act_hash(account_number, accessTokenList)
  accessToken = schwab_accessToken(accessTokenList)

  # Create Order URL and then DELETE order
  orderURL = paste0('https://api.schwabapi.com/trader/v1/accounts/',account_number_hash,'/orders/',orderId)
  orderCancel = httr::DELETE(orderURL,schwab_headers(accessToken))

  # Confirm status code of 200
  schwab_status(orderCancel,
             '. Make sure the order ID is for an open order and the account number is correct.')


  return(orderCancel$url)

}


#' Search for orders by date
#'
#' Search for orders associated with a Schwab account over the previous 60 days. The
#' result is a list of three objects:
#' \enumerate{
#' \item jsonlite formatted extract of all orders
#' \item all entered orders with details
#' \item a data frame of all executed orders with the executions }
#'
#' @inheritParams schwab_orderDetail
#' @param startDate Orders from a certain date with. Format yyyy-mm-dd.
#' @param endDate Filter orders that occurred before a certain date. Format
#'   yyyy-mm-dd
#' @param maxResults the max results to return in the query
#' @param orderStatus search by order status (ACCEPTED, FILLED, EXPIRED,
#'   CANCELED, REJECTED, etc). This can be left blank for all orders. See
#'   documentation for full list
#'
#' @return a list of three objects: a jsonlite formatted extract of all orders,
#'   all entered orders with details, a data frame of all executed orders with
#'   the executions
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get all orders run over the last 50 days (up to 500)
#' schwab_orderSearch(account_number = account_number,
#'             startDate = Sys.Date()-50,
#'             maxResults = 500, orderStatus = '')
#'
#' }
schwab_orderSearch = function(account_number, startDate = Sys.Date()-30, endDate = Sys.Date(),
                        maxResults = 50, orderStatus = '', accessTokenList = NULL){

  # Bind variables to quiet warning
  account_number_hash = schwab_act_hash(account_number, accessTokenList)
  accountNumber <- orderId <- instrument.symbol <- instruction <- total_qty <- NULL
  filledQuantity <- quantity <- duration <- orderType <- instrument.cusip <- enteredTime <- NULL

  # Get access token from options if one is not passed
  accessToken = schwab_accessToken(accessTokenList)

  # Construct URL for GET request
  st_timestamp <- format(startDate, "%Y-%m-%dT%H:%M:%OS3Z")
  ed_timestamp <- format(endDate+2, "%Y-%m-%dT%H:%M:%OS3Z")
  searchURL = paste0('https://api.schwabapi.com/trader/v1/accounts/',account_number_hash,
                     '/orders?maxResults=',maxResults,'&status=',orderStatus,
                     '&fromEnteredTime=',st_timestamp,'&toEnteredTime=',ed_timestamp)

  # Make GET request
  searchOrders = httr::GET(searchURL,schwab_headers(accessToken),encode='json')

  # Confirm status code of 200
  schwab_status(searchOrders)

  # Extract content from GET request
  jsonOrder <- httr::content(searchOrders, as = "text",encoding = 'UTF-8')
  jsonOrder <- jsonlite::fromJSON(jsonOrder)


  OrdrExecFinal=NULL
  OrderEnterFinal=NULL
    # Run a loop for each order within the account
    UnqOrdrs = httr::content(searchOrders)
    if(length(UnqOrdrs)>0){
      for(ords in 1:length(UnqOrdrs)) {

        # Get the high level order details
        OrdrDet = UnqOrdrs[[ords]]
        OrdrDet$orderLegCollection = NULL
        OrdrDet$orderActivityCollection = NULL
        OrdrDet = data.frame(OrdrDet) %>% dplyr::rename(total_qty=quantity)

        # Get the Entry details and merge with order details
        OrdrEnter = UnqOrdrs[[ords]]
        OrdrEnter = dplyr::bind_rows(lapply(OrdrEnter$orderLegCollection,data.frame))
        OrdrEnter = merge(OrdrEnter,OrdrDet)
        OrderEnterFinal = dplyr::bind_rows(OrderEnterFinal,OrdrEnter)

        # Get execution details when available
        OrdrExec = UnqOrdrs[[ords]]
        OrdrExec = dplyr::bind_rows(lapply(OrdrExec$orderActivityCollection,data.frame))
        OrdrEntDet = dplyr::select(OrdrEnter,accountNumber,orderId,instrument.symbol,instruction,total_qty,filledQuantity,duration,orderType,instrument.cusip,
                                   enteredTime)
        OrdrExecAll = merge(OrdrEntDet,OrdrExec)
        OrdrExecFinal = dplyr::bind_rows(OrdrExecFinal,OrdrExecAll)
      }
    }

    # Combine all three outputs into a single list
    orderOutput = list(enteredOrders = dplyr::as_tibble(OrderEnterFinal),
                       executedOrders = dplyr::as_tibble(OrdrExecFinal),
                       allOrderJSON = dplyr::as_tibble(jsonOrder))

    orderOutput
}




#' Place Order for a specific account
#'
#' Place trades through the SchwabAPI using a range of parameters
#'
#' A valid account and access token must be passed. An access token will be
#' passed by default when \code{\link{schwab_auth3_accessToken}} is executed
#' successfully and the token has not expired, which occurs after 30 minutes.
#' Only simple orders using  equities and options can be traded at
#' through this function at this time. This function is built
#' to allow a single trade submission. More complex trades can be executed
#' through the API, but a custom function or submission will need to be
#' constructed. To build more custom trading strategies, reference the
#' 'Schwab API' examples. A full list of the input parameters and details can be
#' found in the documentation. TEST ALL ORDERS FIRST WITH SMALL DOLLAR AMOUNTS!!!
#'
#' A minimum of four parameters are required for submission: ticker, instruction,
#' quantity, and account number associated with the Access Token. The following
#' parameters default: session - NORMAL, duration - DAY, asset type - EQUITY,
#' and order type - MARKET
#'
#' @section Warning: TRADES THAT ARE SUCCESSFULLY ENTERED WILL BE SUBMITTED
#'   IMMEDIATELY THERE IS NO REVIEW PROCESS. THIS FUNCTION HAS HUNDREDS OF
#'   POTENTIAL COMBINATIONS AND ONLY A HANDFUL HAVE BEEN TESTED. IT IS STRONGLY
#'   RECOMMENDED TO TEST THE DESIRED ORDER ON A VERY SMALL QUANTITY WITH LITTLE
#'   MONEY AT STAKE. ANOTHER OPTION IS TO USE LIMIT ORDERS FAR FROM THE CURRENT
#'   PRICE. TD AMERITRADE HAS THEIR OWN ERROR HANDLING BUT IF A SUCCESSFUL
#'   COMBINATION IS ENTERED IT COULD BE EXECUTED IMMEDIATELY. DOUBLE CHECK ALL
#'   ENTRIES BEFORE SUBMITTING.
#'
#'
#' @inheritParams schwab_orderDetail
#' @param ticker a valid Equity/ETF or option. If needed, use schwab_symbolDetail to
#'   confirm. This should be a ticker/symbol, not a CUSIP
#' @param quantity the number of shares to be bought or sold. Must be an
#'   integer.
#' @param instruction Equity instructions include 'BUY', 'SELL', 'BUY_TO_COVER',
#'   or 'SELL_SHORT'. Options instructions include 'BUY_TO_OPEN',
#'   'BUY_TO_CLOSE', 'SELL_TO_OPEN', or 'SELL_TO_CLOSE'
#' @param orderType MARKET, LIMIT (requiring limitPrice), STOP (requiring
#'   stopPrice), STOP_LIMIT, TRAILING_STOP (requiring stopPriceBasis,
#'   stopPriceType, stopPriceOffset)
#' @param limitPrice the limit price for a LIMIT or STOP_LIMIT order
#' @param stopPrice the stop price for a STOP or STOP_LIMIT order
#' @param assetType EQUITY or OPTION. No other asset types are available at this
#'   time. EQUITY is the default.
#' @param session NORMAL for normal market hours, AM or PM for extended market
#'   hours
#' @param duration how long will the trade stay open without a fill: DAY,
#'   GOOD_TILL_CANCEL, FILL_OR_KILL
#' @param stopPriceBasis LAST, BID, or ASK which is the basis for a STOP,
#'   STOP_LIMIT, or TRAILING_STOP
#' @param stopPriceType the link to the stopPriceBasis. VALUE for dollar
#'   difference or PERCENT for a percentage offset from the price basis
#' @param stopPriceOffset an integer that indicates the offset used for the
#'   stopPriceType, 10 and PERCENT is a 10 percent offset from the current price
#'   basis. 5 and VALUE is a 5 dollar offset from the current price basis
#'
#' @return the trade id, account id, and other order details
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get stored refresh token
#' refreshToken = readRDS('/secure/location/')
#'
#' # generate a new access token
#' accessTokenList = schwab_auth3_accessToken('AppKey', 'AppSecret', refreshToken)
#'
#' # Set Account Number
#' account_number = 1234567890
#'
#' # Standard market buy order
#' # Every order must have at least these 4 paramters
#' schwab_placeOrder(account_number = account_number,
#'                   ticker = 'AAPL',
#'                   quantity = 1,
#'                   instruction = 'buy')
#'
#' # Stop limit order - good until canceled
#' schwab_placeOrder(account_number = account_number,
#'             ticker = 'AAPL',
#'             quantity = 1,
#'             instruction = 'sell',
#'             duration = 'good_till_cancel',
#'             orderType = 'stop_limit',
#'             limitPrice = 98,
#'             stopPrice = 100)
#'
#' # Trailing Stop Order
#' schwab_placeOrder(account_number = account_number,
#'             ticker='AAPL',
#'             quantity = 1,
#'             instruction='sell',
#'             orderType = 'trailing_stop',
#'             stopPriceBasis = 'BID',
#'             stopPriceType = 'percent',
#'             stopPriceOffset = 10)
#'
#' # Option Order with a limit price
# quotes = schwab_optionChain(ticker = 'SPY',
#              strikes = 5,
#              endDate = Sys.Date() + 180)
# sym = quotes$fullChain$symbol[1]
# schwab_placeOrder(account_number = account_number,
#             ticker = sym,
#             quantity = 1,
#             instruction = 'BUY_TO_OPEN',
#             duration = 'Day',
#             orderType = 'LIMIT',
#             limitPrice = .02,
#             assetType = 'OPTION')
#'
#' }
schwab_placeOrder = function(account_number, ticker, quantity, instruction,
                             orderType = 'MARKET', limitPrice = NULL, stopPrice = NULL,
                             assetType = c('EQUITY','OPTION'), session='NORMAL', duration='DAY',
                             stopPriceBasis = NULL, stopPriceType = NULL, stopPriceOffset = NULL,
                             accessTokenList = NULL) {

  # Get access token from options if one is not passed
  accessToken = schwab_accessToken(accessTokenList)
  account_number_hash = schwab_act_hash(account_number, accessTokenList)

  # Check symbol and asset type
  if (missing(assetType)) assetType ='EQUITY'

  # Set URL specific to account
  orderURL = paste0('https://api.schwabapi.com/trader/v1/accounts/',account_number_hash,'/orders')

   # Put order details in a list
  orderList = list(orderType = toupper(orderType),
                   complexOrderStrategyType = 'NONE',
                   session = toupper(session),
                   duration = toupper(duration),
                   price = limitPrice,
                   stopPrice = stopPrice,
                   orderStrategyType = 'SINGLE',
                   stopPriceLinkBasis = toupper(stopPriceBasis),
                   stopPriceLinkType = toupper(stopPriceType),
                   stopPriceOffset = stopPriceOffset,
                   orderLegCollection = list(list(
                     instruction = toupper(instruction),
                     # divCapGains = "REINVEST",
                     quantity = quantity,
                     instrument = list(
                       symbol = ticker,
                       assetType = assetType
                     )
                   ))
  )

  # Post order to TD
  postOrder = httr::POST(orderURL,schwab_headers(accessToken),body=orderList,encode='json')
  # httr::content(postOrder)
  # Confirm status code of 201
  schwab_status(postOrder)

  # Collect Order Details
  orderDet = postOrder$headers
  orderOutput = data.frame(
    account_number = gsub('/orders/.*','',gsub('.*accounts/','',orderDet$location)),
    orderId = gsub('.*orders/','',orderDet$location),
    status_code = postOrder$status_code,
    date = orderDet$date,
    location = orderDet$location
  )

  # Return Order Output
  orderOutput
}







