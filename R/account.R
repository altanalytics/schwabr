#' Get account balances positions, and orders returned as a list
#'
#' Retrieves account data for all accounts linked to the Access Token or a specific account
#'
#' The output will be either a list of three data frames or a list of three
#' lists that contain balances, positions, and orders for Schwab accounts
#' linked to the access token or specified. For historical orders, see
#' \code{\link{schwab_orderSearch}}. The default is for a data frame output which is
#' much cleaner.
#'
#' @param output Use 'df' for a list of 3 data frames containing balances,
#'   positions, and orders. Otherwise the data will be returned as a list of
#'   lists
#' @param account_number The account number as shown on Schwab
#' @param accessTokenList A valid Access Token must be set using the output from
#'   \code{\link{schwab_auth3_accessToken}}. The most recent Access Token will be
#'   used by default unless one is manually passed into the function.
#'
#' @return a list of requested account details
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get stored refresh token
#' refreshToken = readRDS('/secure/location/')
#'
#' # Generate a new access token
#' accessTokenList = schwab_auth3_accessToken(appKey, appSecret, refreshToken)
#'
#' # Passing the accessTokenList is optional. The default will return balances
#' asDF = schwab_accountData()
#' asList = schwab_accountData('list',account_number = '', accessTokenList)
#'
#' }
schwab_accountData = function(output = 'df', account_number = '', accessTokenList = NULL) {

  account_number_hash = schwab_act_hash(account_number, accessTokenList)
  # Use helper functions to generate a lists or data frames
  if (output != 'df') {

    # Create a list of each
    bal = schwab_actDataList('balances', account_number_hash, accessTokenList)
    pos = schwab_actDataList('positions', account_number_hash, accessTokenList)
    nums = schwab_actDataList('accountNumbers', account_number_hash, accessTokenList)

  } else {

    # Create a data frame of each
    bal = schwab_actDataDF('balances', account_number_hash, accessTokenList)
    pos = schwab_actDataDF('positions', account_number_hash, accessTokenList)
    nums = schwab_actDataDF('accountNumbers', account_number_hash, accessTokenList)

  }

  # Combine them into a list
  Result = list(balances = bal, positions = pos, accounts = nums)

  return(Result)

}

#' Get account hashed value
#'
#' Retrieves the Hashed account value for a specific account
#'
#'
#' @param account_number A Standard Schwab Account number
#' @param accessTokenList A valid Access Token must be set using
#'   \code{\link{schwab_auth3_accessToken}}. The most recent Access Token will be
#'   used by default unless one is manually passed into the function.
#'
#' @return A hashed account number
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get stored refresh token
#' refreshToken = readRDS('/secure/location/')
#'
#' # Generate a new access token
#' accessTokenList = schwab_auth3_accessToken(appKey, appSecret, refreshToken)
#'
#' # Passing the accessTokenList is optional. The default will return balances
#' act_hash = schwab_act_hash(account_number = '123456789')
#'
#'
#' }
schwab_act_hash = function(account_number = '', accessTokenList = NULL){

  if(account_number == ''){
    return('')
  }

  act_list = schwab_actDataDF('accountNumbers', account_number, accessTokenList)
  account_number_hash = act_list$hashValue[act_list$accountNumber==account_number]
  return(account_number_hash)
}


############### =============================
############### =============================
############### =============================


# ----------- Helper function
# generate account data in list form
schwab_actDataList = function(dataType=c('balances','positions','accountNumbers'),
                              account_number_hash = NULL, accessTokenList=NULL) {

  # Get access token from options if one is not passed
  accessToken = schwab_accessToken(accessTokenList)

  # Check Data Type, default to balances, stop if not one of the three options passed
  if (missing(dataType)) dataType='balances'
  if (!(dataType %in% c('balances','positions','accountNumbers'))) {
    stop('dataType must be "balances", "positons", or "accountNumbers"', call. = FALSE)
  }

  # Set URL end based on user input
  dataTypeURL = switch(dataType,
                       'balances'=account_number_hash,
                       'positions'=paste0(account_number_hash,'?fields=positions'),
                       'accountNumbers'='accountNumbers')

  # Create URL specific to TD Brokerage Account and dataType
  actURL = paste0('https://api.schwabapi.com/trader/v1/accounts/',dataTypeURL)

  # Get account data using a valid accessToken
  accountData <- httr::GET(actURL,schwab_headers(accessToken))
  # Confirm status code of 200
  schwab_status(accountData)

  # Return Account Data
  return(httr::content(accountData))

}


# ----------- Helper function
# generate account data in data frame form
schwab_actDataDF = function(dataType=c('balances','positions','accountNumbers'),
                            account_number_hash='', accessToken=NULL) {

  # Check Data Type
  if (missing(dataType)) dataType='balances'

  # Get Account Data in list form
  actData = schwab_actDataList(dataType,account_number_hash,accessToken)

  if (dataType=='positions') {

    # Pull out account and position details
    actOutput =  dplyr::bind_rows(lapply(actData, function(x) {
      # Merge account details (x) with position details (y)
      merge(x = data.frame(x$securitiesAccount)[,c(2,1,3:5)],
            # y contains the position details
            y = dplyr::bind_rows(lapply(x[[1]]$positions,data.frame)))
      }))
    actOutput = dplyr::as_tibble(actOutput)
  } else if(dataType == 'accountNumbers'){
    actOutput =  dplyr::bind_rows(lapply(actData, function(x) {
      as.data.frame(x)
    }))
    actOutput = dplyr::as_tibble(actOutput)


  } else {

    actOutput = dplyr::bind_rows(lapply(actData, function(x) {
      # Merge account details (x) with balance details (y)
      merge(x = data.frame(x$securitiesAccount)[,c(2,1,3:5)],
            # y contains the current cash balances
            y = data.frame(x[[1]]$currentBalances))
      }))
    actOutput = dplyr::as_tibble(actOutput)
  }

  # Return the output from the IF function
  return(actOutput)

}


