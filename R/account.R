#' Get account balances, positions, and account numbers returned as a list
#'
#' Retrieves account data for all accounts linked to the Access Token or a specific account
#'
#' The output will be either a list of three data frames or a list of three
#' lists that contain balances, positions, and account numbers for Schwab accounts
#' linked to the access token or specified. For historical orders, see
#' \code{\link{schwab_orderSearch}}. The default is for a data frame output which is
#' much cleaner.
#'
#' @param output Use 'df' for a list of 3 data frames containing balances,
#'   positions, and orders. Otherwise the data will be returned as a list of
#'   lists
#' @param account_number The account number as shown on the Schwab website
#' @param value_pull Can be one of 'all','bal','pos','acts' depending on what you
#' want to pull back
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
schwab_accountData = function(output = 'df', account_number = '',
                              value_pull = c('all','bal','pos','acts'), accessTokenList = NULL) {

  account_number_hash = schwab_act_hash(account_number, accessTokenList)
  if(account_number_hash=='invalid'){
    return(list())
  }
  if (missing(value_pull)) value_pull='all'
  value_pull = tolower(value_pull)
  # Use helper functions to generate a lists or data frames
  if (output != 'df') {

    # Create a list of each
    if(value_pull %in% c('all','bal')){
      bal = schwab_actDataList('balances', account_number_hash, accessTokenList)
    } else {
      bal = NULL
    }

    if(value_pull %in% c('all','pos')){
      pos = schwab_actDataList('positions', account_number_hash, accessTokenList)
    } else {
      pos = NULL
    }

    if(value_pull %in% c('all','acts')){
      acts = schwab_actDataList('accountNumbers', account_number_hash, accessTokenList)
    } else {
      acts = NULL
    }

  } else {

    # Create a list of each
    if(value_pull %in% c('all','bal')){
      bal = schwab_actDataDF('balances', account_number_hash, accessTokenList)
    } else {
      bal = NULL
    }

    if(value_pull %in% c('all','pos')){
      pos = schwab_actDataDF('positions', account_number_hash, accessTokenList)
    } else {
      pos = NULL
    }

    if(value_pull %in% c('all','acts')){
      acts = schwab_actDataDF('accountNumbers', account_number_hash, accessTokenList)
    } else {
      acts = NULL
    }

  }

  # Combine them into a list
  act_list = list(balances = bal, positions = pos, accounts = acts)
  if(value_pull != 'all'){
    act_list <- Filter(Negate(is.null), act_list)[[1]]
  }

  return(act_list)

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

  # Get access token from options if one is not passed
  accessToken = schwab_accessToken(accessTokenList)

  # Create URL specific to Brokerage Account and dataType
  actURL = paste0('https://api.schwabapi.com/trader/v1/accounts/accountNumbers')

  # Get account data using a valid accessToken
  accountData <- httr::GET(actURL,schwab_headers(accessToken))
  # Confirm status code of 200
  schwab_status(accountData)
  ret_val = httr::content(accountData)
  ret_val <- Filter(function(x) x$accountNumber == account_number, ret_val)

  if(length(ret_val)==0){
    return('invalid')
  }
  return(ret_val[[1]]$hashValue)
}


############### =============================
############### =============================
############### =============================


# ----------- Helper function
# generate account data in list form
schwab_actDataList = function(dataType=c('balances','positions','accountNumbers'),
                              account_number_hash = '', accessTokenList=NULL) {

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

  # Create URL specific to Brokerage Account and dataType
  actURL = paste0('https://api.schwabapi.com/trader/v1/accounts/',dataTypeURL)

  # Get account data using a valid accessToken
  accountData <- httr::GET(actURL,schwab_headers(accessToken))
  # Confirm status code of 200
  schwab_status(accountData)
  ret_val = httr::content(accountData)
  if(dataTypeURL == 'accountNumbers' & account_number_hash != ''){
    ret_val <- Filter(function(x) x$hashValue == account_number_hash, ret_val)
  }

  # Return Account Data
  return(ret_val)

}


# ----------- Helper function
# generate account data in data frame form
schwab_actDataDF = function(dataType=c('balances','positions','accountNumbers'),
                            account_number_hash='', accessTokenList=NULL) {

  # Check Data Type
  if (missing(dataType)) dataType='balances'

  # Get Account Data in list form
  actData = schwab_actDataList(dataType,account_number_hash,accessTokenList)
  if(!is.null(names(actData[1]))){
    actData = list(actData)
  }
  actData[[1]]$securitiesAccount$positions
  if (dataType=='positions') {
      actOutput =  dplyr::bind_rows(lapply(actData, function(x) {
        act_dets = dplyr::tibble(accountNumber = x$securitiesAccount$accountNumber)
        merge(act_dets,
        dplyr::bind_rows(lapply(x$securitiesAccount$positions, function(y) {
          as.data.frame(y)
        }
        )))
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
      x$securitiesAccount$positions = NULL
      merge(x = data.frame(x$securitiesAccount),
            # y contains the position details
            y = data.frame(x$aggregatedBalance))

    }))
    actOutput = dplyr::as_tibble(actOutput)
  }

  # Return the output from the IF function
  return(actOutput)

}


