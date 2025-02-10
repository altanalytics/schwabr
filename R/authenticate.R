#' Auth Step 1: Generate LogIn URL
#'
#' Create URL to grant App access to Charles Schwab accounts
#'
#' To use the 'Schwab API', both an account and a registered
#' developer app are required. The developer app functions as a middle layer
#' between the brokerage account and the API. A developer app should be
#' registered on the \href{https://developer.schwab.com}{Schwab
#' Developer} site. Once logged in to the developer site, use My Apps to
#' register an application. An App will have a key and secret provided.
#' The Key/Secret is auto generated and can be found under
#' Dashboard > View Details at the bottom. The user must also create a Callback URL.
#' The Callback URL must be a valid URL. The example below assumes the Callback
#' URL is https://127.0.0.1. The Application should be in a "Ready to Use"
#' state before attempting to login.
#'
#'
#' This function will use these inputs to generate a URL where the user can
#' log in to their standard Charles Schwab Access Page and grant the
#' application access to the specific accounts, enabling the API. The URL
#' Authorization Code generated at the end of the log in process will feed into
#' \code{\link{schwab_auth2_refreshToken}}. For questions, please reference the
#' \href{https://developer.schwab.com/products/trader-api--individual/details/documentation/}{Schwab
#' Docs} or see the examples in the 'schwabr' readme.
#'
#'
#' @param appKey 'Schwab API' generated App Key for the registered app.
#' @param callbackURL Users Callback URL for the registered app
#'
#' @return login url to grant app permission to Schwab accounts
#' @export
#'
#' @examples
#'
#' # Visit the URL generated from the function below to log in accept terms and
#' # select the accounts you want to have API permissions.
#'
#' # This assumes you set the callback to 'https://127.0.0.1'
#' appKey = 'ALPHANUM1234KEY'
#' loginURL = schwab_auth1_loginURL(appKey, 'https://127.0.0.1')
#'
schwab_auth1_loginURL = function(appKey, callbackURL) {

  # Generate URL specific to the registered 'Schwab API' Application
  url = paste0('https://api.schwabapi.com/v1/oauth/authorize?client_id=',
               appKey,'&redirect_uri=',callbackURL)

  return(url)

}



#' Auth Step 2: Obtain Refresh Token
#'
#' Get a Refresh Token using the Authorization Code
#'
#' Once a URL has been generated using \code{\link{schwab_auth1_loginURL}}, a user
#' can visit that URL to grant access to Schwab accounts. Once the button "Done"
#' at the end of the process is pressed, the user will be
#' redirected, potentially to "This site can't be reached". This indicates a
#' successful log in. The URL of this page contains the Authorization Code.
#' Paste the entire URL, not just the Authorization Code, into
#' schwab_auth2_refreshToken. The authorization code will be a long alpha
#' numeric string starting with 'https' and having 'code=' embedded.
#'
#' The output of schwab_auth2_refreshToken will be a Refresh Token which will be used
#' to gain access to the Schwab account(s) going forward. The Refresh
#' Token will be valid for 7 days. Be sure to save the Refresh Token to a safe
#' location.
#'
#' The Refresh Token is needed to generate an Access Token using
#' \code{\link{schwab_auth3_accessToken}}, which is used for general account access.
#' The Access Token expires after 30 minutes but the Refresh Token remains
#' active for 7 days. You want to store your refresh token somewhere safe
#' so that you can reference it later to regenerate an authorization token. After 7
#' days you have to manually log in again. The 'Schwab API' team indicated this might
#' change in the future, but no set timeline.
#'
#'
#' @inheritParams schwab_auth1_loginURL
#'
#' @param appSecret 'Schwab API' generated Secret for the registered app.
#'
#' @param codeToken Will be the URL at the end of Auth Step 1. Somewhere in the
#' URL you should see code=CO.xxx. Paste the entire URL into the function.
#'
#'
#'
#' @seealso \code{\link{schwab_auth1_loginURL}} to generate a login url which leads
#'   to an authorization code, and more importantly generated a Refresh Token, you can feed
#'   the refresh token into \code{\link{schwab_auth3_accessToken}}
#'   to generate a new Access Token
#'
#' @return Refresh Token that is valid for 7 days
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Initial access will require manually logging in to the URL from schwab_auth1_loginURL
#' # After a successful log in, the URL authorization code can be fed with a callbackURL
#' tok = schwab_auth2_refreshToken(appKey = 'schwab_APP_KEY',
#'                                appSecret = 'schwab_SECRET',
#'                                callbackURL = 'https://127.0.0.1',
#'                                codeToken = 'https://127.0.0.1?code=Auhtorizationcode')
#'
#'
#' # Save the Refresh Token somewhere safe where it can be retrieved
#' saveRDS(tok$refresh_token,'/secure/location/')
#'
#' }
schwab_auth2_refreshToken = function(appKey, appSecret, callbackURL, codeToken) {


  decodedtoken = urltools::url_decode(gsub('&.*','',gsub(".*code=", "", codeToken)))
  authreq = list(grant_type = "authorization_code",
                 code = decodedtoken,
                 redirect_uri = callbackURL)
  encodedAPI = base64enc::base64encode(charToRaw(paste0(appKey,':',appSecret)))
  authresponse = httr::POST("https://api.schwabapi.com/v1/oauth/token",
                            httr::add_headers(Authorization = paste0("Basic ",encodedAPI),
                                              `Content-Type` = "application/x-www-form-urlencoded"),
                            body = authreq, encode = "form")

  schwab_status(authresponse)

  # Extract content and add expiration time to Access Token
  accessToken = httr::content(authresponse)

  return(accessToken)

}




#' Auth Step 3: Get Access Token
#'
#' Get a new Access Token using a valid Refresh Token
#'
#' An Access Token is required for the functions within 'schwabr' It serves
#' as a user login to your accounts. The token is valid for 30 minutes
#' and allows the user to place trades, get account information, get order
#' history, pull historical stock prices, etc. A Refresh Token is required to
#' generate an Access Token. \code{\link{schwab_auth2_refreshToken}}  can be used to
#' generate a Refresh Token which stays valid for 7 days. The appKey is
#' generated automatically when an App is registered on the
#' \href{https://developer.schwab.com}{Schwab Developer} site. By default,
#' the Access Token is stored into options and will automatically be
#' passed to downstream functions. However, the user can also submit an Access
#' Token manually if multiple tokens are in use (for example: when managing more
#' than one log in.)
#'
#'
#' DISCLOSURE: This software is in no way affiliated, endorsed, or approved by
#' Charles Schwab or any of its affiliates. It comes with absolutely no warranty
#' and should not be used in actual trading unless the user can read and
#' understand the source code. The functions within this package have been
#' tested under basic scenarios. There may be bugs or issues that could prevent
#' a user from executing trades or canceling trades. It is also possible trades
#' could be submitted in error. The user will use this package at their own
#' risk.
#'
#'
#' @param refreshToken An existing Refresh Token generated using
#'   \code{\link{schwab_auth2_refreshToken}}. Only pass the refresh_token, not the entire list
#' @inheritParams schwab_auth2_refreshToken
#'
##' @seealso \code{\link{schwab_auth1_loginURL}} to generate a login url which leads
#'   to an authorization code, then use  \code{\link{schwab_auth2_refreshToken}} to
#'   generate Refresh Token with the authorization code
#'
#' @return Access Token that is valid for 30 minutes. By default it is stored in
#'   options.This is a list of objects that also shows when the access token expires
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # A valid Refresh Token can be fed into the function below for a new Access Token
#' refreshToken = readRDS('/secure/location/')
#' accessTokenList = schwab_auth3_refreshToken('schwab_APPKey', 'schwab_AppSecret', refreshToken)
#'
#' }
schwab_auth3_accessToken = function(appKey, appSecret, refreshToken) {



  # Get default Access Token in environment if available
  accessTokenList <- getOption("schwab_access_token")

  # If default Access Token is not null and environment is interactive, check for expiration
  if (!is.null(accessTokenList)) {
    if(accessTokenList$refresh_token == refreshToken){
    # If Access Token has not expired, ask if new Access Token should be generated if more than 5 minutes left
      MinTillExp = round(as.numeric(accessTokenList$expireTime-Sys.time()),1)
      if (MinTillExp>5) {
        return(accessTokenList)
      }
    }
  }


  authreq = list(grant_type = 'refresh_token',
                 refresh_token = refreshToken)
  encodedAPI = base64enc::base64encode(charToRaw(paste0(appKey,':',appSecret)))
  authresponse = httr::POST("https://api.schwabapi.com/v1/oauth/token",
                            httr::add_headers(Authorization = paste0("Basic ",encodedAPI),
                                              `Content-Type` = "application/x-www-form-urlencoded"),
                            body = authreq, encode = "form")


  # Confirm status code of 200
  schwab_status(authresponse)

  # Extract content and add expiration time to Access Token
  accessToken = httr::content(authresponse)
  accessToken$expireTime = Sys.time() + lubridate::seconds(as.numeric(accessToken$expires_in)-60)
  accessToken$createTime = Sys.time()

  # Set Access Token to a default option
  options(schwab_access_token = accessToken)

  # Return Access Token
  return(accessToken)
}


############### =============================
############### =============================
############### =============================





