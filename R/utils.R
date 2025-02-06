# Utilities used throughout the rameritrade package
# These functions are not exported to the end user

# -------------- Function 1 schwab_accessToken -------------------------
# Validate if passed Access Token is valid or Get Access Token from Options if available.
# No authentication, just retrieving
schwab_accessToken <- function(accessTokenList) {

  # Validate access token if passed by user
  if (!is.null(accessTokenList)) {

    # check that full access token list has been passed
    ErrMsg = paste0('Incorrect object type passed as Access Token. ',
                    'Please pass the output from schwab_auth3_accessToken')
    if (!methods::is(accessTokenList, "list")) stop(ErrMsg, call. = FALSE)
    if (length(accessTokenList) != 8) stop(ErrMsg, call. = FALSE)
    if (!('access_token' %in% names(accessTokenList))) stop(ErrMsg, call. = FALSE)

    # check if passed access token has expired
    if(accessTokenList$expireTime<Sys.time()) {
      stop('Access Token has expired. Please refresh with schwab_auth3_accessToken', call. = FALSE)
    }

    # Return only the token from the list
    return(accessTokenList$access_token)
  }



  # Get access token from default variable and confirm it has not yet expired
  accessTokenList <- getOption("schwab_access_token")
  if (!is.null(accessTokenList)) {

    # check if defauly access token has expired
    if (accessTokenList$expireTime<Sys.time()) {
      stop('Access Token has expired. Please refresh with schwab_auth3_accessToken', call. = FALSE)
    }

    # Return only the token from the list
    return(accessTokenList$access_token)
  }

  # If no access token has been passed or pulled from default, show error
  stop(paste0("An Access Token has not yet been set. Please use the schwab_auth3_accessToken ",
              "function, with a valid Refresh Token to create an Access Token.", call. = FALSE))


}



# -------------- Function 2 schwab_status -------------------------
# Check if function did not return 200 or 201 and send back TD error code
schwab_status = function(x,msg=NULL){

    # Check if status code is 200 or 201 (201 is specific to orders)
    SC = x$status_code
    if (SC!=200 & SC!=201) {

        # Default to TD Error message and append custom if needed
        ErrMsg = httr::content(x)$error
        stop(paste0(SC,' - ',ErrMsg,msg), call. = FALSE)

    }

}



# -------------- Function 3 schwab_headers -------------------------
# Set headers for all calls with an access token
schwab_headers = function(accessToken){
  httr::add_headers('Authorization' = paste("Bearer", accessToken))
}




# -------------- Function 4 schwab_checkRefresh -------------------------
# Check if Refresh token is valid and not expired
schwab_checkRefresh = function(refreshToken){

  # check if refresh token is a list
  if (!methods::is(refreshToken, "list")) {
    stop(paste0('Incorrect object type passed as Refresh Token. Please pass ',
                'the output from schwab_auth3_accessToken',
                call. = FALSE))
  }

  # Validate refresh token
  if (!('refresh_token' %in% names(refreshToken))) {
    stop(paste0('Incorrect object type passed as Refresh Token. Please pass ',
                'the output from schwab_auth3_accessToken',
                call. = FALSE))
  }
  # Check if Refresh Token has expired
  if (refreshToken$refreshExpire<Sys.time()) {
    stop(paste0('The Refresh Token being used has expired. Please reauthenticate ',
                'using schwab_auth3_accessToken to obtain a new Access Token.',
                 call. = FALSE))
  }
}
