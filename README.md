
<!-- README.md is generated from README.Rmd. Please edit that file -->

# schwabr

<!-- badges: start 
![CRAN Version](https://www.r-pkg.org/badges/version/schwabr?color=green)
-->

![Dev Version](https://img.shields.io/badge/github-0.1.3-blue.svg)
![CRAN
Version](https://www.r-pkg.org/badges/version/schwabr?color=green)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/schwabr)
<!-- badges: end -->

R package for the Charles Schwab trade API, facilitating authentication,
trading, price requests, account balances, positions, order history,
option chains, and more. A user will need a Schwab Brokerage account and
Schwab developer app. They also need the account to be Think or Swim
enabled. See full instructions below.

*Please note: the Schwab Developer App takes a few days to be fully
approved so start that process in advance of using this package*

There is another and similar R CRAN package:
[charlesschwabapi](https://cran.r-project.org/package=charlesschwabapi).
It seems similar in terms of functionality to this package. The biggest
difference is in order input where this package is easier to create
orders but also limits the user to more simple order types. `Schwabr`
was built because most of the functionality already existed in the
deprecated TD Ameritrade R API called
[rameritrade](https://altanalytics.github.io/rameritrade/index.html).

## Introduction

Charles Schwab is one of many trading platforms that offer a trade API.
They ported it over from TD Ameritrade, so users of that platform should
be familiar with much of the components here. Other platforms like
Alpaca, TastyTrade, TradeStation, etc. may have more modern APIs. This
API does have a few limitations, obstacles:

- A manual Authentication refresh must be made every 7 days
  - Schwab has indicated they want to update this - no timeline yet
- Setting up an App is a bit tedious
- Auth flow is convoluted
- No fractional shares

All that said, the Schwab API should give a retail user all the basic
functionality needed to programatically manage their accounts and
trading.

### Disclosure

This software is in no way affiliated, endorsed, or approved by Charles
Schwab or any of its affiliates. It comes with absolutely no warranty
and should not be used in actual trading unless the user can read and
understand the source code. The functions within this package have been
tested under basic scenarios. There may be bugs or issues that could
prevent a user from executing trades or canceling trades. It is also
possible trades could be submitted in error. The user will use this
package at their own risk.

Please heed the following warning for the `schwab_placeOrder` function.
WARNING: TRADES THAT ARE SUCCESSFULLY ENTERED WILL BE SUBMITTED
IMMEDIATELY THERE IS NO REVIEW PROCESS. THE `schwab_placeOrder` FUNCTION
HAS HUNDREDS OF POTENTIAL COMBINATIONS AND ONLY A HANDFUL HAVE BEEN
TESTED. SCHWAB HAS THEIR OWN ERROR HANDLING BUT IF A SUCCESSFUL
COMBINATION IS ENTERED IT COULD BE EXECUTED IMMEDIATELY. DOUBLE CHECK
ALL ENTRIES BEFORE SUBMITTING. IT IS STRONGLY RECOMMENDED TO TEST THE
DESIRED ORDER FIRST. THREE POTENTIAL OPTIONS ARE:

1.  Enter trades outside of normal market trading hours and check the
    Schwab website to ensure proper entry
2.  Use limit orders with limit prices far outside the current bid/ask
3.  Enter very small quantities that won’t put much capital at risk

FOR OPTIONS 1 AND 2, BE SURE TO CANCEL ORDERS USING `schwab_cancelOrder`
OR THROUGH THE TD WEBSITE

## Installation

You can install schwabr using:

``` r

# Install from CRAN
install.packages("schwabr")

# Install development version
install.packages("devtools")
devtools::install_github("altanalytics/schwabr")
```

## Authentication

Initial authorization to Schwab requires a 3 step authentication
process. Once initial authorization is achieved, a refresh_token can by
used to grant access for 7 days. After that, a manual login must be
completed following steps 1-3. To get a new refresh token. Below is a
detailed summary of the entire process followed by code demonstrating
the 3 step process. More can be found at the [Schwab
site](https://developer.schwab.com) or the [Authentication
Guide](https://developer.schwab.com/products/trader-api--individual/details/documentation/Retail%20Trader%20API%20Production).
Details are also provided within the functions.

1.  Create a login and Register an App with the individual Developer
    account with [Schwab
    Developer](https://developer.schwab.com/products/trader-api--individual).
    This login is different than the login you use for your Schwab
    accounts.
2.  Create an app under Dashboard. The app serves as a middle layer
    between the brokerage account and API.
3.  Identify the App Key and Secret provided by Schwab. These will not
    be fully valid for a few days. When the app is “Ready to use”, you
    should be good.
4.  Under Edit App, create a Callback URL. This can be relatively simple
    (for example: `https://127.0.0.1`).
5.  Pass the App Key and Callback URL to `schwab_auth1_loginURL` to
    generate a URL specific to the app for user log in.
6.  Visit the URL in a web browser and log in to your Schwab account,
    agree to terms and grant the app access to specific accounts.
7.  When “Done” is clicked, it will redirect to a blank page or
    potentially an error page stating “This site can’t be reached”. This
    indicates a successful log in. The URL of this page has the
    authorization code embedded
    (`https://127.0.0.1?code=CO.AUTHORIZATIONCODE`).
8.  Feed the App Key, Callback URL, and entire URL containing the Auth
    code into `schwab_auth2_refreshToken` to get a Refresh Token.
9.  The Refresh Token is valid for 7 days so be sure to store it
    somewhere safe. After 7 days, you need to manually log in again.
10. The Refresh Token is used to generate an Access Token using
    `schwab_auth3_accessToken` which gives account access for 30
    minutes.
11. The most recent Access Token is stored by default into Options.
    Passing it into the functions is optional unless accessing multiple
    logins.

#### Terminology

- Authorization Code: generated from logging into the URL from
  `schwab_auth1_loginURL`.
- Refresh Token: generated using the Authorization Code and is used to
  create access tokens. Refresh token is valid for 7 days.
- Access Token: generated using the Refresh Token and creates the
  connection to the API. Valid for 30 minutes.

## Authentication Example

The `schwab_auth1_loginURL` is used to gain initial access to the API.
Once a Refresh Token is generated using an authorization code, manual
log in will not be required until the Refresh Token expires in 7 days.

``` r

# --------- Step 1 -----------
# Register an App with Schwab Developer, create a Callback URL, and get an App Key and App Secret.
# The callback URL can be any valid URL (for example: https://127.0.0.1).
# Use the schwab_auth1_loginURL to generate an app specific URL. See the Schwab documentation for issues.

callbackURL = 'https://127.0.0.1'
AppKey = 'APPKEY'

schwab::schwab_auth1_loginURL(AppKey, callbackURL)
# "https://api.schwabapi.com/v1/oauth/authorize?client_id=APPKEY&redirect_uri=https://127.0.0.1"

# Visit the URL above to see a Schwab login screen. Log in with your Schwab account details to grant the app access. 


# --------- Step 2 -----------
# A successful log in to the URL from Step 1 will result in a blank page once "Done" is clicked. 
# The URL of this blank page contains the Authorization Code. 
# The blank page may indicate "This site can't be reached". The URL still contains a valid Authorization Code.
# Feed the Authorization Code URL into schwab_auth2_refreshToken to get a Refresh Token.

authCode = 'https://127.0.0.1?code=CO.AUTHORIZATIONCODE&session=ABCD' # This could be very long
refreshToken = schwabr::schwab_auth2_refreshToken(AppKey, AppSecret, callbackURL, authCode)
# "Successful Refresh Token Generated"

# Save the Refresh Token to a safe location so it can be retrieved as needed. It will be valid for 7 days.
saveRDS(refreshToken$refresh_token,'/secure/location/')


# --------- Step 3 -----------
# Use the Refresh Token to get an Access Token
# The function will return an Access Token and also store it for use as a default token in Options

refreshToken = readRDS('/secure/location/')
accessTokenList = schwab_auth3_accessToken(appKey, appSecret, refreshToken)
# "Successful Login. Access Token has been stored and will be valid for 30 minutes"

# Authentication has been completed. Other functions can now be used.


# --------- Automation -----------
# You may be able to use Python and Selenium to partially automate this process if you are familiar with such tools.
```

## Get Account Data

Use the `schwabr_accountData` to get current account data that includes
balances, positions, and account numbers.

``` r
library(schwabr)

refreshToken = readRDS('/secure/location/')
appKey = 'APP_KEY'
appSecret = 'APP_SECRET'
accessTokenList = schwabr::schwab_auth3_accessToken(appKey, appSecretm refreshToken)

actDF = schwab_accountData()
str(actDF)
# List of 3
# $ balances : tibble [1 × 33] (S3: tbl_df/tbl/data.frame)
#  ..$ account_number                   : chr "1234"
#  ..$ type                            : chr "MARGIN"
#  ..$ roundTrips                      : int 0
#  ..$ isDayTrader                     : logi TRUE
#  ..$ isClosingOnlyRestricted         : logi FALSE
#  ..$ accruedInterest                 : num 0
#  ..$ cashBalance                     : num 0
#  ..$ cashReceipts                    : num 0
#  ..$ longOptionMarketValue           : num 0
#  ..$ liquidationValue                : num 0

actList = schwab_accountData('list')
str(actList)
# List of 3
# $ balances :List of 1
#  ..$ :List of 2
#  .. ..$ securitiesAccount:List of 9
#  .. .. ..$ type                   : chr "MARGIN"
#  .. .. ..$ account_number          : chr "1234"
#  .. .. ..$ roundTrips             : int 0
#  .. .. ..$ isDayTrader            : logi TRUE
#  .. .. ..$ isClosingOnlyRestricted: logi FALSE
```

## Get Pricing Data

Use the `price` functions to get quotes or historical pricing. Quotes
will be real-time if the account has access to real-time quotes.

``` r
library(schwabr)

refreshToken = readRDS('/secure/location/')
appKey = 'APP_KEY'
appSecret = 'APP_SECRET'
accessTokenList = schwabr::schwab_auth3_accessToken(appKey, appSecret, refreshToken)

### Quote data
SP500Qt = schwabr::schwab_priceQuote(c('SPY', 'IVV', 'VOO'))
str(SP500Qt)

# tibble [3 × 71] (S3: tbl_df/tbl/data.frame)
# $ assetMainType                     : chr [1:3] "EQUITY" "EQUITY" "EQUITY"
# $ assetSubType                      : chr [1:3] "ETF" "ETF" "ETF"
# $ quoteType                         : chr [1:3] "NBBO" "NBBO" "NBBO"
# $ realtime                          : logi [1:3] TRUE TRUE TRUE
# $ ssid                              : int [1:3] 1281357639 354190790 2003991281
# $ symbol                            : chr [1:3] "SPY" "IVV" "VOO"


# Historical Data
SP500H = schwabr::schwab_priceHistory(c(c('SPY','IVV','VOO')))
head(SP500H)
# ticker date       date_time            open  high   low close   volume
# <chr>  <date>     <dttm>              <dbl> <dbl> <dbl> <dbl>    <int>
# 1 SPY    2024-12-26 2024-12-26 01:00:00  600.  602.  598.  601. 41338891
# 2 SPY    2024-12-27 2024-12-27 01:00:00  598.  598.  591.  595. 64969310
# 3 SPY    2024-12-30 2024-12-30 01:00:00  588.  592.  584.  588. 56578757
# 4 SPY    2024-12-31 2024-12-31 01:00:00  590.  591.  584.  586. 57052654
# 5 SPY    2025-01-02 2025-01-02 01:00:00  589.  591.  580.  585. 50203975
# 6 SPY    2025-01-03 2025-01-03 01:00:00  588.  593.  586.  592. 37888459


# Time series data
# History is only available back to a certain time depending on frequency
schwabr::schwab_priceHistory('AAPL', startDate = Sys.Date()-1, freq='5min')
# A tibble: 78 × 8
# ticker date       date_time            open  high   low close  volume
# <chr>  <date>     <dttm>              <dbl> <dbl> <dbl> <dbl>   <int>
# 1 AAPL   2025-01-23 2025-01-23 09:30:00  225.  226.  224.  225. 2734142
# 2 AAPL   2025-01-23 2025-01-23 09:35:00  225.  226.  225.  226.  905901
# 3 AAPL   2025-01-23 2025-01-23 09:40:00  226.  226.  225.  225.  739369
# 4 AAPL   2025-01-23 2025-01-23 09:45:00  225.  225.  224.  225.  677993
# 5 AAPL   2025-01-23 2025-01-23 09:50:00  225.  226.  225.  225.  554527
# 6 AAPL   2025-01-23 2025-01-23 09:55:00  225.  226.  225.  225.  619021
# 7 AAPL   2025-01-23 2025-01-23 10:00:00  225.  226.  225.  226.  689537
# 8 AAPL   2025-01-23 2025-01-23 10:05:00  226.  226.  225.  226.  993476
# 9 AAPL   2025-01-23 2025-01-23 10:10:00  226.  226.  226.  226.  589547
# 10 AAPL   2025-01-23 2025-01-23 10:15:00  226.  227.  226.  227.  861674

```

## Placing Trades

Order entry offers hundreds of potential combinations. It is strongly
recommended to submit trades outside market hours first to test the
trade entries. You can confirm proper entry on the Schwab website before
entering. See the [order sample
guide](https://developer.schwab.com/products/trader-api--individual/details/documentation/Retail%20Trader%20API%20Production)
for more examples. Please note, `schwab_placeOrder` only allows for
single order entry and will not support some of the complex examples in
the guide.

``` r
library(schwabr)

# Set Access Token using a valid Refresh Token
refreshToken = readRDS('/secure/location/')
appKey = 'APP_KEY'
appSecret = 'APP_SECRET'
accessTokenList = schwabr::schwab_auth3_accessToken(appKey, appSecretm refreshToken)
account_number = 1234567890

# Market Order
Ord0 = schwabr::schwab_placeOrder(account_number,
                                  ticker = 'PSLV',
                                  quantity = 1,
                                  instruction = 'BUY')
schwabr::schwab_cancelOrder(Ord0$orderId, account_number)
# [1] "Order Cancelled"



# Good till cancelled stop limit INCORRECT ENTRY
Ordr1 = schwabr::schwab_placeOrder(account_number = account_number,
                                  ticker = 'SCHB',
                                  quantity = 1,
                                  instruction = 'buy',
                                  duration = 'good_till_cancel',
                                  orderType = 'stop_limit',
                                  limitPrice = 50,
                                  stopPrice = 49)
# Error: 400 - The stop price must be above the current ask for buy stop orders 
#        and below the bid for sell stop orders.



# Good till Cancelled Stop Limit Order correct entry
Ordr1 = schwabr::schwab_placeOrder(account_number = account_number,
                                   ticker = 'SCHB',
                                   quantity = 1,
                                   instruction = 'buy',
                                   duration = 'good_till_cancel',
                                   orderType = 'stop_limit',
                                   limitPrice = 86,
                                   stopPrice = 85)
schwabr::schwab_cancelOrder(Ordr1$orderId, account_number)
# [1] "Order Cancelled"



# Trailing Stop Order
Ordr2 = schwabr::schwab_placeOrder(account_number = account_number,
                                   ticker = 'SPY',
                                   quantity = 1,
                                   instruction = 'sell',
                                   orderType = 'trailing_stop',
                                   stopPriceBasis = 'BID',
                                   stopPriceType = 'percent',
                                   stopPriceOffset = 10)
schwabr::schwab_cancelOrder(Ordr2$orderId,account_number)
# [1] "Order Cancelled"

# Option Order
Ord3 = schwabr::schwab_placeOrder(account_number = account_number,
                                  ticker = 'SLV_091820P24.5',
                                  quantity = 1,
                                  instruction = 'BUY_TO_OPEN',
                                  duration = 'Day',
                                  orderType = 'LIMIT',
                                  limitPrice = .02,
                                  assetType = 'OPTION')
schwabr::schwab_cancelOrder(Ord3$orderId, account_number)
# [1] "Order Cancelled"
```

## Option Chains

You can pull entire option chains for individual securities.

``` r

# Pull all SPY chains for 6 months with 12 strikes above and below current market
SPY = schwab_optionChain('SPY',
                     strikes = 12,
                     endDate = Sys.Date() + 180)

# This returns a list of two data frames
str(SPY$underlying)
# tibble [1 × 23] (S3: tbl_df/tbl/data.frame)
# $ symbol           : chr "SPY"
# $ description      : chr "SPDR S&P 500"
# $ change           : num 2.52
# $ percentChange    : num 0.76
# $ close            : num 332
# $ quoteTime        : num 1.6e+12
# $ tradeTime        : num 1.6e+12
# $ bid              : num 334
# $ ask              : num 334
# $ last             : num 335
# $ mark             : num 334
# $ markChange       : num 1.53
# $ markPercentChange: num 0.46
# $ bidSize          : int 300
# $ askSize          : int 100
# $ highPrice        : num 338
# $ lowPrice         : num 333
# $ openPrice        : num 333
# $ totalVolume      : int 101506148
# $ exchangeName     : chr "PAC"
# $ fiftyTwoWeekHigh : num 359
# $ fiftyTwoWeekLow  : num 218
# $ delayed          : logi TRUE

str(SPY$fullChain)
# $ putCall               : chr [1:552] "PUT" "PUT" "PUT" "PUT" ...
# $ symbol                : chr [1:552] "SPY_093020P329" "SPY_093020P330" "SPY_093020P331" "SPY_093020P332" ...
# $ description           : chr [1:552] "SPY Sep 30 2020 329 Put (Quarterly)" "SPY Sep 30 2020 330 Put (Quarterly)" "SPY Sep 30 2020 331 Put (Quarterly)" "SPY Sep 30 2020 332 Put (Quarterly)" ...
# $ exchangeName          : chr [1:552] "OPR" "OPR" "OPR" "OPR" ...
# $ bid                   : num [1:552] 0 0 0.01 0.01 0.04 0.3 1.15 2.02 3.14 4.1 ...
# $ ask                   : num [1:552] 0.01 0.01 0.02 0.02 0.05 0.38 1.25 2.33 3.24 4.61 ...
# $ last                  : num [1:552] 0.01 0.02 0.01 0.02 0.05 0.32 1.22 2.35 3.02 4.3 ...
# $ mark                  : num [1:552] 0.01 0.01 0.02 0.02 0.05 0.34 1.2 2.17 3.19 4.36 ...
# $ bidSize               : int [1:552] 0 0 6739 1457 390 40 10 10 10 15 ...
# $ askSize               : int [1:552] 4927 3498 6062 3177 10 15 10 141 10 150 ...
# $ bidAskSize            : chr [1:552] "0X4927" "0X3498" "6739X6062" "1457X3177" ...
# $ lastSize              : int [1:552] 0 0 0 0 0 0 0 0 0 0 ...
# $ highPrice             : num [1:552] 0.33 0.49 0.67 1 1.4 1.93 2.58 3.2 4.1 5 ...
# $ lowPrice              : num [1:552] 0.01 0.01 0.01 0.01 0.01 0.02 0.07 0.21 0.36 0.66 ...
# $ openPrice             : num [1:552] 0 0 0 0 0 0 0 0 0 0 ...
# $ closePrice            : num [1:552] 0.61 0.81 1.08 1.4 1.8 2.28 2.85 3.51 4.25 5.06 ...
# $ totalVolume           : int [1:552] 29439 60708 55127 95477 127601 162990 158762 130057 61796 36514 ...
# $ tradeDate             : logi [1:552] NA NA NA NA NA NA ...
# $ tradeTimeInLong       : num [1:552] 1.6e+12 1.6e+12 1.6e+12 1.6e+12 1.6e+12 ...
# $ quoteTimeInLong       : num [1:552] 1.6e+12 1.6e+12 1.6e+12 1.6e+12 1.6e+12 ...
# $ netChange             : num [1:552] -0.6 -0.8 -1.07 -1.38 -1.75 -1.96 -1.63 -1.16 -1.23 -0.76 ...
# $ volatility            : num [1:552] 11.52 9.39 8.48 5.92 NaN ...
# $ delta                 : num [1:552] -0.008 -0.009 -0.027 -0.036 NaN NaN -0.889 -0.952 -0.949 -0.88 ...
# $ gamma                 : num [1:552] 0.01 0.015 0.041 0.077 NaN NaN 0.202 0.077 0.055 0.057 ...
# $ theta                 : num [1:552] -0.021 -0.02 -0.046 -0.042 NaN NaN -0.102 -0.078 -0.113 -0.362 ...
# $ vega                  : num [1:552] 0.004 0.004 0.011 0.014 0.045 0.07 0.033 0.017 0.018 0.035 ...
# $ rho                   : num [1:552] 0 0 0 0 NaN NaN -0.008 -0.009 -0.009 -0.008 ...
# $ openInterest          : int [1:552] 19356 25285 12418 10482 12659 7611 12022 3251 2734 2637 ...
# $ timeValue             : num [1:552] 0.01 0.02 0.01 0.02 0.05 0.32 1.11 1.24 0.91 1.19 ...
# $ theoreticalOptionValue: num [1:552] 0.005 0.005 0.015 0.015 NaN ...
# $ theoreticalVolatility : num [1:552] 29 29 29 29 29 29 29 29 29 29 ...
# $ optionDeliverablesList: logi [1:552] NA NA NA NA NA NA ...
# $ strikePrice           : num [1:552] 329 330 331 332 333 334 335 336 337 338 ...
# $ expirationDate        : num [1:552] 1.6e+12 1.6e+12 1.6e+12 1.6e+12 1.6e+12 ...
# $ daysToExpiration      : int [1:552] 0 0 0 0 0 0 0 0 0 0 ...
# $ expirationType        : chr [1:552] "Q" "Q" "Q" "Q" ...
# $ lastTradingDay        : num [1:552] 1.6e+12 1.6e+12 1.6e+12 1.6e+12 1.6e+12 ...
# $ multiplier            : num [1:552] 100 100 100 100 100 100 100 100 100 100 ...
# $ settlementType        : chr [1:552] " " " " " " " " ...
# $ deliverableNote       : chr [1:552] "" "" "" "" ...
# $ isIndexOption         : logi [1:552] NA NA NA NA NA NA ...
# $ percentChange         : num [1:552] -98.4 -97.5 -99.1 -98.6 -97.2 ...
# $ markChange            : num [1:552] -0.61 -0.81 -1.07 -1.38 -1.75 -1.94 -1.65 -1.34 -1.06 -0.7 ...
# $ markPercentChange     : num [1:552] -99.2 -99.4 -98.6 -98.9 -97.5 ...
# $ nonStandard           : logi [1:552] FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ inTheMoney            : logi [1:552] FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ mini                  : logi [1:552] FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ expireDate            : Date[1:552], format: "2020-09-30" "2020-09-30" "2020-09-30" "2020-09-30" ...

```
