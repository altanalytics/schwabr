% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/account.R
\name{schwab_accountData}
\alias{schwab_accountData}
\title{Get account balances, positions, and account numbers returned as a list}
\usage{
schwab_accountData(
  output = "df",
  account_number = "",
  value_pull = c("all", "bal", "pos", "acts"),
  accessTokenList = NULL
)
}
\arguments{
\item{output}{Use 'df' for a list of 3 data frames containing balances,
positions, and orders. Otherwise the data will be returned as a list of
lists}

\item{account_number}{The account number as shown on the Schwab website}

\item{value_pull}{Can be one of 'all','bal','pos','acts' depending on what you
want to pull back}

\item{accessTokenList}{A valid Access Token must be set using the output from
\code{\link{schwab_auth3_accessToken}}. The most recent Access Token will be
used by default unless one is manually passed into the function.}
}
\value{
a list of requested account details
}
\description{
Retrieves account data for all accounts linked to the Access Token or a specific account
}
\details{
The output will be either a list of three data frames or a list of three
lists that contain balances, positions, and account numbers for Schwab accounts
linked to the access token or specified. For historical orders, see
\code{\link{schwab_orderSearch}}. The default is for a data frame output which is
much cleaner.
}
\examples{
\dontrun{

# Get stored refresh token
refreshToken = readRDS('/secure/location/')

# Generate a new access token
accessTokenList = schwab_auth3_accessToken(appKey, appSecret, refreshToken)

# Passing the accessTokenList is optional. The default will return balances
asDF = schwab_accountData()
asList = schwab_accountData('list',account_number = '', accessTokenList)

}
}
