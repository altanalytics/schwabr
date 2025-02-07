test_that("Function response type matches expectation", {

  ### No tests will work on Cran because no access
  ### All tests bundled into one to only require one log in
  skip_on_cran()

  library(jsonlite)

  #saveRDS(account_number,'/Users/tonytrevisan/Downloads/actnum.RDS')
  account_number = readRDS('~/actnum.RDS')
  accessTokenList = readRDS('~/actlist.RDS')
  options(schwab_access_token = accessTokenList)


  account_number = schwab_accountData()$accounts$accountNumber[1]


  ### Check account information
  expect_output(str(schwab_accountData()), "List of 3")
  expect_output(str(schwab_accountData(output='list')), "List of 3")

  expect_error(schwab_accountData(accessTokenList = 'adfsd')) # expect fail



  ### Check pricing
  SP500Qt = schwab_priceQuote(c('SPY','IVV','VOO'))
  expect_equal(nrow(SP500Qt), 3)

  SP500H = schwab_priceHistory(c(c('SPY','IVV','VOO')))
  expect_equal(is.data.frame(SP500H), TRUE)
  expect_equal(length(unique(SP500H$ticker)), 3)
  expect_error(schwab_priceQuote(accessToken = 'fail'))


  ### Check Options
  SLV = schwab_optionChain('SLV')
  expect_output(str(SLV), "List of 2")
  expect_equal(nrow(SLV$fullChain)>100,TRUE)

  expect_equal(is.data.frame(schwab_transactSearch(account_number)),TRUE)
  expect_equal(ncol(schwab_symbolDetail('aapl'))>40,TRUE)

  ### Orders
  # ORder search
  AllOrd = schwab_orderSearch(account_number)
  # TestOrder = schwab_orderDetail(AllOrd$executedOrders$orderId[[1]],account_number)
 # expect_equal(length(TestOrder)>15,TRUE)

  ## Place order way above limit
  PSLVQt = schwab_priceQuote('PSLV',output='list')
  Ord3 = schwab_placeOrder(account_number = account_number, ticker='PSLV',
                     quantity = 1, instruction='BUY', duration='Day',
                     orderType = 'LIMIT', limitPrice = round(PSLVQt$PSLV$quote$bidPrice*.5,2))
  # Ord3Res = schwab_cancelOrder(Ord3$orderId,account_number)

  expect_equal(ncol(Ord3),5)
  # expect_match(Ord3Res,'https')
  # expect_match(Ord3Res,'accounts')
  # expect_match(Ord3Res,'orders')

  ### ORder errors
  expect_error(schwab_placeOrder(account_number = account_number, ticker='SLBB_091820P24.5',
                                quantity = 1, instruction='BUY_TO_OPEN', duration='Day',
                                orderType = 'LIMIT', limitPrice = .02, assetType = 'OPTION'))
  expect_error(schwab_placeOrder(account_number = account_number,ticker='pslv',
                      quantity = 1,instruction='buy',duration='good_till_cancel',
                      orderType = 'stop_limit',limitPrice=round(PSLVQt$PSLV$bidPrice*.75,2),stopPrice=round(PSLVQt$PSLV$bidPrice*.8,2)))


})

