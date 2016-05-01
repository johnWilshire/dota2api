# test account_to_steam

test_that( "account_to_steam", {
  # test with just key arguments
  sixtyfour <- "76561197992765754"
  thirtytwo <- 32500026
  expect_equal(account_to_steam(thirtytwo), sixtyfour)


  expect_equal(account_to_steam(as.character(thirtytwo)), sixtyfour)
})
