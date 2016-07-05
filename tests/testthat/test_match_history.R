# match history tests

test_that( "match_history_url", {
  # test with just key arguments
  expect_equal(match_history_url(api_key = "<my_key>"),
               "https://api.steampowered.com/IDOTA2Match_570/GetMatchHistory/V001/?format=JSON&key=<my_key>")

  # test https flag off
  expect_equal(match_history_url(api_key = "<my_key>", https = FALSE),
               "http://api.steampowered.com/IDOTA2Match_570/GetMatchHistory/V001/?format=JSON&key=<my_key>")

  # test with arbitrary parameter
  expect_equal(match_history_url(api_key = "<my_key>", account_id = 123456789),
               "https://api.steampowered.com/IDOTA2Match_570/GetMatchHistory/V001/?format=JSON&key=<my_key>&account_id=123456789")

})
