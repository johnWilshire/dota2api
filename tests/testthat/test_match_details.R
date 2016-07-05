# match details tests

test_that( "match_details_url", {
  # test with just key arguments
  expect_equal(match_details_url(api_key = "<my_key>", match_id = 12411412),
               "https://api.steampowered.com/IDOTA2Match_570/GetMatchDetails/V001/?format=JSON&key=<my_key>&match_id=12411412")

  # test https flag off
  expect_equal(match_details_url(api_key = "<my_key>", match_id = 12411412, https = FALSE),
               "http://api.steampowered.com/IDOTA2Match_570/GetMatchDetails/V001/?format=JSON&key=<my_key>&match_id=12411412")

})
