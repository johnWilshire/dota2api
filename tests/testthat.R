library(testthat)
library(dota2api)


api_key <- scan("keyfile.txt", what = character())

print(getwd())
test_check("dota2api")
