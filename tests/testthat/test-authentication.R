url_oat <- "https://rel-test.breedinginsight.net/v1/programs/f152169d-049f-4a7c-b5d8-c725b14e66f0"
token <- "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJUeXIgV2llc25lci1IYW5rcyIsIm5iZiI6MTc4MzQ1MDk2Mywicm9sZXMiOlsiU3lzdGVtIEFkbWluaXN0cmF0b3IiXSwiaXNzIjoiYmktYXBpIiwiaWQiOiI5MzgxYTM2NC00Y2MwLTQ3ODItYTNlMy01YTQyMDg2NTljMWIiLCJleHAiOjE3ODM0OTQxNjMsImlhdCI6MTc4MzQ1MDk2M30.TOFF2gpq_2ISOIgxRkMArAeN1eA4-gYxjuJnn5_AELY"

test_that("no auth credentials are found within test_that local scope prior to logging in", {
  expect_false(auth_exists())
})

test_that("login and logout functions work locally in testing scope", {
  vcr::local_cassette("authentication")
  login_deltabreed(url_oat, token, verbose = FALSE)
  expect_true(auth_exists())
  expect_no_error(check_auth())
  logout_deltabreed()
  expect_false(auth_exists())
})

test_that("incorrect tokens are rejected as 401", {
  vcr::local_cassette("auth_401")
  expect_error(login_deltabreed(url_oat, token = substr(token, 1, nchar(token)-1)))
})

