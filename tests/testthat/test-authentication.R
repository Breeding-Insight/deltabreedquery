skip_on_cran()

test_that("no auth credentials are found within test_that local scope prior to logging in", {
  expect_false(auth_exists())
})

test_that("login and logout functions work locally in testing scope", {
  vcr::local_cassette("authentication")
  login_deltabreed(reltest_url, reltest_token, verbose = FALSE)
  expect_true(auth_exists())
  expect_no_error(check_auth())
  logout_deltabreed()
  expect_false(auth_exists())
})

test_that("incorrect tokens are rejected as 401", {
  expect_error(login_deltabreed(reltest_url, token = substr(reltest_token, 1, nchar(reltest_token)-1)))
})
