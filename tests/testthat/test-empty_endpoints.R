skip_on_cran()

# base URL for the Empty Program instance on rel-test
empty_prog_url <- "https://rel-test.breedinginsight.net/v1/programs/ee1b32e6-287f-4412-902e-1bab9e615b97"

test_that("responses from empty endpoints are 200 or 500 response", {
  vcr::local_cassette("empty_requests")
  for (endpoint in c("germplasm","trials","studies","variables","seasons","observationunits","observations")){
    req <- build_get_request(paste0(empty_prog_url, "/brapi/v2"),
                             reltest_token,
                             endpoint)
    resp <-   req |>
      httr2::req_url_query() |>
      httr2::req_perform()
    expect_in(resp$status_code, c(200,500))
  }
})

# need to test login first in order to record a login cassette
# otherwise downstream login attempts on cassette will fail
test_that("login + authentication still works on empty program",{
  # vcr::local_vcr_configure_log(file = stdout())
  vcr::local_cassette("empty_login")
  expect_no_error(login_deltabreed(empty_prog_url, reltest_token))
  expect_no_error(check_auth())
  expect_true(auth_exists())
})

test_that("get_germplasm on empty program returns empty df", {
#  vcr::local_vcr_configure_log(file = stdout())
  vcr::local_cassette("empty_germplasm")
  login_deltabreed(empty_prog_url, reltest_token, verbose = FALSE)
  df <- get_germplasm()
  expect_equal(nrow(df), 0)
  expect_equal(class(df), "data.frame")
})

test_that("get_variables() on empty program returns empty df", {
  vcr::local_cassette("empty_variables")
  login_deltabreed(empty_prog_url, reltest_token, verbose = FALSE)
  df <- get_variables()
  expect_equal(nrow(df), 0)
  expect_equal(class(df), "data.frame")
})

test_that("get_experiments() on empty program returns empty df", {
  vcr::local_cassette("empty_experiments")
  login_deltabreed(empty_prog_url, reltest_token, verbose = FALSE)
  df <- get_experiments()
  expect_equal(nrow(df), 0)
  expect_equal(class(df), "data.frame")
})

test_that("get_observations() on empty program returns empty df and raises warning", {
  vcr::local_cassette("empty_obs")
  login_deltabreed(empty_prog_url, reltest_token, verbose = FALSE)
  expect_warning(df <- get_observations())
  expect_equal(nrow(df), 0)
  expect_equal(class(df), "data.frame")
})

