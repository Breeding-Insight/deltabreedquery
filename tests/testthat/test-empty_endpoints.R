# base URL for the Empty-program instance on rel-test
empty_prog_url <- "https://rel-test.breedinginsight.net/v1/programs/828857d9-f82e-4ede-9ede-621b1440b191"
token <- "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJUeXIgV2llc25lci1IYW5rcyIsIm5iZiI6MTc4MzQzNzg4Mywicm9sZXMiOlsiU3lzdGVtIEFkbWluaXN0cmF0b3IiXSwiaXNzIjoiYmktYXBpIiwiaWQiOiI5MzgxYTM2NC00Y2MwLTQ3ODItYTNlMy01YTQyMDg2NTljMWIiLCJleHAiOjE3ODM0ODEwODMsImlhdCI6MTc4MzQzNzg4M30.O6-zE4nEft3_xoxc2azreCGF8M0BhzlZDPvD4sJ2a5s"

test_that("responses from empty endpoints are 200 or 500 response", {
  vcr::local_vcr_configure_log(file = stdout())
  vcr::local_cassette("empty_requests")
  for (endpoint in c("germplasm","trials","studies","variables","seasons","observationunits","observations")){
    req <- build_get_request(paste0(empty_prog_url, "/brapi/v2"),
                             token,
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
  expect_no_error(login_deltabreed(empty_prog_url, token))
  expect_no_error(check_auth())
  expect_true(auth_exists())
})

test_that("get_germplasm on empty program returns empty df", {
#  vcr::local_vcr_configure_log(file = stdout())
  vcr::local_cassette("empty_germplasm")
  login_deltabreed(empty_prog_url, token, verbose = FALSE)
  df <- get_germplasm()
  expect_equal(nrow(df), 0)
  expect_equal(class(df), "data.frame")
})

test_that("get_variables() on empty program returns empty df", {
  vcr::local_cassette("empty_variables")
  login_deltabreed(empty_prog_url, token, verbose = FALSE)
  df <- get_variables()
  expect_equal(nrow(df), 0)
  expect_equal(class(df), "data.frame")
})

test_that("get_experiments() on empty program returns empty df", {
  vcr::local_cassette("empty_experiments")
  login_deltabreed(empty_prog_url, token, verbose = FALSE)
  df <- get_experiments()
  expect_equal(nrow(df), 0)
  expect_equal(class(df), "data.frame")
})

test_that("get_observations() on empty program returns empty df", {
  vcr::local_cassette("empty_obs")
  login_deltabreed(empty_prog_url, token, verbose = FALSE)
  df <- get_observations()
  expect_equal(nrow(df), 0)
  expect_equal(class(df), "data.frame")
})

