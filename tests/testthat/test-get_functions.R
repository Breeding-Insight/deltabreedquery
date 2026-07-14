url_oat <- "https://rel-test.breedinginsight.net/v1/programs/f152169d-049f-4a7c-b5d8-c725b14e66f0"
token <- "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJUeXIgV2llc25lci1IYW5rcyIsIm5iZiI6MTc4MzQ1MDk2Mywicm9sZXMiOlsiU3lzdGVtIEFkbWluaXN0cmF0b3IiXSwiaXNzIjoiYmktYXBpIiwiaWQiOiI5MzgxYTM2NC00Y2MwLTQ3ODItYTNlMy01YTQyMDg2NTljMWIiLCJleHAiOjE3ODM0OTQxNjMsImlhdCI6MTc4MzQ1MDk2M30.TOFF2gpq_2ISOIgxRkMArAeN1eA4-gYxjuJnn5_AELY"

test_that("all get functions fail when authentication is missing", {
  # this shouldn't be required, but devtools::check is somehow not isolating these test scopes
  logout_deltabreed()
  expect_error(get_germplasm())
  expect_error(get_experiments())
  expect_error(get_variables())
  expect_error(get_observations())
})

test_that("germplasm df is correct shape", {
  vcr::local_cassette("get_germplasm")
  login_deltabreed(url_oat, token, verbose = FALSE)
  germ <- get_germplasm()
  expect_shape(germ, dim = c(40,9))

  germ_mapping <- define_mapping_germplasm()
  expect_identical(colnames(germ), names(germ_mapping))
})

test_that("variables df is correct shape and has correct column names", {
  vcr::local_cassette("get_variables")
  login_deltabreed(url_oat, token, verbose = FALSE)
  vars <- get_variables(verbose = TRUE)
  expect_shape(vars, dim = c(24,11))

  var_mapping <- define_mapping_variables()
  expect_identical(colnames(vars), names(var_mapping))
})

test_that("experiments df is correct shape and has correct column mames", {
  vcr::local_cassette("get_experiments")
  login_deltabreed(url_oat, token, verbose = FALSE)
  expts <- get_experiments(verbose = TRUE)
  expect_shape(expts, dim = c(4,7))

  expt_mapping <- define_mapping_expts()
  expect_identical(colnames(expts), names(expt_mapping))
})

test_that("observation df is correct shape", {
  vcr::local_cassette("get_observations")
  login_deltabreed(url_oat, token, verbose = FALSE)
  obs <- get_observations(verbose = FALSE)
  expect_shape(obs, dim = c(254, 36))
})

test_that("observation df is correct shape after dropping NA cols", {
  vcr::local_cassette("get_obs_naomit")
  login_deltabreed(url_oat, token, verbose = FALSE)
  obs_naomit <- get_observations(drop_empty_columns = TRUE)
  expect_shape(obs_naomit, dim = c(254, 34))
})



