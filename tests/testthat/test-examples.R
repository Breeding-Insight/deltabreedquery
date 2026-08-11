# Example dataset contains responses from an instance loaded with small amount
# of dummy data. All responses saved out as static JSONs, see files in /inst/
# for more details

test_that("example login is working", {
  expect_no_error(login_deltabreed("example", verbose = FALSE))
})

test_that("get_experiments functions on example dataset", {
  login_deltabreed("example", verbose = FALSE)
  exp <- get_experiments(verbose = FALSE)
  exp2 <- get_experiments(verbose = FALSE, include_dbids = TRUE)
  expect_all_true(dim(exp) == c(6,7))
  expect_all_true(dim(exp2) == c(6,9))
  expect_equal(nrow(exp2), length(unique(exp2$studyDbId)))
})

test_that("get_germplasm functions on example dataset", {
  login_deltabreed("example", verbose = FALSE)
  germ = get_germplasm()
  expect_all_true(dim(germ) == c(10,9))
  expect_all_true(germ$GID == sort(germ$GID))
})


test_that("get_observations functions on example dataset", {
  login_deltabreed("example", verbose = FALSE)
  obs = get_observations(verbose = FALSE)
  obs_dropped = get_observations(verbose = FALSE, drop_empty_columns = TRUE)
  expect_equal(ncol(obs) - 2, ncol(obs_dropped))
  expect_all_true(dim(obs) == c(130,20))
})

test_that("data typing is working correctly on observation columns", {
  login_deltabreed("example", verbose = FALSE)
  obs = get_observations(verbose = FALSE)
  expect_equal(class(obs$BGPer), "numeric")
  expect_equal(class(obs$Yield), "numeric")
  expect_equal(class(obs$HeadingDate), "Date")
  expect_equal(class(obs$Lodging), "factor")
  expect_equal(class(obs$StripeRust), "factor")
})

test_that("filter_observations returning correct nrows", {
  login_deltabreed("example",
                   verbose = FALSE)
  obs = get_observations(include_dbids = TRUE,
                         verbose = FALSE)
  expect_equal(nrow(filter_observations(year = 2025,
                                        verbose = FALSE)),
               sum(obs$Year == 2025))
  expect_equal(nrow(filter_observations(exp_type = "AYT",
                                        verbose = FALSE)),
               sum(substr(obs$ExpName,1,3) == "AYT"))
  expect_equal(nrow(filter_observations(location = c("Alma", "Tisdale"),
                                        verbose = FALSE)),
               sum(obs$Location != "Manitoba"))

  expect_equal(nrow(filter_observations(location = "Manitoba",
                                        year = 2025,
                                        verbose = FALSE)),
               sum(obs$Location == "Manitoba" & obs$Year == 2025))
})

