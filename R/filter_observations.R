

filter_observations <- function(year = numeric(),
                                location = "",
                                expt_name = "",
                                env_name = "",
                                obs_level = "",
                                page_size = 5000){
  if (!auth_exists()) {
    stop("No authentication credentials found.",
         "Please run `login_deltabreed()` to authenticate first.")
  }
  env <- get("deltabreedr_global", envir = .GlobalEnv)
  # first pull the experiments/environments themselves
  expts <- get_experiments(verbose = FALSE)

  # list of DbIds to submit to the
}


filter_observations(year)
