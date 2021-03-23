#' Convert stanFit object to a Backfillz object
#'
#' @param object        Stanfit or dataframe.
#' @param verbose       Set to TRUE to see function messages
#' Dataframe must contain columns 'parameters', 'iterations' and 'chains'.
as_backfillz <- function(object, verbose = TRUE) {

  # check arguments
  stopifnot((class(object) == "stanfit") | (class(object) == "data.frame"))

  # create backfillz object
  backfillz_object <- methods::new("backfillz")

  # populate backfillz object and set theme
  if (class(object) == "stanfit") {
    backfillz_object@mcmc_samples <-
      rstan::extract(object, permuted = FALSE, inc_warmup = TRUE)
    backfillz_object@mcmc_model <- object@stanmodel@model_code
  } else if (class(object) == "data.frame") {
    backfillz_object@mcmc_samples <- df_to_stanarray(object)
    backfillz_object@mcmc_model <- "Samples imported from dataframe"
  }

  # set default theme
  backfillz_object <- set_theme(backfillz_object, verbose)

  # initialise plot history
  backfillz_object@plot_history <- data.frame(
    ID = 1,
    Date = date(),
    Event = "Object Creation",
    R_version = R.Version()$version.string,
    Saved = FALSE,
    stringsAsFactors = FALSE
  )

  return(backfillz_object)
}
