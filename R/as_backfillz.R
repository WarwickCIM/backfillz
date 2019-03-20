#' Convert stanFit object to a Backfillz object
#'
#' @param stanfit_object  Backfillz object
as_backfillz <- function(stanfit_object) {

  # check inputs
  assertive::is_s4(stanfit_object)
  stopifnot(class(stanfit_object) == 'stanfit')

  # create backfillz object
  backfillz_object <- methods::new('Backfillz')

  # populate backfillz object and set theme
  backfillz_object@mcmc_samples <- rstan::extract(stanfit_object, permuted = FALSE)
  backfillz_object <- set_theme(backfillz_object)

  return(backfillz_object)
}
