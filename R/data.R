#' Example stanfit object
#'
#' An example stanfit object following the example fit
#'  of a heirarchical schools model from the
#'  pystan documentation.
#'  'https://pystan.readthedocs.io/en/latest/'.
#'
#' @format A stanfit object with 18 parameters,
#'  3 chains, 10000 iterations and 500 warmup samples
#' @examples
#' # show model used to create sample
#' sample_stanfit@stanmodel
#' # print model summary
#' sample_stanfit
"sample_stanfit"
