#' Converts a data frame to the output structure of extract(stan_object, permuted = TRUE)
#'
#' @param df                data frame with the column names 'parameters', 'iterations' and 'chains'
#' @return                  An array from df with the dimensions [iteration, chain, parameters]
#' @examples
#' df <- data.frame(
#'               parameters = c('mu', 'mu'),
#'               iterations = c(0.345, 0.263),
#'               chains = c(1, 2),
#'               stringsAsFactors = FALSE
#'               )
#' df_to_stanarray(df)
df_to_stanarray <- function(df) {

  # Check input
  assertive::assert_is_data.frame(df)
  assertive::assert_is_character(df$parameters)
  assertive::assert_is_numeric(df$iterations)
  assertive::assert_is_numeric(df$chains)

  # Sort dataframe alphabetically by parameters then numerically by chain
  df <- df[order(df$parameters, df$chains),]

  # Get properties of sample
  n_iterations <- length(df$iterations[df$parameters == df$parameters[1] & df$chains == df$chains[1]])
  chain_names <- unique(df$chains)
  n_chains <- length(chain_names)
  parameter_names  <- unique(df$parameters)
  n_parameters <- length(parameter_names)

  # Create multidimensional array consistent with the stan output
  array(
    data = df$iterations,
    dim = c(n_iterations, n_chains, n_parameters),
    dimnames = list(
      iterations = NULL,
      chains = paste('chain:', chain_names, sep = ''),
      parameters = parameter_names
    )
  )
}

