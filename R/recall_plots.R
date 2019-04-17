#' Recall a series of plots from the backfillz object using the ID from the summary() function
#'
#' @param object            Backfillz or Stanfit object. Stanfit objects are converted to Backfillz object using the as_backfillz function.
#' @param ID                Vector of strings giving the parameters to plot (e.g., c('mu', 'sd')).
recall_plots <- function(object, ID = NULL) {

  # Check input
  if(!class(object) == 'Backfillz'){
    stop('Object is not a stanfit, Backfillz or data frame object')
  }

  assertive::assert_is_s4(object)
  assertive::assert_is_numeric(ID)

  # plot plots corresponding to id
  found <- 0
  for (i in 1:length(object@plot_store)){ # 1 is the creation entry in the log
    if (object@plot_store[[i]]$ID == ID) {
      print(object@plot_store[[i]]$plot)
      Sys.sleep(time = 5)
      found <- found + 1
    }
  }

  message(paste('Found', found, ' plots matching the ID ', ID))
}
