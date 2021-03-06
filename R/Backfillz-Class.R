#' An S4 class to represent a the backfillz plot object.
#'
#' @slot theme_name             Name of current theme
#' @slot theme_text_family      Text family theme option for plots
#' @slot theme_text_font        Text font theme option for plots
#' @slot theme_text_font_colour Text font colour theme option for plots
#' @slot theme_text_cex_title   Text size title for plots
#' @slot theme_text_cex_main    Text size main for plots
#' @slot theme_text_cex_axis    Text size axis for plots
#' @slot theme_text_col_title   Text colour title for plots
#' @slot theme_text_col_main    Text colour main for plots
#' @slot theme_text_col_axis    Text colour axis for plots
#' @slot theme_bg_colour        Background colour theme option for plots
#' @slot theme_mg_colour        Middleground colour theme option for plots
#' @slot theme_fg_colour        Foregound colour option for plots
#' @slot theme_alpha            Transparency option for plots
#' @slot theme_palette          List of hexdecimal colours for use in plots
#' @slot mcmc_samples           MCMC samples from a bayesian model fit
#' @slot mcmc_model             Text definition of MCMC model
#' @slot plot_history           Data frame with overall plot history
#' @slot plot_store             List containing stored plots
#' @slot df_slice_histogram     Slot containing data from
#'  plot_slice_histrogram function
#' @slot df_trace_dial          Slot containing data from
#'  plot_slice_dial function
#' @slot df_spiral_stream       Slow containing data from
#'  plot_spiral_stream function
backfillz <- methods::setClass("backfillz",
                               slots = list(
                                 theme_name = "character",
                                 theme_text_family = "character",
                                 theme_text_font = "numeric",
                                 theme_text_font_colour = "character",
                                 theme_text_cex_title = "numeric",
                                 theme_text_cex_main = "numeric",
                                 theme_text_cex_axis = "numeric",
                                 theme_text_col_title = "character",
                                 theme_text_col_main = "character",
                                 theme_text_col_axis = "character",
                                 theme_bg_colour = "character",
                                 theme_mg_colour = "character",
                                 theme_fg_colour = "character",
                                 theme_alpha = "numeric",
                                 theme_palette = "list",
                                 mcmc_samples = "array",
                                 mcmc_model = "character",
                                 plot_history = "data.frame",
                                 plot_store = "list",
                                 df_slice_histogram = "data.frame",
                                 df_trace_dial = "data.frame",
                                 df_spiral_stream = "data.frame"
                                 )
                               )

# set plot method
methods::setMethod(
  f = "plot",
  signature = c(x = "backfillz", y = "missing"),
  definition = function(x, y, ...) {
    plot_backfillz <- function(x, plot_type = "slice_histogram", ...) {
      if (plot_type == "slice_histogram") {
        message("Plotting slice histogram")
        object <- plot_slice_histogram(x, ...)
      } else if (plot_type == "trace_dial") {
        message("Plotting trace dial")
        object <- plot_trace_dial(x, ...)
      } else if (plot_type == "spiral_stream") {
        message("Plotting spiral stream plot")
        object <- plot_spiral_stream(x, ...)
      }
      else {
        message("Could not find plot type. Please specify slice_histogram")
        object <- x
      }
      return(object)
    }
    plot_backfillz(x, ...)
  }
)

# set summary method
methods::setMethod(
  f = "summary",
  signature = "backfillz",
  definition = function(object) {
    message(cat("backfillz S4 Object"))

    print(object@plot_history)
  }
)

# set show method
methods::setMethod(
  f = "show",
  signature = "backfillz",
  definition = function(object) {
    message(cat("Backfillz S4 Object"))

    message(cat("\nMCMC sample"))
    if (length(object@mcmc_samples > 0)) {
      message(cat(paste("Samples:", attributes(object@mcmc_samples)$dim[1])))
      message(cat(paste("Chains",
        length(attributes(object@mcmc_samples)$dimnames$chains))))
    } else {
      message(
        cat(paste0("No MCMC samples in Backfillz object.",
        " Please run as_backfillz on a stanfit object."))
      )
    }

    message(cat("\nPlot parameters"))
    if (length(object@theme_name) > 0) {
      message(cat(paste("Theme: ", object@theme_name)))
    } else {
      message(cat(paste0("No theme set. ",
      "Please run set_theme on this Backfillz object.")))
    }

    message(cat("\nPlot history"))
    print(object@plot_history)

    }
  )
