#' Set graphical plot slots of a backfillz object to a theme
#'
#' @param backfillz_object  Backfillz object
#' @param theme             Character specifying theme (e.g., 'default', 'solarized_dark')
set_theme <- function(backfillz_object, theme = 'default') {

  # check inputs
  assertive::is_character(theme)
  assertive::is_s4(backfillz_object)
  stopifnot(class(backfillz_object) == 'Backfillz')

  # set theme values
  if (theme == 'default') {
    message('Setting backfillz object theme to default')
    backfillz_object@theme_name               <- 'default'
    backfillz_object@theme_text_family        <- 'sans'
    backfillz_object@theme_text_font          <- 1
    backfillz_object@theme_text_font_colour   <- 'black'

    backfillz_object@theme_text_cex_title     <- 1.5
    backfillz_object@theme_text_cex_main      <- 1
    backfillz_object@theme_text_cex_axis      <- 0.8

    backfillz_object@theme_text_col_title     <- 'grey10'
    backfillz_object@theme_text_col_main      <- 'grey60'
    backfillz_object@theme_text_col_axis      <- 'grey40'

    backfillz_object@theme_bg_colour          <- 'white'
    backfillz_object@theme_mg_colour          <- 'grey50'
    backfillz_object@theme_fg_colour          <- 'black'
    backfillz_object@theme_alpha              <- 0.7
    backfillz_object@theme_palette            <- list(
      '#FF0000',
      '#0000FF',
      '#FF00FF',
      '#800000',
      '#000080',
      '#FF6347'
    )
  } else if (theme ==  'solarized_dark') {
    message('Setting backfillz object theme to solarized dark')
    backfillz_object@theme_name               <- 'solarized_dark'
    backfillz_object@theme_text_family        <- 'mono'
    backfillz_object@theme_text_font          <- 1
    backfillz_object@theme_text_font_colour   <- '#2AA198'

    backfillz_object@theme_text_cex_title     <- 2
    backfillz_object@theme_text_cex_main      <- 1
    backfillz_object@theme_text_cex_axis      <- 0.8

    backfillz_object@theme_text_col_title     <- 'grey10'
    backfillz_object@theme_text_col_main      <- 'grey60'
    backfillz_object@theme_text_col_axis      <- 'grey40'

    backfillz_object@theme_bg_colour          <- '#002B36'
    backfillz_object@theme_mg_colour          <- 'grey50'
    backfillz_object@theme_fg_colour          <- '#93A1A1'
    backfillz_object@theme_alpha              <- 0.7
    backfillz_object@theme_palette            <- list(
      '#657B83',
      '#D30102',
      '#D33682',
      '#859900',
      '#93A1A1',
      '#268BD2'
    )
  } else if (theme ==  'demo 1') {
    message('Setting backfillz object theme to demo 1')
    backfillz_object@theme_name               <- 'demo 1'
    backfillz_object@theme_text_family        <-  'mono' #"Roboto" # "IBM Plex Mono" # "Bungee Shade" # "IBM Plex Sans Condensed"   # 'mono'
    backfillz_object@theme_text_font          <-  1
    backfillz_object@theme_text_font_colour   <- 'grey20'

    backfillz_object@theme_text_cex_title     <- 1.5
    backfillz_object@theme_text_cex_main      <- 1
    backfillz_object@theme_text_cex_axis      <- 0.6

    backfillz_object@theme_text_col_title     <- 'grey10'
    backfillz_object@theme_text_col_main      <- 'grey60'
    backfillz_object@theme_text_col_axis      <- 'grey40'

    backfillz_object@theme_bg_colour          <- 'grey98'
    backfillz_object@theme_mg_colour          <- 'grey90'
    backfillz_object@theme_fg_colour          <- 'grey40'
    backfillz_object@theme_alpha              <- 0.8
    backfillz_object@theme_palette            <- list(
      '#A3C96D',
      '#DDCF1E',
      '#8E4D91',
      '#003B24',
      '#912B2F',
      '#7C6EAC'
    )

  } else {
    message('Theme not specified so setting backfillz object theme to default')
    backfillz_object@theme_name               <- 'default'
    backfillz_object@theme_text_family        <- 'sans'
    backfillz_object@theme_text_font          <- 1
    backfillz_object@theme_text_font_colour   <- 'black'

    backfillz_object@theme_text_cex_title     <- 2
    backfillz_object@theme_text_cex_main      <- 1
    backfillz_object@theme_text_cex_axis      <- 0.8

    backfillz_object@theme_text_col_title     <- 'grey10'
    backfillz_object@theme_text_col_main      <- 'grey60'
    backfillz_object@theme_text_col_axis      <- 'grey40'

    backfillz_object@theme_bg_colour          <- 'white'
    backfillz_object@theme_mg_colour          <- 'grey50'
    backfillz_object@theme_fg_colour          <- 'black'

    backfillz_object@theme_alpha              <- 0.7
    backfillz_object@theme_palette            <- list(
      '#FF0000',
      '#0000FF',
      '#FF00FF',
      '#800000',
      '#000080',
      '#FF6347'
    )
  }

  return(backfillz_object)
}