#' Creates a trace dial plot
#'
#' @param object            Backfillz or Stanfit object. Stanfit objects are converted to Backfillz object using the as_backfillz function.
#' @param parameters        Vector of strings giving the parameters to plot (e.g., c('mu', 'sd')).
#' @param n_bins            Number of bins displayed in histograms at end of dial.
#' @param n_burnin          Number of burnin samples.
#' @param save_plot         Set to TRUE to save plots in the Backfillz object.
plot_trace_dial <- function(object, parameters = NULL, n_bins = 40, n_burnin = 10, save_plot = FALSE) {

  assertive::assert_is_logical(save_plot)

  # check inputs
  if(!class(object) == 'stanfit' & !class(object) == 'Backfillz' & !class(object) == 'data.frame'){
    stop('Object is not a stanfit, Backfillz or data frame object')
  }

  assertive::assert_is_numeric(n_bins)
  assertive::assert_is_numeric(n_burnin)

  # convert stanfit
  if((class(object) == 'stanfit') | (class(object) == 'data.frame')) {
    object <- as_backfillz(object)
  }


  # Preallocate the data frame stored in the backfillz object
  object@df_trace_dial <- data.frame(
    parameter = character(),
    sample_min = numeric(),
    sample_max= numeric(),
    stringsAsFactors = FALSE
  )

  # assign default of first 2 parameters if parameters are not specified
  if(is.null(parameters)) {
    message('No parameters specified. Plotting the first two model parameters.')
    parameters <- as.array(attributes(object@mcmc_samples)$dimnames$parameters)[1:2]
  }

  # extract theme properties
  colour_trace_line             <- object@theme_text_font_colour
  colour_guide_lines            <- object@theme_fg_colour
  colour_inner_burn_segment     <- alpha(colour = object@theme_mg_colour,
                                     alpha = (object@theme_alpha + 0.2))
  colour_outer_burn_segment     <- alpha(colour = object@theme_mg_colour,
                                     alpha = (object@theme_alpha + 0.1))
  colour_remaining_segment      <- alpha(colour = object@theme_mg_colour,
                                         alpha = (object@theme_alpha - 0.3))


  create_single_plot <- function(parameter) {
    # Makes plot of samples for a single parameter
    #
    # Args:
    #           parameter: the parameter we want to plot as a string
    #           n_bins: number of bins in the histogram
    #           n_burnin: number of burnin samples
    #
    # Returns:
    #           updated backfillaz object
    #           MCMC trace dial plot for a single parameter to the plot device
    # switch(Sys.info()['sysname'],
    #        Darwin = quartz(),
    #        dev.new()
    # )

    #dev.new()

    message(paste('Plotting parameter:', parameter))

    # Extract sample parameters
    chains <- object@mcmc_samples[,,parameter]
    n_samples <- dim(chains)[1]
    n_chains <- dim(chains)[2]
    max_sample <- max(chains)
    min_sample <- min(chains)

    # positions of 3/4 of a circle where trace is plotted
    positions <- seq(from = 0.5 * pi, to = 2 * pi, length.out =  n_samples)
    y_positions <- sin(positions)
    x_positions <- cos(positions)

    # scaling function
    scale_0_1 <- function(x) { (x - min(x)) / max(x) }

    # apply scale function to each chain
    chains_0_1 <- apply(X = chains, MARGIN = 2, FUN = scale_0_1)

    # whole circle positions
    full_x_positions <- x_positions * chains_0_1
    full_y_positions <- y_positions * chains_0_1

    # donut circle positions
    chains_donut <- (chains_0_1 * (2/3)) + 1/3
    donut_x_positions <- x_positions * chains_donut
    donut_y_positions <- y_positions * chains_donut

    # polygon positions
    burnin_polygon_positions <- list(
      inner = list(
        x = c(x_positions[1:n_burnin] * (1/3), rev(x_positions[1:n_burnin] * (2/3))),
        y = c(y_positions[1:n_burnin] * (1/3), rev(y_positions[1:n_burnin] * (2/3)))
      ),
      outer = list(
        x = c(x_positions[1:n_burnin] * (2/3), rev(x_positions[1:n_burnin] * (3/3))),
        y = c(y_positions[1:n_burnin] * (2/3), rev(y_positions[1:n_burnin] * (3/3)))
      )
    )

    remaining_polygon_positions <- list(
      x = c(x_positions[n_burnin:n_samples] * (1/3), rev(x_positions[n_burnin:n_samples] * 1)),
      y = c(y_positions[n_burnin:n_samples] * (1/3), rev(y_positions[n_burnin:n_samples] * 1))
    )

    # set up
    border <- 0.08
    par(fig = c(border, 1 - border, border, 1 - border),
        family = object@theme_text_family,
        font = object@theme_text_font,
        bg = object@theme_bg_colour,
        col.lab = object@theme_text_font_colour,
        col.axis = object@theme_text_font_colour)

    par(mar=c(0, 0, 0, 0))

    # base plot
    plot(
      x_positions,
      y_positions,
      type = 'n',
      axes = FALSE,
      xlim = c(-1, 1),
      ylim = c(-1, 1),
      xaxs="i",
      yaxs="i"
      )

    text(0, 0, parameter, col = colour_guide_lines)

    # Add plot labels

    # bottom right
    mtext(
      at = 1,
      text = 'BackFillz.R by CIM, University of Warwick',
      side = 1,
      cex = object@theme_text_cex_axis ,
      col = object@theme_text_col_axis,
      adj = 1
    )

    # top left
    mtext(
      at = -0.4,
      text = 'Spiral plot',
      side = 3,
      cex = object@theme_text_cex_axis ,
      col = object@theme_text_col_axis,
      adj = 1
    )

    # draw polygons and annotations

    # polygons
    polygon(
      burnin_polygon_positions$inner$x,
      burnin_polygon_positions$inner$y,
      col = colour_inner_burn_segment,
      border = 'white'
    )

    polygon(
      burnin_polygon_positions$outer$x,
      burnin_polygon_positions$outer$y,
      col = colour_outer_burn_segment,
      border = 'white'
    )

    polygon(
      remaining_polygon_positions$x,
      remaining_polygon_positions$y,
      col = colour_remaining_segment,
      border = 'white'
    )

    # labels
    axoutr <- 0.32

    # start of sample label
    segments(0, 0.25, 0, axoutr, col = colour_guide_lines, lwd = 1)
    text(0, 0.28, labels = '0', pos = 1, cex = 0.6, col = colour_guide_lines)

    # burnin label
    segments(
      x_positions[n_burnin] * 0.22,
      y_positions[n_burnin] * 0.22,
      x_positions[n_burnin] * axoutr,
      y_positions[n_burnin] * (1/3),
      col = colour_guide_lines,
      lwd = 1
    )

    text_position <- NULL
    text(
      x_positions[n_burnin] * 0.2,
      y_positions[n_burnin] * 0.2,
      labels = n_burnin,
      cex = 0.6,
      col = colour_guide_lines,
      font = 1,
      pos = text_position
    )

    # end of sample label
    segments(
      x_positions[n_samples] * (1/4),
      y_positions[n_samples] * (1/4),
      x_positions[n_samples] * axoutr,
      y_positions[n_samples] * (1/3),
      col = colour_guide_lines,
      lwd = 1
    )

    text(
      x_positions[n_samples] * 0.28,
      y_positions[n_samples] * 0.25,
      labels = n_samples,
      pos = 2,
      cex = 0.6,
      col = colour_guide_lines
    )

    # draw inner axis and iteration steps
    tenths <- seq(1, n_samples, by = 10)
    hundreths <- seq(1, n_samples, by = 100)

    segments(
      x_positions[tenths] * axoutr,
      y_positions[tenths] * axoutr,
      x_positions[tenths] * 0.31,
      y_positions[tenths] * 0.31,
      col = colour_guide_lines,
      lwd = 0.1
    )

    segments(
      x_positions[hundreths] * axoutr,
      y_positions[hundreths] * axoutr,
      x_positions[hundreths] * 0.3,
      y_positions[hundreths] * 0.3,
      col = colour_trace_line,
      lwd = 0.5
    )

    lines(
      x_positions[n_burnin:n_samples] * (2/3),
      y_positions[n_burnin:n_samples] * (2/3),
      col = colour_guide_lines
    )

    # draw data
    for (i in 1:n_chains) {

      # burnin
      lines(
        donut_x_positions[1:n_burnin, i],
        donut_y_positions[1:n_burnin, i],
        col = alpha(colour = object@theme_palette[[i]],
                    alpha = 0.7),
        lwd = 1
        )

      # remaining sample
      lines(
        donut_x_positions[n_burnin:n_samples, i],
        donut_y_positions[n_burnin:n_samples, i],
        col = object@theme_palette[[i]],
        lwd = 1
        )
    }

    # chain start axis
    par(fig = c(0.49, 0.5, 0.5 + (1/3) * (0.5 - border), 1 - border), new = TRUE,
        family = object@theme_text_family,
        font = object@theme_text_font,
        col.lab = object@theme_text_font_colour,
        col.axis = object@theme_text_font_colour)

    par(mar = c(0, 0, 0, 0))

    plot(
      -99, -99,
      type = 'n',
      axes = FALSE,
      ylim = c(max_sample, min_sample),
      yaxs = 'i'
      )

    ## burnin axis
    axis(
      4,
      cex.axis= 0.5,
      las = 1,
      col.axis = colour_guide_lines,
      col = colour_guide_lines
      )

    axis(
      4,
      at = seq(floor(min_sample - 0.1 * min_sample), max_sample + 0.1 * max_sample, by = 1),
      labels = FALSE,
      cex.axis = 0.5,
      las = 1,
      col.axis = colour_guide_lines,
      col = colour_guide_lines,
      tck = -0.1
      )

    # Calculate burnin and remaining sample histogram properties
    histogram_breaks <- c(seq(floor(min_sample), ceiling(max_sample), length = (n_bins + 1)))

    hist_create <- function(x) {
      hist(
        x,
        breaks = histogram_breaks,
        plot = FALSE
      )
    }

    histogram_test  <- hist_create(chains)
    histogram_1     <- hist_create(chains[1:n_burnin])
    histogram_2     <- hist_create(chains[n_burnin:n_samples])

    histograms_max_density <- max(
      histogram_test$density,
      histogram_1$density,
      histogram_2$density
    )

    # draw histograms
    par(
      fig = c(
        0.5 + (1/3) * (0.5 - border),
        1 - border,
        0.5,
        0.5 + 0.5 * (1 - border - 0.5)),
      new = TRUE,
      family = object@theme_text_family,
      font = object@theme_text_font,
      col.lab = object@theme_text_font_colour,
      col.axis = object@theme_text_font_colour
      )

    par(mar = c(0, 0, 0, 0))

    plot(
      -99,
      -99,
      xlim = c(min_sample, max_sample),
      ylim = c(0, histograms_max_density),
      xaxs = 'i',
      axes = FALSE,
      type = 'n'
    )

    # histogram label
    mtext(
      at = histograms_max_density,
      text = 'Burn-in histrogram',
      side = 2,
      cex = object@theme_text_cex_axis ,
      col = object@theme_text_col_axis,
      adj = 1
    )

    ## histogram background block
    rect(
      par('usr')[1],
      par('usr')[3],
      par('usr')[2],
      par('usr')[4],
      col = colour_outer_burn_segment,
      border = FALSE
    )

    ## draw histogram over top
    hist(
      chains[1:n_burnin],
      axes = FALSE,
      xlim = c(min_sample, max_sample),
      ylim = c(0, histograms_max_density),
      breaks = histogram_breaks,
      main = '',
      col = 'white',
      border = colour_outer_burn_segment,
      prob = TRUE,
      add = TRUE,
      xaxs = 'i'
    )

    ## add histogram xes
    axis(
      4,
      cex.axis = 0.5,
      las = 1,
      col.axis = colour_guide_lines,
      col = colour_guide_lines
      )

    axis(
      4,
      at = c(0, 1),
      col.axis = colour_guide_lines,
      col = colour_guide_lines,
      tck = 0
      )

    plot_histogram_lines <- function(histogram, colour = 'black') {
      # Plots lines on a histogram following histogram densities
      #
      # Args:
      #           parameter: histogram to be plotted onto
      #           colour: line colour argument
      #
      # Returns:  Nothing

      # check inputs
      assertive::assert_is_character(colour)
      if(!class(histogram) == 'histogram') {
        warning('Non histogram passed to plot_histogram_lines function')
      }

      # preallocate x and y sequence vectors
      x_sequence <- rep(-99, 2 * length(histogram$density))
      y_sequence <- x_sequence

      # calculate lengths
      seq_length <- length(x_sequence)
      breaks_length <- length(histogram$breaks)

      # vector alternating bins and densities
      x_sequence[seq(1, seq_length - 1, by = 2)] <-
        histogram$breaks[1:(breaks_length - 1)]

      x_sequence[seq(2, seq_length, by = 2)] <-
        histogram$breaks[2:breaks_length]

      y_sequence[seq(1, seq_length - 1, by = 2)] <-
        histogram$density

      y_sequence[seq(2, seq_length, by = 2)] <-
        histogram$density

      lines(x_sequence, y_sequence, col = colour)
    }

    ## add density lines for the burnin chains
    for (i in 1:n_chains) {
      histogram_3 <- hist(
        chains[1:n_burnin, i],
        breaks = histogram_breaks,
        plot = FALSE
      )
      plot_histogram_lines(histogram_3, alpha(colour = object@theme_palette[[i]],
                                              alpha = 0.7))
    }

    ## add remaining sample histogram
    par(fig = c(
      0.5 + (1/3) * (0.5 - border),
      1 - border,
      0.5 + 0.5 * (1 - border - 0.5),
      1 - border
      ),
      new = TRUE,
      family = object@theme_text_family,
      font = object@theme_text_font,
      col.lab = object@theme_text_font_colour,
      col.axis = object@theme_text_font_colour
    )

    par(mar = c(0, 0, 0, 0))

    plot(
      -99,
      -99,
      xlim = c(min_sample, max_sample),
      ylim = c(0, histograms_max_density),
      xaxs = 'i',
      axes = FALSE,
      type = 'n'
    )

    ## histogram background block
    rect(
      par('usr')[1],
      par('usr')[3],
      par('usr')[2],
      par('usr')[4],
      col = colour_remaining_segment,
      border = FALSE
    )

    # histogram label
    mtext(
      at = histograms_max_density,
      text = 'Sample histrogram',
      side = 2,
      cex = object@theme_text_cex_axis ,
      col = object@theme_text_col_axis,
      adj = 1
    )

    ## draw histogram over top
    hist(
      chains[n_burnin:n_samples],
      axes = FALSE,
      xlim = c(min_sample, max_sample),
      ylim = c(0, histograms_max_density),
      breaks = histogram_breaks,
      main = '',
      col = 'white',
      border = colour_remaining_segment,
      prob = TRUE,
      add = TRUE,
      xaxs = 'i'
    )

    ## add axis

    ### top axis
    axis(
      3,
      at = seq(floor(min_sample - 0.1 * max_sample),
               max_sample + 0.1 * max_sample,
               by = 1),
      labels = FALSE,
      cex.axis = 0.5,
      las = 1,
      col.axis = colour_guide_lines,
      col = colour_guide_lines,
      tck = -0.01
    )

    axis(
      3,
      cex.axis = 0.5,
      las = 1,
      col.axis = colour_guide_lines,
      col = colour_guide_lines
      )

    ### side axis
    axis(
      4,
      cex.axis = 0.5,
      las = 1,
      col.axis = colour_guide_lines,
      col = colour_guide_lines
      )

    axis(
      4,
      at = c(0, 1),
      col.axis = colour_guide_lines,
      col = colour_guide_lines,
      tck = 0
      )

    for (i in 1:n_chains) {
      histogram_4 <- hist(
        chains[n_burnin:n_samples, i],
        breaks = histogram_breaks,
        plot = FALSE)
      plot_histogram_lines(histogram_4, object@theme_palette[[i]])
    }

    # Save plot within the backfillz object
    this_plot <- grDevices::recordPlot()
    ID <- max(object@plot_history$ID + 1)
    saved_plot_items <- list(
      ID = ID,
      time = date(),
      type = 'Trace dial',
      parameters = parameter,
      plot = this_plot
    )

    if(save_plot) {
      # Append plot details to the backfillz object
      object@plot_store <<- append(
        object@plot_store,
        list(
          saved_plot_items
        )
      )

    }

    # Save plot values in backfillz object
    object@df_trace_dial <<- rbind(
      object@df_trace_dial,
      data.frame(
        parameter = parameter,
        sample_min = min_sample,
        sample_max = max_sample
      )
    )

    return(object)
  }

  parameters <- as.matrix(parameters)

  apply(X = parameters, FUN = create_single_plot, MARGIN = 1)

  ID <- max(object@plot_history$ID + 1)

  # Update log
  object@plot_history <- rbind(
    object@plot_history,
    data.frame(
      ID = ID,
      Date = date(),
      Event = 'spiral_stream',
      R_version = R.Version()$version.string,
      Saved = save_plot,
      stringsAsFactors = FALSE
    )
  )

  return(object)

}
