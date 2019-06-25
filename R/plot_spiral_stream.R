#' Creates a spiral steamgrapgh of MCMC chains
#'
#' @param object            Backfillz or Stanfit object. Stanfit objects are converted to Backfillz object using the as_backfillz function.
#' @param start_sample      Chain position to start the spirals.
#' @param steps             Vector of step increments for each spiral within chain. Defaults to 25 percent of sample for 4 steps.
#' @param parameters        Vector of parameters to plot. Defaults to two parameters.
#' @param laps              Number of laps for the spitals. Defaults to 4.
#' @param save_plot         Set to TRUE to save plots in the Backfillz object.
plot_spiral_stream <- function(object = NULL, start_sample = NULL, steps = NULL, parameters = NULL, laps = 4, save_plot = FALSE){

  assertive::assert_is_logical(save_plot)

  # check inputs
  if(!class(object) == 'stanfit' & !class(object) == 'Backfillz' & !class(object) == 'data.frame'){
    stop('Object is not a stanfit, Backfillz or data frame object')
  }

  # convert stanfit
  if((class(object) == 'stanfit') | (class(object) == 'data.frame')) {
    object <- as_backfillz(object)
  }

  # Preallocate the data frame stored in the backfillz object
  object@df_spiral_stream <- data.frame(
    parameter = character(),
    sample_min = numeric(),
    sample_max= numeric(),
    stringsAsFactors = FALSE
  )

  # default step if not provided
  if(is.null(steps)) {
    #message('Steps argument not provided. Defaulting to 25%, 50%, 75% and 100% steps.')
    steps <- c(3, 8, 15)
  }

  # assign default of first 2 parameters if parameters are not specified
  if(is.null(parameters)) {
    message('No parameters specified. Plotting the first two model parameters.')
    parameters <- as.array(attributes(object@mcmc_samples)$dimnames$parameters)[1:2]
  }

  # Spiral plot helper function
  calculate_equi_spiral_points <- function(n_points, laps, inner, otr) {
    this_range <- ((1 + otr) * n_points) ^ 0.5
    lap_width <- this_range / laps

    # start point
    x_start <- this_range
    y_start <- 0

    # Clarkson equation - best equation, in the world.
    spiral_length <- pi * lap_width * laps ^ 2 - pi * lap_width * inner ^ 2

    arc_length <- spiral_length / n_points

  }

  equi.spi <- function( Npnts, laps=5, innr=1, otr=0.05 ){

    rng <- ((1+otr)*Npnts)^0.5
    lapWdth <- rng/laps

    # set start point
    y0 <- strtY <- rng
    x0 <- strtX <- 0

    # calculate length and take away inner spiral (clarkson equation)
    lspi <-  pi*lapWdth*laps^2 - pi*lapWdth*innr^2

    #calculate spiral arc length for the 1st step
    alngth <- (lspi/Npnts)

    #assume it approximates a circle and calculate chord length
    thet <- alngth/rng
    dst <-  rng*sin(thet)

    deg1 <- 0.5*pi # start angle ( 12 o'clock )

    x2 <- rep(-99, Npnts)
    y2 <- rep(-99, Npnts)
    hs <- rep(-99, Npnts)
    deg <- rep(-99, Npnts)
    degsum <- rep(-99, Npnts)
    i2 <- 0

    upVal <- round( (1+otr)*Npnts, digits=0)
    loVal <- upVal-Npnts+1

    for( i in upVal:loVal ){

      h <- i^0.5
      deg0 <- atan( dst/ h )
      deg1 <- deg1 - deg0
      x1 <- h*cos( deg1 );  y1 <- h*sin( deg1 )

      i2 <- i2+1
      x2[i2] <- x1; y2[i2] <- y1
      x0 <- x1;  y0 <- y1

      hs[i2] <- h
      deg[i2] <- deg0
      degsum[i2] <- deg1

    }

    x3 <- x2/rng
    y3 <- y2/rng
    hs <- hs/rng

    sPnts <- cbind(x3, y3, hs, deg, degsum  )
    sPnts <- as.data.frame( sPnts  )
    names(sPnts) <- c( "spX", "spY", "h", "deg", "degsum" )
    sPnts

  }

  plot_spiral_row <- function(this_chain, steps, chain_number, lap_width, n, inner) {
    # Plots a row of spirals for a single chain
    # Each spiral corresponds to a step value
    # Args:
    #           this_chain: samples comprising a single chain
    #           steps: the step incriments for each spiral
    #
    # Returns:
    #           Plots a row of spirals

    # scaling function
    scale_0_1 <- function(x) { (x - min(x)) / max(x) }
    this_chain_0_1 <- scale_0_1(this_chain)
    this_chain_spiral <- lap_width * this_chain_0_1

    # Spiral parameters
    # border coordinates
    lines <- seq(0, 2*pi, l = 1000)
    y_lines <- sin(lines)
    x_lines <- cos(lines)

    for (spiral_number in 1:length(steps)) {
      # Calcaulate parameters of and plot each spiral
      chain_var <- stats::var(this_chain)
      spiral_points <- rep(-99, n)
      step <- steps[spiral_number]

      # Unclear what this is. Ask Greg. Variables names are unclear. Needs changing!
      for (i in 1:n) {

        if (i >= step) {
          klw <- i - step
        } else {
          klw <- 1
        }

        if (i <= (n - step)) {
          khg <- i + step
        } else {
          khg <- n
        }

        spiral_points[i] <- stats::var(this_chain[klw:khg])

      }

      vrsa <- as.data.frame(spiral_points)
      names(vrsa) <- c("vl")
      vrsa <- as.data.frame( vrsa )

      vrsb <- vrsa - min(vrsa, na.rm = F)
      vrsb <- vrsb/max(vrsb, na.rm = F)
      vrsb <- lap_width*vrsb

      es <- equi.spi( Npnts = n, laps = laps, innr=inner)
      spi <- es$degsum
      Yspi <- sin(spi); Xspi <- cos(spi)

      # create spiral by multiplying by 1
      Dspi5 <- es$h + vrsb$vl
      Dspi6 <- es$h - vrsb$vl

      Xral5 <- Xspi*Dspi5; Yral5 <- Yspi*Dspi5
      Xral6 <- Xspi*Dspi6; Yral6 <- Yspi*Dspi6

      X0line <- Xspi*es$h; Y0line <- Yspi*es$h

      # Create plot space
      plot(
        x_lines,
        y_lines,
        lwd = 0.1,
        type = 'n',
        xlim = c(-1-max(vrsb),1+max(vrsb)),
        ylim = c(-1-max(vrsb),1+max(vrsb)),
        axes = FALSE,
        ann = FALSE,
        xaxs = "i",
        yaxs = "i"
      )

      # Plot spiral
      polygon(
        c(Xral5, rev(Xral6)),
        c(Yral5, rev(Yral6)),
        col = object@theme_palette[[chain_number]],
        border=FALSE
      )
    }
  }

  create_single_plot <- function(parameter) {
    # Plots a row of spirals. Each spiral is a chain at a corresponding increment.
    # E.g., 3 chains at 4 increments results in 12 spirals
    # Args:
    #           parameter: the parameter we want to plot as a string
    #           steps: the step incriments for each spiral
    #
    # Returns:
    #           updated backfillaz object
    #           Plots a row of spirals for a given parameter on the current plot device


    # Create a new device for the plot
    # plot.new appears to not work..
    # Running platform specific for the moment
    # switch(Sys.info()['sysname'],
    #        Darwin = quartz(),
    #        dev.new()
    # )
    #dev.new()


    # Extract sample parameters
    n <- dim(object@mcmc_samples[,,parameter])[1]
    n_chains <- dim(object@mcmc_samples[,,parameter])[2]
    max_sample <- max(object@mcmc_samples[,,parameter])
    min_sample <- min(object@mcmc_samples[,,parameter])

    # setup plot device
    par(mfrow = c(n_chains, length(steps)))
    par(mar = c(0, 0, 0, 0))
    par(oma = c(3, 3, 3, 3))

    par(family = object@theme_text_family,
        font = object@theme_text_font,
        bg = object@theme_bg_colour,
        col.lab = object@theme_text_font_colour,
        col.axis = object@theme_text_font_colour)

    inner <- 1
    lap_width <- 1 / (laps + inner)

    for (chain_number in 1:n_chains){
      this_chain <- object@mcmc_samples[, chain_number, parameter]
      plot_spiral_row(this_chain, steps = steps, chain_number, lap_width, n, inner)
    }

    # Outer labels
    mtext(parameter, 3, outer = TRUE, col = object@theme_text_font_colour, font = object@theme_text_font)
    mtext('Variance', 1, outer = TRUE, col = object@theme_text_font_colour, font = object@theme_text_font)
    mtext('Chain', 2, outer = TRUE, col = object@theme_text_font_colour, font = object@theme_text_font)

    # Save plot within the backfillz object
    this_plot <- grDevices::recordPlot()
    ID <- max(object@plot_history$ID + 1)
    saved_plot_items <- list(
      ID = ID,
      time = date(),
      type = 'Spiral stream',
      parameters = parameter,
      plot = this_plot
    )

    if (save_plot) {
      # Append plot details to the backfillz object
      object@plot_store <<- append(
        object@plot_store,
        list(
          saved_plot_items
        )
      )
    }

    # Save plot values in backfillz object
    object@df_spiral_stream <<- rbind(
      object@df_spiral_stream,
      data.frame(
        parameter = 'spiral stream run',
        sample_min = 0,
        sample_max = 0,
        stringsAsFactors = FALSE
      )
    )

  }

  parameters <- as.matrix(parameters)

  # Create a plot for each parameter
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
