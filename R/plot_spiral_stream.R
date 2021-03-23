#' Creates a spiral steamgrapgh of MCMC chains
#'
#' @param object            Backfillz or Stanfit object. Stanfit objects
#'  are converted to Backfillz object using the as_backfillz function.
#' @param start_sample      Chain position to start the spirals.
#' @param steps             Vector of step increments for each spiral
#'  within chain. Defaults to 25 percent of sample for 4 steps.
#' @param parameters        Vector of parameters to plot. Defaults to
#'  two parameters.
#' @param laps              Number of laps for the spitals. Defaults to 4.
#' @param save_plot         Set to TRUE to save plots in the Backfillz object.
#' @param verbose           Set to TRUE to see function messages
plot_spiral_stream <- function(
  object = NULL,
  start_sample = NULL,
  steps = NULL,
  parameters = NULL,
  laps = 4,
  save_plot = FALSE,
  verbose = TRUE) {

  assertive::assert_is_logical(save_plot)

  # check inputs
  if (
    !class(object) == "stanfit" &
     !class(object) == "backfillz" &
      !class(object) == "data.frame") {
    stop("Object is not a stanfit, Backfillz or data frame object")
  }

  # convert stanfit
  if ((class(object) == "stanfit") | (class(object) == "data.frame")) {
    object <- as_backfillz(object, verbose)
  }

  # Preallocate the data frame stored in the backfillz object
  object@df_spiral_stream <- data.frame(
    parameter = character(),
    sample_min = numeric(),
    sample_max = numeric(),
    stringsAsFactors = FALSE
  )

  # default step if not provided
  if (is.null(steps)) {
    steps <- c(3, 8, 15)
  }

  # assign default of first 2 parameters if parameters are not specified
  if (is.null(parameters)) {
    if (verbose) {
      message(paste0("No parameters specified. ",
      "Plotting the first two model parameters."))
    }
    parameters <- as.array(
      attributes(object@mcmc_samples)$dimnames$parameters)[1:2]
  }

  equi_spi <- function(n_points, laps = 5, innr = 1, otr = 0.05) {

    rng <- ((1 + otr) * n_points)^0.5
    lap_width <- rng / laps

    # calculate length and take away inner spiral (clarkson equation)
    lspi <-  pi * lap_width * laps^2 - pi * lap_width * innr^2

    #calculate spiral arc length for the 1st step
    arc_length <- (lspi / n_points)

    #assume it approximates a circle and calculate chord length
    theta <- arc_length / rng
    distance <-  rng * sin(theta)

    deg1 <- 0.5 * pi # start angle ( 12 o"clock )

    x2 <- rep(-99, n_points)
    y2 <- rep(-99, n_points)
    hs <- rep(-99, n_points)
    deg <- rep(-99, n_points)
    degsum <- rep(-99, n_points)
    i2 <- 0

    upper_value <- round((1 + otr) * n_points, digits = 0)
    lower_value <- upper_value - n_points + 1

    for (i in upper_value:lower_value) {

      h <- i^0.5
      deg0 <- atan(distance / h)
      deg1 <- deg1 - deg0
      x1 <- h * cos(deg1)
      y1 <- h * sin(deg1)

      i2 <- i2 + 1
      x2[i2] <- x1
      y2[i2] <- y1

      hs[i2] <- h
      deg[i2] <- deg0
      degsum[i2] <- deg1

    }

    x3 <- x2 / rng
    y3 <- y2 / rng
    hs <- hs / rng

    spiral_points <- cbind(x3, y3, hs, deg, degsum)
    spiral_points <- as.data.frame(spiral_points)
    names(spiral_points) <- c("spX", "spY", "h", "deg", "degsum")
    spiral_points

  }

  plot_spiral_row <- function(
    this_chain, steps,
    chain_number,
    lap_width,
    n,
    inner) {
    # Plots a row of spirals for a single chain
    # Each spiral corresponds to a step value
    # Args:
    #           this_chain: samples comprising a single chain
    #           steps: the step incriments for each spiral
    #
    # Returns:
    #           Plots a row of spirals

    # scaling function
    scale_0_1 <- function(x) {
      (x - min(x)) / max(x)
    }

    # Spiral parameters
    # border coordinates
    lines <- seq(0, 2 * pi, l = 1000)
    y_lines <- sin(lines)
    x_lines <- cos(lines)

    for (spiral_number in seq_len(length(steps))) {
      # Calcaulate parameters of and plot each spiral
      spiral_points <- rep(-99, n)
      step <- steps[spiral_number]

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
      vrsa <- as.data.frame(vrsa)

      vrsb <- vrsa - min(vrsa, na.rm = FALSE)
      vrsb <- vrsb / max(vrsb, na.rm = FALSE)
      vrsb <- lap_width * vrsb

      es <- equi_spi(n_points = n, laps = laps, innr = inner)
      spi <- es$degsum
      y_spiral <- sin(spi)
      x_spiral <- cos(spi)

      # create spiral by multiplying by 1
      spiral_degree_1 <- es$h + vrsb$vl
      spiral_degree_2 <- es$h - vrsb$vl

      # create x and y spiral coordinates
      x_radius_length_1 <- x_spiral * spiral_degree_1
      y_radius_length_1 <- y_spiral * spiral_degree_1
      x_radius_length_2 <- x_spiral * spiral_degree_2
      y_radius_length_2 <- y_spiral * spiral_degree_2

      # Create plot space
      plot(
        x_lines,
        y_lines,
        lwd = 0.1,
        type = "n",
        xlim = c(-1 - max(vrsb), 1 + max(vrsb)),
        ylim = c(-1 - max(vrsb), 1 + max(vrsb)),
        axes = FALSE,
        ann = FALSE,
        xaxs = "i",
        yaxs = "i"
      )

      # Plot spiral
      polygon(
        c(x_radius_length_1, rev(x_radius_length_2)),
        c(y_radius_length_1, rev(y_radius_length_2)),
        col = object@theme_palette[[chain_number]],
        border = FALSE
      )
    }
  }

  create_single_plot <- function(parameter) {
    # Plots a row of spirals. Each spiral is a
    # chain at a corresponding increment.
    # E.g., 3 chains at 4 increments results in 12 spirals
    # Args:
    #           parameter: the parameter we want to plot as a string
    #           steps: the step incriments for each spiral
    #
    # Returns:
    #           updated backfillaz object
    #           Plots a row of spirals for a given parameter
    # on the current plot device

    # Extract sample parameters
    n <- dim(object@mcmc_samples[, , parameter])[1]
    n_chains <- dim(object@mcmc_samples[, , parameter])[2]

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

    for (chain_number in seq_len(n_chains)) {
      this_chain <- object@mcmc_samples[, chain_number, parameter]
      plot_spiral_row(
        this_chain, steps = steps,
        chain_number,
        lap_width,
        n,
        inner)
    }

    # Outer labels
    mtext(
      parameter,
      3,
      outer = TRUE,
      col = object@theme_text_font_colour,
      font = object@theme_text_font
    )

    mtext(
      "Variance", 1,
      outer = TRUE,
      col = object@theme_text_font_colour,
      font = object@theme_text_font
      )

    mtext(
      "Chain",
      2,
      outer = TRUE,
      col = object@theme_text_font_colour,
      font = object@theme_text_font
      )

    # Save plot within the backfillz object
    this_plot <- grDevices::recordPlot()
    id <- max(object@plot_history$ID + 1)
    saved_plot_items <- list(
      ID = id,
      time = date(),
      type = "Spiral stream",
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
        parameter = "spiral stream run",
        sample_min = 0,
        sample_max = 0,
        stringsAsFactors = FALSE
      )
    )

  }

  parameters <- as.matrix(parameters)

  # Create a plot for each parameter
  apply(X = parameters, FUN = create_single_plot, MARGIN = 1)

  id <- max(object@plot_history$ID + 1)

  # Update log
  object@plot_history <- rbind(
    object@plot_history,
    data.frame(
      ID = id,
      Date = date(),
      Event = "spiral_stream",
      R_version = R.Version()$version.string,
      Saved = save_plot,
      stringsAsFactors = FALSE
    )
  )

  return(object)
  }
