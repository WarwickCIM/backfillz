#' Creates a slice histogram of MCMC chains
#'
#' @param object            Backfillz or Stanfit object. Stanfit objects are converted to Backfillz object using the as_backfillz function.
#' @param slices            Dataframe giving the cut off points for the trace histograms and parameters to show.
#' @param save_plot         Set to TRUE to save plots in the Backfillz object.
plot_slice_histogram <- function(object = NULL, slices = NULL, save_plot = FALSE) {

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
  object@df_slice_histogram <- data.frame(
    parameter = character(),
    sample_min = numeric(),
    sample_max= numeric(),
    stringsAsFactors = FALSE
  )

  # Check slices argument
  if(is.null(slices)){ # if no argument for slices
    message('Using default slices of 0 - 0.4, 0.8 - 1.')
    message('Plotting the first two parameters only.')
    message('To plot other parameters please pass a slice argument to plot_slice_histogram')
    parameters <- as.array(attributes(object@mcmc_samples)$dimnames$parameters)[1:2]
    n_parameters <- length(parameters)
    lower <- c(0, 0.8)
    upper <- c(0.4, 1)
    slices <- data.frame(
      parameters = character(),
      lower = numeric(),
      upper = numeric(),
      stringsAsFactors = TRUE
    )
    for (parameter in parameters) {
      slices <- rbind(
        slices,
        data.frame(
          parameters = rep(parameter, length(upper)),
          lower = lower,
          upper = upper,
          stringsAsFactors = TRUE
        )
      )
    }
  } else { # if the user has passed a slices  argument
    # Extract the parameters
    parameters <- as.array(unique(slices$parameters))
  }

  create_single_plot <- function(parameter){
    # Makes plot of samples for a single parameter
    #
    # Args:
    #           parameter: the parameter we want to plot as a string
    #           segments:  segments we want to show on right of the plot as a data frame column (list)
    #
    # Returns:
    #           updated backfillaz object
    #           MCMC trace segment plot for a single parameter to the plot device

    # Create a new device for the plot
    # plot.new appears to not work..
    # Running platform specific for the moment
    #switch(Sys.info()['sysname'],
    #       Darwin = quartz(),
    #       dev.new()
    #)

    #dev.new()

    # Extract sample parameters
    n <- dim(object@mcmc_samples[,,parameter])[1]
    n_chains <- dim(object@mcmc_samples[,,parameter])[2]
    max_sample <- max(object@mcmc_samples[,,parameter])
    min_sample <- min(object@mcmc_samples[,,parameter])

    # Check, order and tag the slice
    slices$order <- NaN
    slices$order[slices$parameters == parameter] <- 1:length(slices$order[slices$parameters == parameter])


    # MIDDLE: JOINING SEGMENTS--------------------------------------
    par(fig=c(0.08+1/3, 2/3-0.08, 0.25, 0.85),
        family = object@theme_text_family,
        font = object@theme_text_font,
        bg = object@theme_bg_colour,
        fg = object@theme_fg_colour,
        col.lab = object@theme_text_font_colour,
        col.axis = object@theme_text_font_colour,
        cex.axis = object@theme_text_cex_axis,
        cex.main = object@theme_text_cex_title,
        cex.lab = object@theme_text_cex_main,
        bty = 'n')

    par(mar = c(0,0,0,0))

    plot(
      0:1, 0:1, type = 'n', yaxs = 'i', axes = FALSE, xaxs = 'i', ann=F
    )

    # background rectangle - colour to match the rects in the Left Hand Plot
    rect(0, 0, 1, 1, border = FALSE,
         col = adjustcolor(object@theme_mg_colour,
                           alpha.f = object@theme_alpha)
    )


    create_slice <- function(x, y){
      polygon (x = c(0, 1, 1, 0),
               y = c(x[1],
                     (x[3]-1)/max(slices$order[slices$parameters==parameter]),
                     x[3]/max(slices$order[slices$parameters==parameter]),
                     x[2]),
               col= object@theme_bg_colour,
               border=NA
      )
      lines(x = c(0, 1), y = c(x[1], (x[3]-1)/max(slices$order[slices$parameters==parameter])),
            lty = 1, col=object@theme_fg_colour)
      lines(x = c(0, 1), y = c(x[2], x[3]/max(slices$order[slices$parameters==parameter])),
            lty = 1, col=object@theme_fg_colour)

    }

    apply(
      X = slices[slices$parameter == parameter, c('lower', 'upper', 'order')],
      FUN = create_slice,
      MARGIN = 1
    )



    # LEFT: TRACE PLOT ------------------------------------------
    par(fig = c(0.08, 0.08 + 1/3, 0.25, 0.85), new = TRUE)
    par(mar = c(0, 0, 0, 0))

    # Create empty plot
    plot(1,
         type="n",
         xlim=c(min_sample, max_sample),
         ylim=c(0, n),
         axes = FALSE,
         xaxs = 'i',
         yaxs = 'i'
    )

    # add background rectangle
    rect( min_sample, 0,
          max_sample, 0,
          col= object@theme_bg_colour,
          border=F )


    # Fill plot with data
    line_plot <- function(x){
      lines(x[-1],
            1:n,
            col = alpha(object@theme_palette[[x[1]]], 1),
      )
    }

    # Plot every chain
    apply(X = rbind(1:n_chains, object@mcmc_samples[,,parameter]),
          FUN = line_plot, MARGIN = 2)

    axis(1, las=2 )

    # Add y axis for slices - values only on Right Hand Side
    axis(
      4,
      at = (as.numeric(unlist(slices[slices$parameters == parameter, c('lower', 'upper')])) * n),
      line = FALSE,
      col = NA,
      las=2
    )

    # Add whole y axis on Left Hand Side
    axis(
      2,
      line = FALSE,
      col = object@theme_fg_colour,
      col.ticks = object@theme_fg_colour,
      las=2
    )

    ## Place transparent rectangles over non slice data

    # bottom to first slice and last slice to top
    rect(
      min_sample,
      0,
      max_sample +100,
      min(slices$lower[slices$parameters == parameter]) * n,
      col = adjustcolor(object@theme_mg_colour, alpha.f = object@theme_alpha),
      border = NA
    )

    rect(
      min_sample-(max_sample-min_sample),
      0,
      min_sample,
      min(slices$lower[slices$parameters == parameter]) * n,
      col = adjustcolor(object@theme_bg_colour, alpha.f = 0.5),
      border = F, xpd=NA
    )

    if (max(slices$upper[slices$parameters == parameter]) < 1){
      rect(
        min_sample,
        max(slices$upper[slices$parameters == parameter]) * n,
        max_sample +100,
        1 * n,
        col = adjustcolor(object@theme_mg_colour, alpha.f = object@theme_alpha),
        border = NA
      )
      rect(
        min_sample-(max_sample-min_sample),
        max(slices$upper[slices$parameters == parameter]) * n,
        min_sample,
        1*n,
        col = adjustcolor(object@theme_bg_colour, alpha.f = 0.5),
        border =F, xpd=NA
      )
    }

    # find inbetween slices and add
    x <- matrix(unlist(slices[slices$parameters == parameter, c('lower', 'upper')])[-c(1,sum(slices$parameters == parameter)*2)], ncol=2)
    x <- x[x[,1] != x[,2],]

    create_inbetween_rectangle <- function(x){
      rect(
        min_sample,
        x[2] * n,
        max_sample +100,
        x[1] * n,
        col = adjustcolor(object@theme_mg_colour, alpha.f = object@theme_alpha),
        border = NA
      )
      rect(
        min_sample-(max_sample-min_sample),
        x[2] * n,
        min_sample,
        x[1]*n,
        col = adjustcolor(object@theme_bg_colour, alpha.f = 0.5),
        border = F, xpd=NA
      )
    }

    if (is.null(dim(x))){ # if there is only one inbetween slice
      create_inbetween_rectangle(as.array(x))
    } else { # if there are more than one inbetween slice
      apply(X = x, MARGIN = 1, FUN = create_inbetween_rectangle)
    }

    # add lines for slices

    segments(
      min_sample,
      slices$lower[slices$parameters == parameter] * n,
      max_sample,
      slices$lower[slices$parameters == parameter] * n,
      col = object@theme_fg_colour,
      lty=1
    )
    segments(
      min_sample,
      slices$upper[slices$parameters == parameter] * n,
      max_sample,
      slices$upper[slices$parameters == parameter] * n,
      col = object@theme_fg_colour,
      lty=1
    )
    segments(
      min_sample,
      slices$lower[slices$parameters == parameter] * n,
      min_sample,
      slices$upper[slices$parameters == parameter] * n,
      col = object@theme_fg_colour,
      lty=1
    )
    segments(
      max_sample,
      slices$lower[slices$parameters == parameter] * n,
      max_sample,
      slices$upper[slices$parameters == parameter] * n,
      col = object@theme_fg_colour,
      lty=1
    )


    box( col="red" ) #object@theme_fg_colour )


    # RIGHT: SLICE HISTOGRAM AND SAMPLE DENSITY ----------------------
    create_slice_histogram <- function(x){
      x <- slices[x,]

      par(mar = c(0, 0, 0, 3))
      par(
        fig = c(2/3-0.08,
                1,
                0.25 + 0.6*(x$order-1)/max(slices$order[slices$parameters==parameter]),
                0.25 + 0.6*x$order/max(slices$order[slices$parameters==parameter])
        ),
        new = TRUE
      )

      hist_obj <- hist(object@mcmc_samples[(x$lower*n):(x$upper*n),,parameter],
                       breaks=c(seq(floor(min_sample), ceiling(max_sample), length = 40)),
                       plot=F
      )

      hist(object@mcmc_samples[(x$lower*n):(x$upper*n),,parameter],
           axes = FALSE,
           ann = FALSE,
           xlim=c(min_sample, max_sample),
           ylim=c(0, 1.2*max(hist_obj$density)),
           breaks=c(seq(floor(min_sample), ceiling(max_sample), length = 40)),
           main = "",
           prob = TRUE,
           family = object@theme_text_family,
           font = object@theme_text_font,
           col=object@theme_bg_colour,
           border=object@theme_fg_colour,
           yaxs='i'
      )

      axis(4, las=2 )
      axis(1, line=NA, labels=F, tck=-0.03  )
      rect( par("usr")[1], par("usr")[3],
            par("usr")[2], par("usr")[4],
            col=NA,
            border=object@theme_fg_colour
      )

      if(x$order == 1){
        axis(1, las=2 )
      }

      if(x$order == length(slices$order[slices$parameters == parameter]) ){
        axis(3, las=2 )
      }

      # Draw density plot lines for each chain
      create_density_line <- function(x){
        lines(
          density(x[-1]),
          col = alpha(object@theme_palette[[x[1]]], 0.5),
          lwd=2.5
        )
      }
      apply(X = rbind(1:n_chains, object@mcmc_samples[,,parameter]),
            FUN = create_density_line,
            MARGIN = 2)

    }

    apply(as.array(which(slices$parameters == parameter)),
          FUN = create_slice_histogram,
          MARGIN = 1)


    # Save plot values in backfillz object
    object@df_slice_histogram <<- rbind(
      object@df_slice_histogram,
      data.frame(
        parameter = parameter,
        sample_min = min_sample,
        sample_max = max_sample
      )
    )


    # BOTTOM: Trace plot with Raftery Lewis indicator bar  ----------------------

    create_raftery_lewis_lines <- function(x){
      #x <- slices[x,]

      # create vector for chain plot heights
      chain_part <- seq( 0, 0.1, l=(n_chains+1) )

      par(mar = c(0, 0, 0, 0))
      par(
        fig = c(0.08,
                1/3+0.08,
                0.05 + chain_part[ x[1] ],
                0.05 + chain_part[ x[1]+1 ]
        ),
        new = TRUE
      )

      # get rafetery_lewis chain length estimate
      RD <- raftery.diag( object@mcmc_samples[,x[1],parameter] )

      # Create empty plot and scale to n or Raftery diagnostic suggested chain length
      plot(1:n,
           x[-1],
           type="n",
           ylim=c(min_sample, max_sample),
           xlim=c(0, max( n, as.numeric(RD$resmatrix[2]) ) ),
           axes = F,
           xaxs = 'i',
           yaxs = 'i',
           col = alpha(object@theme_palette[[x[1]]], 1)
      )

      # draw rect to Raftery diagnostic suggested chain length
      rect(0, min_sample, as.numeric(RD$resmatrix[2]), max_sample,
           col=adjustcolor(object@theme_palette[[ x[1] ]], alpha.f = 0.4),
           #col="red",
           border=F
      )

      # add chain as lines
      lines(1:n,
            x[-1],
            col = alpha(object@theme_palette[[x[1]]], 1)
      )

      # add warning 'x' if chain needs longer run
      if( as.numeric(RD$resmatrix[2]) > n ) mtext(4,
                                                  text="X",
                                                  las=2,
                                                  cex=object@theme_text_cex_main
      )

    }

    apply(X = rbind(1:n_chains, object@mcmc_samples[,,parameter]),
          FUN = create_raftery_lewis_lines,
          MARGIN = 2)

    # Overlay: plot to add titles  ----------------------

    par(mar = c(0, 0, 0, 0))
    par(fig = c(0, 1, 0, 1), new = TRUE )

    plot(-99,-99, type='n', axes=F, ann=F, xlim=c(0,1), ylim=c(0,1))

    # title
    text( 0.05, 0.99,
          labels= paste('Trace slice histogram of ', parameter, sep = ''),
          cex= object@theme_text_cex_title ,
          col= object@theme_text_col_title,
          adj=0
    )

    # Trace Plot Label
    text( 0.05, 0.9,
          labels= "Trace Plot with Slices",
          cex= object@theme_text_cex_axis ,
          col= object@theme_text_col_axis,
          adj=0
    )

    # Histogram / Density Plot Label
    text( 2/3-0.08, 0.15,
          labels= "Density Plots for Slices",
          cex= object@theme_text_cex_axis ,
          col= object@theme_text_col_axis,
          adj=0
    )

    # Raftery Lewis Label
    text( 0.05, 0,
          labels= "Raftery-Lewis Diagnostic",
          cex= object@theme_text_cex_axis ,
          col= object@theme_text_col_axis,
          adj=0
    )

    # CIM
    text( 1.02, -0.001,
          labels= "BackFillz.R by CIM, University of Warwick",
          cex= object@theme_text_cex_axis ,
          col= object@theme_text_col_axis,
          adj=1
    )

    # Save plot within the backfillz object
    this_plot <- grDevices::recordPlot()
    ID <- max(object@plot_history$ID + 1)
    saved_plot_items <- list(
      ID = ID,
      parameters = parameter,
      time = date(),
      type = 'Slice histgram',
      plot = this_plot
    )

    if(save_plot){
      # Append plot details to the backfillz object
      object@plot_store <<- append(
        object@plot_store,
        list(
          saved_plot_items
        )
      )
    }

    return(object)

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
      Event = 'Slice Historgram',
      R_version = R.Version()$version.string,
      Saved = save_plot,
      stringsAsFactors = FALSE
    )
  )

  return(object)
}
