# Code from here: 
# https://r-graph-gallery.com/130-ring-or-donut-chart.html
# Original written by Markus Gesmann, who is in Ireland and it seems
# that UK vs US compilers must be different since I had to make several 
# syntax edits before the function would work.
# Then I edited the function to order the data (and labels, correspondingly) 
# largest to smallest no what order it was given; for Shiny application.
# Editted by MLM Feb 2023
#
#
# Let's use the function, it works like PiePlot!
# inner.radius controls the width of the ring!
# doughnut( x=c(15,20,60,5), inner.radius=0.5 )

#
# The doughnut function permits to draw a donut plot
doughnut <-
  function (x, # labels = names(x),
            labels = c("% EPT","% Non-Insect","% Midges","% Other Insects"),
            edges = 200, outer.radius = 0.8,
            inner.radius=0.6, clockwise = TRUE,
            init.angle = 180, # 90, 
            # init.angle = if (clockwise) 90 else 0,
            col = c(4,"dodgerblue",3,"gold"),
            density = NULL,
            angle = 45, border = FALSE, lty = NULL,
            main = "Macroinvertbrate breakdown by group", ...) {
    if (!is.numeric(x) || any(is.na(x) | x < 0))
      stop("'x' values must be positive.")
    if (is.null(labels)){
      labels <- as.character(seq_along(x)) 
    } else {
      labels <- as.graphicsAnnot(paste0(x[order(x,decreasing=T)],labels[order(x,decreasing=T)]))
    }
    x <- x[order(x,decreasing=T)]
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L]) {
      xlim <- (pin[1L]/pin[2L]) * xlim
    } else ylim <- (pin[2L]/pin[1L]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col)) {
      col <- if (is.null(density)) {
        palette() } else par("fg")
    }
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise) {-2 * pi} else 2 * pi
    t2xy <- function(t, radius) {
      t2p <- twopi * t + init.angle * pi/180
      list(x = radius * cos(t2p),
           y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
      n <- max(2, floor(edges * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                outer.radius)
      polygon(c(P$x, 0), c(P$y, 0), density = density[i],
              angle = angle[i], border = border[i],
              col = col[i], lty = lty[i])
      Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
      lab <- as.character(labels[i])
      if (!is.na(lab) && nzchar(lab)) {
        lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
        text(1.1 * Pout$x, 1.1 * Pout$y, labels[i],
             xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0))
      }
      ## Add white disc          
      Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                  inner.radius)
      polygon(Pin$x, Pin$y, density = density[i],
              angle = angle[i], border = border[i],
              col = "white", lty = lty[i])
    }
    title(main = main)
    # invisible(NULL)
  }


