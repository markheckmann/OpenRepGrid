\name{biplotDraw}
\alias{biplotDraw}
\title{biplotDraw is the workhorse doing the drawing of a 2D biplot.}
\usage{
  biplotDraw(x, inner.positioning = TRUE,
    outer.positioning = TRUE, c.labels.inside = F,
    flipaxes = c(F, F), strokes.x = 0.1, strokes.y = 0.1,
    offsetting = TRUE, offset.labels = 0, offset.e = 1,
    axis.ext = 0.1, mai = c(0.2, 1.5, 0.2, 1.5),
    rect.margins = c(0.01, 0.01), srt = 45, cex.pos = 0.7,
    xpd = TRUE, c.lines = TRUE, col.c.lines = grey(0.9),
    zoom = 1, ...)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{inner.positioning}{Logical. Whether to calculate
  positions to minimize overplotting of elements and
  construct labels (default is\code{TRUE}). Note that the
  positioning may slow down the plotting.}

  \item{outer.positioning}{Logical. Whether to calculate
  positions to minimize overplotting of of construct labels
  on the outer borders (default is\code{TRUE}). Note that
  the positioning may slow down the plotting.}

  \item{c.labels.inside}{Logical. Whether to print
  construct labels next to the points.  Can be useful
  during inspection of the plot (default \code{FALSE}).}

  \item{flipaxes}{Logical vector of length two. Whether x
  and y axes are reversed (default is \code{c(F,F)}).}

  \item{strokes.x}{Length of outer strokes in x direction
  in NDC.}

  \item{strokes.y}{Length of outer strokes in y direction
  in NDC.}

  \item{offsetting}{Do offsetting? (TODO)}

  \item{offset.labels}{Offsetting parameter for labels
  (TODO).}

  \item{offset.e}{offsetting parameter for elements
  (TODO).}

  \item{axis.ext}{Axis extension factor (default is
  \code{.1}). A bigger value will zoom out the plot.}

  \item{mai}{Margins available for plotting the labels in
  inch (default is \code{c(.2, 1.5, .2, 1.5)}).}

  \item{rect.margins}{Vector of length two (default is
  \code{c(.07, .07)}). Two values specifying the additional
  horizontal and vertical margin around each label.}

  \item{srt}{Angle to rotate construct label text. Only
  used in case \code{offsetting=FALSE}.}

  \item{cex.pos}{Cex parameter used during positioning of
  labels if prompted. Does usually not have to be changed
  by user.}

  \item{xpd}{Logical (default is \code{TRUE}). Wether to
  extend text labels over figure region. Usually not needed
  by the user.}

  \item{c.lines}{Logical. Whether construct lines from the
  center of the biplot to the sourrounding box are drawn
  (default is \code{FALSE}).}

  \item{col.c.lines}{The color of the construct lines from
  the center to the borders of the plot (default is
  \code{gray(.9)}).}

  \item{zoom}{Scaling factor for all vectors. Can be used
  to zoom the plot in and out (default \code{1}).}

  \item{...}{Not evaluated.}
}
\value{
  Invisible return of dataframe used during construction of
  plot (useful for developers).
}
\description{
  When the number of elements and constructs differs to a
  large extent, the absolute values of the coordinates for
  either constructs or elements will be much smaller or
  greater. This is an inherent property of the biplot. In
  the case it is not necessary to be able to read off the
  original data entries from the plot, the axes for
  elements and constructs can be scaled seperately. The
  proportional projection values will stay unaffetced. the
  absolue will change though. For grid interpretation the
  absolze values are usually oh no importance. Thus, there
  is an optional argument \code{normalize} which is
  \code{FALSE} as a default which rescales the axes so the
  longest construct and element vector will be set to the
  length of \code{1}.
}
\author{
  Mark Heckmann
}
\keyword{internal}

