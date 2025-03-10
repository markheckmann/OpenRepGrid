% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repgrid-plots.r
\name{prepareBiplotData}
\alias{prepareBiplotData}
\title{Prepare dataframe passed to drawing functions for biplots.}
\usage{
prepareBiplotData(
  x,
  dim = c(1, 2),
  map.dim = 3,
  e.label.cex = 0.8,
  c.label.cex = 0.6,
  e.label.col = "black",
  c.label.col = grey(0.8),
  c.label.col.left = NULL,
  c.label.col.right = NULL,
  e.point.cex = 0.7,
  c.point.cex = 0.8,
  e.point.col = "black",
  c.point.col = "darkred",
  e.color.map = c(0.4, 1),
  c.color.map = c(0.4, 1),
  c.points.devangle = 90,
  c.labels.devangle = 90,
  c.points.show = TRUE,
  c.labels.show = TRUE,
  e.points.show = TRUE,
  e.labels.show = TRUE,
  unity = TRUE,
  unity3d = FALSE,
  scale.e = 0.9,
  ...
)
}
\arguments{
\item{x}{\code{repgrid} object.}

\item{dim}{Dimensions to be used for biplot (default is \code{c(1,2)}).}

\item{map.dim}{Third dimension used to map aesthetic attributes (depth)
(default is \code{3}).}

\item{e.label.cex}{Size of the element labels. Two values can be entered that will
represents the lower and upper size of a range of cex the values of \code{map.dim}
are mapped onto. The default is \code{c(.4, .8)}. If only one cex value
is supplied (e.g. \code{.7}) no mapping occurs and all element labels
will have the same size irrespective of their value on the \code{map.dim}
dimension.}

\item{c.label.cex}{Size of the construct labels. Two values can be entered that will
represents the lower and upper size of a range of cex the values of \code{map.dim}
are mapped onto. The default is \code{c(.4, .8)}. If only one cex value
is supplied (e.g. \code{.7}) no mapping occurs and all construct labels
will have the same size irrespective of their value on the \code{map.dim}
dimension.}

\item{e.label.col}{Color(s) of the element labels. Two values can be entered that will
create a color ramp. The values of \code{map.dim} are mapped onto the ramp.
The default is \code{c("white", "black")}. If only one color color value
is supplied (e.g. \code{"black"}) no mapping occurs and all element labels
will have the same color irrespective of their value on the \code{map.dim}
dimension.}

\item{c.label.col}{Color(s) of the construct labels. Two values can be entered that will
create a color ramp. The values of \code{map.dim} are mapped onto the ramp.
The default is \code{c("white", "black")}. If only one color color value
is supplied (e.g. \code{"black"}) no mapping occurs and all construct labels
will have the same color irrespective of their value on the \code{map.dim}
dimension.}

\item{c.label.col.left, c.label.col.right}{Explicit color values for left and right construct poles.
\code{NULL} by default. Will overwrite \code{c.label.col}.}

\item{e.point.cex}{Size of the element symbols. Two values can be entered that will
represents the lower and upper size of a range of cex the values of \code{map.dim}
are mapped onto. The default is \code{c(.4, .8)}. If only one cex value
is supplied (e.g. \code{.7}) no mapping occurs and all elements
will have the same size irrespective of their value on the \code{map.dim}
dimension.}

\item{c.point.cex}{Size of the construct symbols. Two values can be entered that will
represents the lower and upper size of a range of cex the values of \code{map.dim}
are mapped onto. The default is \code{c(.4, .8)}. If only one cex value
is supplied (e.g. \code{.7}) no mapping occurs and all elements
will have the same size irrespective of their value on the \code{map.dim}
dimension.}

\item{e.point.col}{Color(s) of the element symbols. Two values can be entered that will
create a color ramp. The values of \code{map.dim} are mapped onto the ramp.
The default is \code{c("white", "black")}. If only one color color value
is supplied (e.g. \code{"black"}) no mapping occurs and all elements
will have the same color irrespective of their value on the \code{map.dim}
dimension.}

\item{c.point.col}{Color(s) of the construct symbols. Two values can be entered that will
create a color ramp. The values of \code{map.dim} are mapped onto the ramp.
The default is \code{c("white", "darkred")}. If only one color color value
is supplied (e.g. \code{"black"}) no mapping occurs and all elements
will have the same color irrespective of their value on the \code{map.dim}
dimension.}

\item{e.color.map}{Value range to determine what range of the color ramp defined in
\code{e.color} will be used for mapping the colors.
Default is \code{c(.4, ,1)}. Usually not important for the user.}

\item{c.color.map}{Value range to determine what range of the color ramp defined in
\code{c.color} will be used for mapping. Default is \code{c(.4, ,1)}.
Usually not important for the user.}

\item{unity}{Scale elements and constructs coordinates to unit scale in 2D (maximum of 1)
so they are printed more neatly (default \code{TRUE}).}

\item{unity3d}{Scale elements and constructs coordinates to unit scale in 3D (maximum of 1)
so they are printed more neatly (default \code{TRUE}).}

\item{scale.e}{Scaling factor for element vectors. Will cause element points to move a bit more
to the center. This argument is for visual appeal only.}

\item{...}{Not evaluated.}

\item{devangle}{The deviation angle from the x-y plane in degrees. These can only be calculated
if a third dimension \code{map.dim} is specified. Only the constructs
vectors that do not depart
more than the specified degrees from the shown x-y plane will be printed.
This facilitates the visual interpretation, as only vectors represented in
the current plane are shown. Set the value to \code{91} (default)
to show all vectors.}
}
\value{
\code{dataframe} containing the variables \verb{type, show, x, y, z, labels, color, cex}. Usually not of interest to the user.
}
\description{
Data frame contains the variables \verb{type, show, x, y, z, labels, color, cex}.
}
\note{
\if{html}{\out{<div class="sourceCode">}}\preformatted{           TODO:  How to omit `map.dim`?
}\if{html}{\out{</div>}}
}
\keyword{internal}
