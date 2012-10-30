\name{biplotSlater2d}
\alias{biplotSlater2d}
\title{Draws Slater's INGRID biplot in 2D.}
\usage{
  biplotSlater2d(x, center = 1, g = 1, h = 1, ...)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{center}{Numeric. The type of centering to be
  performed.  0= no centering, 1= row mean centering
  (construct), 2= column mean centering (elements), 3=
  double-centering (construct and element means), 4=
  midpoint centering of rows (constructs).  Slater's biplot
  uses \code{1} (row centering).}

  \item{g}{Power of the singular value matrix assigned to
  the left singular vectors, i.e. the constructs.}

  \item{h}{Power of the singular value matrix assigned to
  the right singular vectors, i.e. the elements.}

  \item{...}{Additional parameters for be passed to
  \code{\link{biplot2d}}.}
}
\description{
  The default is to use row centering and no normalization.
  Note that Slater's biplot is just a special case of a
  biplot that can be produced using the
  \code{\link{biplot2d}} function with the arguments
  \code{center=1, g=1, h=1}. The arguments that can be used
  in this function are the same as in
  \code{\link{biplot2d}}. Here, only the arguments that are
  set for Slater's biplot are described. To see all the
  parameters that can be changed see
  \code{\link{biplot2d}}.
}
\examples{
\dontrun{
   # See examples in \\code{\\link{biplot2d}} as the same arguments
   # can used for this function.
}
}
\author{
  Mark Heckmann
}
\seealso{
  Unsophisticated biplot: \code{\link{biplotSimple}}; \cr
  2D biplots: \code{\link{biplot2d}},
  \code{\link{biplotEsa2d}},
  \code{\link{biplotSlater2d}};\cr Pseudo 3D biplots:
  \code{\link{biplotPseudo3d}},
  \code{\link{biplotEsaPseudo3d}},
  \code{\link{biplotSlaterPseudo3d}};\cr Interactive 3D
  biplots: \code{\link{biplot3d}},
  \code{\link{biplotEsa3d}},
  \code{\link{biplotSlater3d}};\cr Function to set view in
  3D: \code{\link{home}}.
}

