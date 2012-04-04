\name{biplotEsa3d}
\alias{biplotEsa3d}
\title{Draw the eigenstructure analysis (ESA) biplot in rgl (3D device).}
\usage{
  biplotEsa3d(x, center = 1, g = 1, h = 1, ...)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{center}{Numeric. The type of centering to be
  performed.  0= no centering, 1= row mean centering
  (construct), 2= column mean centering (elements), 3=
  double-centering (construct and element means), 4=
  midpoint centering of rows (constructs).  Default is
  \code{4} (scale midpoint centering).}

  \item{g}{Power of the singular value matrix assigned to
  the left singular vectors, i.e. the constructs.}

  \item{h}{Power of the singular value matrix assigned to
  the right singular vectors, i.e. the elements.}

  \item{...}{Additional arguments to be passed to
  \code{\link{biplot3d}}.}
}
\description{
  The 3D biplot opens an interactive 3D device that can be
  rotated and zoomed using the mouse. A 3D device
  facilitates the exploration of grid data as significant
  proportions of the sum-of-squares are often represented
  beyond the first two dimensions. Also, in a lot of cases
  it may be of interest to explore the grid space from a
  certain angle, e.g. to gain an optimal view onto the set
  of elements under investigation (e.g. Raeithel, 1998).
  Note that the eigenstructure analysisis just a special
  case of a biplot that can also be produced using the
  \code{\link{biplot3d}} function with the arguments
  \code{center=4, g=1, h=1}.
}
\examples{
\dontrun{

   biplotEsa3d(boeker)
   biplotEsa3d(boeker, unity3d=T)

   biplotEsa3d(boeker, e.sphere.col="red",
               c.text.col="blue")
   biplotEsa3d(boeker, e.cex=1)
   biplotEsa3d(boeker, col.sphere="red")

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

