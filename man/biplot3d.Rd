\name{biplot3d}
\alias{biplot3d}
\title{Draw grid in rgl (3D device).}
\usage{
  biplot3d(x, dim = 1:3, labels.e = TRUE, labels.c = TRUE,
    lines.c = TRUE, lef = 1.3, center = 1, normalize = 0,
    g = 0, h = 1, col.active = NA, col.passive = NA,
    c.sphere.col = grey(0.4), c.cex = 0.6,
    c.text.col = grey(0.4), e.sphere.col = grey(0),
    e.cex = 0.6, e.text.col = grey(0), alpha.sphere = 0.05,
    col.sphere = "black", unity = FALSE, unity3d = FALSE,
    scale.e = 0.9, zoom = 1, ...)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{dim}{Dimensions to display.}

  \item{labels.e}{Logical. whether element labels are
  displayed.}

  \item{labels.c}{Logical. whether construct labels are
  displayed.}

  \item{lines.c}{Numeric. The way lines are drawn through
  the construct vectors.  \code{0 =} no lines, \code{1 =}
  lines from constructs to outer frame, \code{2 =} lines
  from the center to outer frame.}

  \item{lef}{Construct lines extension factor}

  \item{center}{Numeric. The type of centering to be
  performed.  0= no centering, 1= row mean centering
  (construct), 2= column mean centering (elements), 3=
  double-centering (construct and element means), 4=
  midpoint centering of rows (constructs).  Default is
  \code{1} (row centering).}

  \item{normalize}{A numeric value indicating along what
  direction (rows, columns) to normalize by standard
  deviations. \code{0 = none, 1= rows, 2 = columns}
  (default is \code{0}).}

  \item{g}{Power of the singular value matrix assigned to
  the left singular vectors, i.e. the constructs.}

  \item{h}{Power of the singular value matrix assigned to
  the right singular vectors, i.e. the elements.}

  \item{col.active}{Columns (elements) that are no
  supplementary points, i.e. they are used in the SVD to
  find principal components. default is to use all
  elements.}

  \item{col.passive}{Columns (elements) that are
  supplementary points, i.e. they are NOT used in the SVD
  but projecte into the component space afterwards. They do
  not determine the solution. Default is \code{NA}, i.e. no
  elements are set supplementary.}

  \item{c.sphere.col}{Color of construct spheres.}

  \item{c.cex}{Size of construct text.}

  \item{c.text.col}{Color for construct text.}

  \item{e.sphere.col}{Color of elements.}

  \item{e.cex}{Size of element labels.}

  \item{e.text.col}{Color of element labels.}

  \item{alpha.sphere}{Numeric. alpha blending of the
  sourrounding sphere (default\code{".05"}).}

  \item{col.sphere}{Color of sourrouding sphere
  (default\code{"black"}).}

  \item{unity}{Scale elements and constructs coordinates to
  unit scale (maximum of 1) so they are printed more neatly
  (default \code{TRUE}).}

  \item{unity3d}{To come.}

  \item{scale.e}{Scaling factor for element vectors. Will
  cause element points to move a bit more to the center
  (but only if \code{unity} or \code{unity3d} is
  \code{TRUE}).  This argument is for visual appeal only.}

  \item{zoom}{Not yet used. Scaling factor for all vectors.
  Can be used to zoom the plot in and out (default
  \code{1}).}

  \item{...}{Parameters to be passed on.}
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
}
\examples{
\dontrun{

   biplot3d(boeker)
   biplot3d(boeker, unity3d=T)

   biplot3d(boeker, e.sphere.col="red",
            c.text.col="blue")
   biplot3d(boeker, e.cex=1)
   biplot3d(boeker, col.sphere="red")

   biplot3d(boeker, g=1, h=1)    # INGRID biplot
   biplot3d(boeker, g=1, h=1,    # ESA biplot
            center=4)
}
}
\author{
  Mark Heckmann
}
\references{
  Raeithel, A. (1998). Kooperative Modellproduktion von
  Professionellen und Klienten - erlauetert am Beispiel des
  Repertory Grid. \emph{Selbstorganisation, Kooperation,
  Zeichenprozess: Arbeiten zu einer
  kulturwissenschaftlichen, anwendungsbezogenen
  Psychologie} (pp. 209-254). Opladen: Westdeutscher
  Verlag.
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

