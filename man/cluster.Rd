\name{cluster}
\alias{cluster}
\title{Cluster analysis (of constructs or elements).}
\usage{
  cluster(x, along = 0, dmethod = "euclidean",
    cmethod = "ward", p = 2, trim = NA, main = NULL,
    mar = c(4, 2, 3, 15), cex = 0, lab.cex = 0.8,
    cex.main = 0.9, print = TRUE, ...)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{along}{Along which dimension to cluster. 1 =
  constructs only, 2= elements only, 0=both (default).}

  \item{dmethod}{The distance measure to be used. This must
  be one of "euclidean", "maximum", "manhattan",
  "canberra", "binary" or "minkowski". Any unambiguous
  substring can be given.  For additional information on
  the different types type \code{?dist}.}

  \item{cmethod}{The agglomeration method to be used. This
  should be (an unambiguous abbreviation of) one of
  \code{"ward"}, \code{"single"}, \code{"complete"},
  \code{"average"}, \code{"mcquitty"}, \code{"median"} or
  \code{"centroid"}.}

  \item{p}{The power of the Minkowski distance, in case
  \code{"minkowski"} is used as argument for
  \code{dmethod}.}

  \item{trim}{the number of characters a construct is
  trimmed to (default is \code{10}). If \code{NA} no
  trimming is done. Trimming simply saves space when
  displaying the output.}

  \item{main}{Title of plot. The default is a name
  indicating the distance function and cluster method.}

  \item{mar}{Define the plot region (bottom, left, upper,
  right).}

  \item{cex}{Size parameter for the nodes. Usually not
  needed.}

  \item{lab.cex}{Size parameter for the constructs on the
  right side.}

  \item{cex.main}{Size parameter for the plot title
  (default is \code{.9}).}

  \item{print}{Logical. Wether to print the dendrogram
  (default is \code{TRUE}).}

  \item{...}{Additional parameters to be passed to plotting
  function from \code{as.dendrogram}. Type
  \code{?as.dendrogram} for further information. This
  option is usually not needed, except if special designs
  are needed.}
}
\value{
  Reordered \code{repgrid} object.
}
\description{
  \code{cluster} is a preliminary implementation of a
  cluster function. It supports various distance measures
  as well as cluster methods. More is to come.
}
\examples{
\dontrun{

  cluster(bell2010)
  cluster(bell2010, main="My cluster analysis")   # new title
  cluster(bell2010, type="t")                     # different drawing style
  cluster(bell2010, dmethod="manhattan")          # using manhattan metric
  cluster(bell2010, cmethod="single")             # do single linkage clustering
  cluster(bell2010, cex=1, lab.cex=1)             # change appearance
  cluster(bell2010, lab.cex=.7,                   # advanced appearance changes
          edgePar = list(lty=1:2, col=2:1))
}
}
\author{
  Mark Heckmann
}
\seealso{
  \code{\link{bertinCluster}}
}

