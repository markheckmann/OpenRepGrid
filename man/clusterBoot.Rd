\name{clusterBoot}
\alias{clusterBoot}
\title{Multiscale bootstrap cluster analysis.}
\usage{
  clusterBoot(x, along = 1, align = TRUE,
    dmethod = "euclidean", cmethod = "ward", p = 2,
    nboot = 1000, r = seq(0.8, 1.4, by = 0.1), seed = NULL,
    ...)
}
\arguments{
  \item{x}{\code{grid object}}

  \item{align}{Whether the constructs should be aligned
  before clustering (default is \code{TRUE}). If not, the
  matrix is clustered as is. Aligning will reverse
  constructs to make them more similar to another. See
  Details section for mor information.}

  \item{along}{Along which dimension to cluster. 1 =
  constructs, 2= elements.}

  \item{p}{Power of the Minkowski metric. Not yet passed on
  to pvclust!}

  \item{seed}{Random seed for bootstrapping. Can be set for
  reproducibility (see \code{\link{set.seed}}). Usually not
  needed.}

  \item{...}{Arguments to pass on to
  \code{\link{pvclust}}.}

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

  \item{nboot}{the number of bootstrap replications. The
  default is \code{1000}.}

  \item{r}{numeric vector which specifies the relative
  sample sizes of bootstrap replications. For original
  sample size \eqn{n} and bootstrap sample size \eqn{n'},
  this is defined as \eqn{r=n'/n}.}
}
\value{
  A pvclust object as returned by the function
  \code{\link{pvclust}}
}
\description{
  p-values are calculated for each branch of the cluster
  dendrogram to indicate the stability of a specific
  partition. \code{clusterBoot} will yield the same
  clusters as the \code{\link{cluster}} function with
  additional biased (BP=bootstrap probabiulity) and
  approximately unbiased (AU) p-values for each partition.
}
\details{
  In standard (hierarchical) cluster analysis the question
  arises which of the idientified clusters are significant
  or emerged by chance. Over the last decade several
  methods have been developed to test clusters for
  robustness. One line of research in this area is based on
  resampling. The idea is to resample the rows or columns
  of the data matrix and to build the dendrogram for each
  bootstrap sample (Felsenstein, 1985). The p-values
  indicates the number of times a specific cluster is
  identified across the bootstrap resamples. As the p-value
  is biased (Hillis & Bull, 1993; Zharkikh & Li, 1995) a
  method called multiscale bootstrap is used to correct the
  p-value (Shimodaira, 2002, 2004). In conventional
  bootstrap analysis the size of the resample is identical
  to the orginal sample size. Multiscale bootstrap varies
  the bootstrap sample size in order to infer a correction
  formula for the biased p-value on the basis of the
  different bootstrap results (Suzuki & Shimodaira, 2006).
}
\examples{
\dontrun{

 # p-values for construct dendrogram
 s <- clusterBoot(boeker)
 plot(s)
 pvrect(s, max.only=FALSE)

 # p-values for element dendrogram
 s <- clusterBoot(boeker, along=2)
 plot(s)
 pvrect(s, max.only=FALSE)
}
}
\author{
  Mark Heckmann
}
\references{
  Felsenstein, J. (1985). Confidence Limits on Phylogenies:
  An Approach Using the Bootstrap. \emph{Evolution, 39}(4),
  783. doi:10.2307/2408678

  Hillis, D. M., & Bull, J. J. (1993). An Empirical Test of
  Bootstrapping as a Method for Assessing Confidence in
  Phylogenetic Analysis. \emph{Systematic Biology, 42}(2),
  182-192.

  Shimodaira, H. (2002) An approximately unbiased test of
  phylogenetic tree selection. \emph{Syst, Biol., 51},
  492-508.

  Shimodaira,H. (2004) Approximately unbiased tests of
  regions using multistep- multiscale bootstrap resampling.
  \emph{Ann. Stat., 32}, 2616-2614.

  Suzuki, R., & Shimodaira, H. (2006). Pvclust: an R
  package for assessing the uncertainty in hierarchical
  clustering. \emph{Bioinformatics, 22}(12), 1540-1542.
  doi:10.1093/bioinformatics/btl117

  Zharkikh, A., & Li, W.-H. (1995). Estimation of
  confidence in phylogeny: the complete-and-partial
  bootstrap technique. \emph{Molecular Phylogenetic
  Evolution, 4}(1), 44-63.
}

