\name{indexPvaff}
\alias{indexPvaff}
\title{Percentage of Variance Accounted for by the First Factor (PVAFF)}
\usage{
  indexPvaff(x)
}
\arguments{
  \item{x}{\code{repgrid} object.}
}
\description{
  The PVAFF is used as a measure of cognitive complexity.
  It was introduced in an unpublished PhD thesis by Jones
  (1954, cit. Bonarius, 1965). To calculate it, the 'first
  factor' is extracted from the construct correlation
  matrix by principal component analysis. The PVAFF
  reflects the amount of variation that is accounted for by
  a single linear component. If a single latent component
  is able to explain the variation in the grid, the
  cognitive complexity is said to be low. In this case the
  construct system is regarded as 'simple' (Bell, 2003).
}
\details{
  The percentage of variance is calculated using the
  corelation matrix of te constructs submitted to
  \code{\link{svd}}.
}
\section{Development}{
  TODO: Results have not yet been checked against other
  grid programs.
}
\examples{
indexPvaff(bell2010)
   indexPvaff(feixas2004)

   # save results to object
   p <- indexPvaff(bell2010)
   p
}
\author{
  Mark Heckmann
}
\references{
  Bell, R. C. (2003). An evaluation of indices used to
  represent construct structure. In G. Chiari & M. L.
  Nuzzo (Eds.), \emph{Psychological Constructivism and the
  Social World} (pp. 297-305). Milan: FrancoAngeli.

  Bonarius, J. C. J. (1965). Research in the personal
  construct theory of George A. Kelly: role construct
  repertory test and basic theory. In B. A. Maher (Ed.),
  \emph{Progress in experimental personality research}
  (Vol. 2). New York: Academic Press.

  James, R. E. (1954). \emph{Identification in terms of
  personal constructs} (Unpublished doctoral thesis). Ohio
  State University, Columbus, OH.
}

