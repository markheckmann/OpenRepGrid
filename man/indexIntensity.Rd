\name{indexIntensity}
\alias{indexIntensity}
\title{Intensity index}
\usage{
  indexIntensity(x, rc = FALSE, output = TRUE, trim = 30,
    digits = 2)
}
\arguments{
  \item{x}{\code{repgrid} object.}

  \item{rc}{Whether to use Cohen's rc for the calculation
  of inter-element correlations. See also
  \code{\link{elementCor}} for further explanations.}

  \item{output}{Logical. Whether to print the results to
  the console (default \code{TRUE}).}

  \item{trim}{The number of characters a construct is
  trimmed to (default is \code{30}). If \code{NA} no
  trimming occurs. Trimming simply saves space when
  displaying correlation of constructs or elements with
  long names.}

  \item{digits}{Numeric. Number of digits to round to
  (default is \code{2}).}
}
\value{
  Invisibly returns \code{list} object containing: \cr
  \item{c.int}{Intensity scores by construct.}
  \item{e.int}{Intensity scores by element.}
  \item{e.int}{Intensity scores by element.}
  \item{c.int.mean}{Average intensity score for
  constructs.} \item{e.int.mean}{Average intensity score
  for elements.} \item{total.int}{Toal intensity score.}
}
\description{
  Calculate intensity index.
}
\details{
  The Intensity index has been suggested by Bannister
  (1960) as a measure of the amount of construct linkage.
  Bannister suggested that the score reflects the degree of
  organization of the construct system under investigation
  (Bannister & Mair, 1968). The index resulted from his and
  his colleagues work on construction systems of patient
  suffering schizophrenic thought disorder. The concept of
  intensity has a theoretical connection to the notion of
  "tight" and "loose" construing as proposed by Kelly
  (1991). While tight constructs lead to unvarying
  prediction, loose constructs allow for varying
  predictions. Bannister hypothesized that schizophrenic
  thought disorder is liked to a process of extremely loose
  construing leading to a loss of predictive power of the
  subject's construct system. The Intensity score as a
  structural measure is thought to reflect this type of
  system disintegration (Bannister, 1960).

  Implementation as in the Gridcor programme and explained
  on the correspoding help pages: "\ldots the sum of the
  squared values of the correlations of each construct with
  the rest of the constructs, averaged by the total number
  of constructs minus one. This process is repeated with
  each element, and the overall Intensity is calculated by
  averaging the intensity scores of constructs and
  elements."
  \url{http://www.terapiacognitiva.net/record/pag/man11.htm}.
  Currently the total is calculated as the unweighted
  average of all single scores (for elements and
  construct).
}
\note{
  TODO: Results have not been tested against other
  programs' results.
}
\examples{
\dontrun{

 indexIntensity(bell2010)
 indexIntensity(bell2010, trim=NA)

 # using Cohen's rc for element correlations
 indexIntensity(bell2010, rc=T)

 # prevent output to console
 res <- indexIntensity(bell2010, out=F)
 res
}
}
\author{
  Mark Heckmann
}
\references{
  Bannister, D. (1960). Conceptual structure in
  thought-disordered schizophrenics. \emph{The Journal of
  mental science}, 106, 1230-49.
}

