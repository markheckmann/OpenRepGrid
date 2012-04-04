\name{formatMatrix}
\alias{formatMatrix}
\title{Format a matrix and add index column.}
\usage{
  formatMatrix(x, rnames = rownames(x),
    pre.index = c(T, F), cnames = seq_len(ncol(x)),
    indexcol = F, digits = 2, diag = F, mode = 1)
}
\arguments{
  \item{x}{A matrix onbject.}

  \item{rnames}{Row names.}

  \item{cnames}{Column names.}

  \item{pre.index}{Whether to make index prefix for rows
  and column names.}

  \item{indexcol}{Whether to make an index column.}

  \item{diag}{Whether to show diagonal.}

  \item{mode}{Whether to show upper (mode=1), lower
  (mode=2) or both triangles (mode=0) of the matrix.}
}
\description{
  Format a matrix and add index column.
}
\author{
  Mark Heckmann
}
\keyword{internal}

