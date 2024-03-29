% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.r
\name{importExcelInternal}
\alias{importExcelInternal}
\title{workhorse function (parser) for importExcel.}
\usage{
importExcelInternal(file, dir = NULL, sheetIndex = 1, min = NULL, max = NULL)
}
\arguments{
\item{file}{A vector of filenames including the full path if file is not in current working
directory. The file suffix has to be \code{.xlsx} (used since Excel 2007).}

\item{dir}{Alternative way to supply the directory where the file is located
(default \code{NULL}).}

\item{sheetIndex}{The number of the Excel sheet that contains the grid data.}

\item{min}{Optional argument (\code{numeric}, default \code{NULL})
for minimum rating value in grid.}

\item{max}{Optional argument (\code{numeric}, default \code{NULL})
for maximum rating value in grid.}
}
\description{
workhorse function (parser) for importExcel.
}
\keyword{internal}
