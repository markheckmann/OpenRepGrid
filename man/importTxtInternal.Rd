\name{importTxtInternal}
\alias{importTxtInternal}
\title{ImportTxtInternal is the parser for importTxt.}
\usage{
  importTxtInternal(file, dir = NULL, min = NULL,
    max = NULL)
}
\arguments{
  \item{file}{Filename including path if file is not in
  current working directory. File can also be a complete
  URL. The fileformat is \code{.txt}.}

  \item{dir}{Alternative way to supply the directory where
  the file is located (default \code{NULL}).}

  \item{min}{Optional argument (\code{numeric}, default
  \code{NULL}) for minimum rating value in grid.}

  \item{max}{Optional argument (\code{numeric}, default
  \code{NULL}) for maximum rating value in grid.}
}
\value{
  List of relevant data.
}
\description{
  ImportTxtInternal is the parser for importTxt that
  constructs an import object. The \code{.txt} file has to
  be in a fixed format. There are three mandatory blocks
  each starting and ending with a predefined tag in
  uppercase letters. The first block starts with
  \code{ELEMENTS} and ends with \code{END ELEMENTS} and
  contains one element in each line. The other mandatory
  blocks contain the constructs and ratings (see below). In
  the block containing the constructs the left and right
  pole are seperated by a colon (:). To define missing
  values use \code{NA} like in the example below. One
  optional block contains the range of the rating scale
  used defined by two numbers. The order of the blocks is
  arbitrary. All text not contained within the blocks is
  discarded and can thus be used for comments.
}
\details{
  \tabular{l}{ \code{---------------- .txt file
  -----------------} \cr \cr \code{anything not contained
  within the tags will be discarded} \cr \code{ELEMENTS}
  \cr \code{element 1} \cr \code{element 2} \cr
  \code{element 3} \cr \code{END ELEMENTS} \cr \cr
  \code{CONSTRUCTS} \cr \code{left pole 1 : right pole 1}
  \cr \code{left pole 2 : right pole 2} \cr \code{left pole
  3 : right pole 3} \cr \code{left pole 4 : right pole 4}
  \cr \code{END CONSTRUCTS} \cr \cr \code{RATINGS} \cr
  \code{1 3 2} \cr \code{4 1 1} \cr \code{1 4 4} \cr
  \code{3 1 1} \cr \code{END RATINGS} \cr \cr \code{RANGE}
  \cr \code{1 4} \cr \code{END RANGE} \cr
  \code{---------------- end of file ----------------} \cr
  }

  Note that the maximum and minimum value has to be defined
  using the \code{min} and \code{max} arguments if no
  \code{RANGE} block is contained in the data file.
  Otherwise the scaling range is inferred from the
  available data and a warning is issued as the range may
  be erroneous. This may effect other functions that depend
  on knowing the correct range and it is thus strongly
  recommended to set the scale range correctly.

  Question marks (?) in the ratings are treated as missing
  data.
}
\examples{
\dontrun{

# supposing that the data file sample.txt is in the current directory
file <- "sample.txt"
imp <- importTxtInternal(file)

# specifying a directory (arbitrary example directory)
dir <- "/Users/markheckmann/data"
imp <- importTxtInternal(file, dir)

# using a full path
imp <- importTxtInternal("/Users/markheckmann/data/sample.txt")

# load Gridsuite data from URL
imp <- importTxtInternal("http://www.openrepgrid.uni-bremen.de/data/sample.txt")
}
}
\author{
  Mark Heckmann
}
\keyword{internal}

