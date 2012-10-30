\name{makeRepgrid}
\alias{makeRepgrid}
\title{Make a new repgrid object.}
\usage{
  makeRepgrid(args)
}
\arguments{
  \item{args}{Arguments needed for the construction of the
  grid (list).  These include \code{name} followed by a
  vector containing the element names. \code{l.name}
  followed by a vector with the left construct poles.
  \code{r.name} followed by a vector with the right
  construct poles. \code{scores} followed by a vector
  containing the rating scores row wise.}
}
\value{
  \code{NULL}
}
\description{
  The function creates a \code{repgrid} object from
  scratch. A number of paramters have to be defined in
  order to make a new grid (see parameters).
}
\examples{
\dontrun{

   # make list object containing the arguments
   args <- list( name=c("element_1", "element_2", "element_3", "element_4"),
		              l.name=c("left_1", "left_2", "left_3"),
		  	          r.name=c("right_1", "right_2", "right_3"),
		  	          scores=c(	1,0,1,0,
						                1,1,1,0,
						                1,0,1,0	) )
   # make grid object
   x <- makeRepgrid(args)
   x
}
}
\author{
  Mark Heckmann
}

