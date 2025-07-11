% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/settings.r
\name{settings}
\alias{settings}
\title{global settings for OpenRepGrid}
\usage{
settings(...)
}
\arguments{
\item{...}{Use parameter value pairs (\verb{par1=val1, par2=val2}) to
change a parameter. Use parameter names to request
parameter's value (\verb{"par1", "par2"}).}
}
\description{
global settings for OpenRepGrid
}
\note{
Currently the following parameters can be changed, ordered by topic.
The default value is shown in the brackets at the end of a line.
\itemize{
\item \code{show.scale}: Show grid scale info? (\code{TRUE})
\item \code{show.meta}: Show grid meta data? (\code{TRUE})
\item \code{show.trim}: Number of chars to trim strings to (\code{30})
\item \code{show.cut}: Maximum number of characters printed on the sides of a grid (\code{20})
\item \code{e.no}: Print element ID number? (\code{TRUE})
\item \code{c.no}: Print construct ID number? (\code{TRUE})
\item \code{preferred}: Print preferred pole indicator? (\code{TRUE})
}
}
\examples{
\dontrun{
# get current settings
settings()

# get some parameters
settings("show.scale", "show.meta")

# change parameters
bell2010

settings(show.meta = F)
bell2010

settings(show.scale = F, show.cut = 30)
bell2010
}

}
