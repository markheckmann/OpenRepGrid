% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.r
\name{importDataframe}
\alias{importDataframe}
\title{Convert a dataframe into a repgrid object.}
\usage{
importDataframe(
  x,
  format = "element_columns",
  rmin = NULL,
  rmax = NULL,
  pole_sep = ":"
)
}
\arguments{
\item{x}{A dataframe. See Detail section and examples for required format.}

\item{format}{One of  \code{element_columns} (default), \code{construct_columns}, or \code{long}. See corresponding sections below.}

\item{rmin, rmax}{Min and max of rating scale.}

\item{pole_sep}{Character(s) to seperate the constructs poles (defaults to a colon) for format \code{construct_columns}.
Without a separator, constructs are used as right poles, all left poles will be \code{NA}.}
}
\value{
A `repgrid`` object.
}
\description{
There are three different dataframe formats from which a \code{repgrid} object can be created:
Columns are a) \code{element_columns}, b) \code{construct_columns}, c) \code{long}.
Three corresponding sample dataframes, (\link{df_element_columns}, \link{df_construct_columns}, and \link{df_long}) are included
in the package (see examples). See Detail section below for more info.
}
\section{Format \code{element_columns}}{


In this format, each element has a separate column, and each row contains the ratings for one construct.
It is a common way to represent grid data and looks like this.

\tabular{lccccrr}{
\code{1}           \tab \code{element_1}  \tab \code{element_2} \tab \code{element_3} \tab \code{element_4}  \tab \code{5} \tab \code{preferred} \cr
\code{left_pole_1} \tab \code{1}   \tab \code{5}  \tab \code{3}  \tab \code{4}   \tab \code{right_pole_1} \tab \code{left} \cr
\code{left_pole_2} \tab \code{3}   \tab \code{1}  \tab \code{1}  \tab \code{3}   \tab \code{right_pole_2} \tab \code{right} \cr
\code{left_pole_3} \tab \code{4}   \tab \code{2}  \tab \code{5}  \tab \code{1}   \tab \code{right_pole_3} \tab \code{NA} \cr
}

The columns names contains the minimum of the rating scale (\code{1}), the names of the elements (\code{element_1} to
\code{element_4}), the maximum of the rating scale (\code{5}), and optionally the column \code{preferred}, indicating the preferred
pole. Each row contains the constructs entries (left pole, ratings, right pole, preferred pole). The preferred pole
must be one of \code{left}, \code{right}, \code{none}, \code{NA} (see \code{\link[=preferredPoles]{preferredPoles()}}). See sample dataframe \link{df_element_columns}.
}

\section{Format \code{construct_columns}}{


In this format, each construct has a separate column, and each row contains represents element. This format often
results when summarising data (see examples). It looks like this:

The first column is named \code{elements} followed by the constructs. The construct poles are separated by a colon by
default (see arg \code{pole_sep}). The rows below contain the elements' entries (element name, ratings). The min and max
of the rating scale should be passed explicitly via the args \code{rmin} and \code{rmax}. See sample dataframe
\link{df_construct_columns}.

\tabular{lrrr}{
\code{elements}  \tab \code{left_pole_1:right_pole_1} \tab \code{left_pole_2:right_pole_2} \tab \code{left_pole_3:right_pole_3} \cr
\code{element_1} \tab \code{1}                        \tab \code{5}                        \tab \code{3}                        \cr
\code{element_2} \tab \code{3}                        \tab \code{1}                        \tab \code{1}                        \cr
\code{element_3} \tab \code{4}                        \tab \code{2}                        \tab \code{5}                        \cr
}
}

\section{Format \code{long}}{


The \code{long} format gets its name from the fact, that it has less columns, but many more rows. It is a common format
in data analytics. Here, each row contains a different element-construct combination and the corresponding rating
value. The format looks like this:

\tabular{lllrlrr}{
\code{element}   \tab \code{left_pole}   \tab \code{right_pole}   \tab \code{rating} \tab \code{preferred_pole} \tab \code{rmin} \tab \code{rmax} \cr
\verb{element 1} \tab \verb{left pole 1} \tab \verb{right pole 1} \tab \code{1}      \tab \code{left}           \tab \code{1}    \tab \code{5}    \cr
\code{element_2} \tab \verb{left pole 1} \tab \verb{right pole 1} \tab \code{5}      \tab \code{left}           \tab \code{1}    \tab \code{5}    \cr
\code{element_3} \tab \verb{left pole 1} \tab \verb{right pole 1} \tab \code{4}      \tab \code{left}           \tab \code{1}    \tab \code{5}    \cr
}

The columns \code{element}, \code{left_pole}, \code{right_pole}, and \code{rating} are mandatory, the columns \code{preferred_pole}, \code{rmin},
and \code{rmax} are optional. \code{rmin} and \code{rmax} contain the min and max of the rating scale. Alternatively, you may
pass \code{rmin} and \code{rmax} as arguments in the function call. See sample dataframe \link{df_long}.
}

\examples{
# dataframe with elements as columns (preferred way)
importDataframe(df_element_columns)

# dataframe with constructs as columns
importDataframe(df_construct_columns, format = "construct_columns", rmin = 1, rmax = 5)

# dataframe with long format
importDataframe(df_long, format = "long", rmin = 1, rmax = 5)
}
\seealso{
Import data
\code{\link{importExcel}()},
\code{\link{importGridcor}()},
\code{\link{importGridstat}()},
\code{\link{importGridsuite}()},
\code{\link{importScivesco}()},
\code{\link{importTxt}()}
}
\concept{import}
