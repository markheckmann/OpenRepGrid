\name{multigridFileToSinglegridFiles}
\alias{multigridFileToSinglegridFiles}
\title{Converts a Gridstat multigrid file into temporary single grid files and
returns their path}
\usage{
  multigridFileToSinglegridFiles(file)
}
\arguments{
  \item{file}{Filenames of Gridstat file}
}
\value{
  A vector containing the paths to the temp files
}
\description{
  The format for a multigrid file resembles the single
  Gridstat data file. The lines of the single files are
  simply placed below each other without any blank lines in
  between. The function reads in a file and tests if it is
  a multigrid file. Multigrid files are seperated into
  single Gridstat temp files. The file path for the temp
  files is returned. If the file is a single grid files the
  path is left unaltered.
}
\keyword{internal}

