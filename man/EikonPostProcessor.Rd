% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eikonHelperfunctions.r
\name{EikonPostProcessor}
\alias{EikonPostProcessor}
\title{A postprocessor to process Eikon (python) get_Data into r data.frames}
\usage{
EikonPostProcessor(Eikon_get_dataOuput, SpaceConvertor = ".")
}
\arguments{
\item{Eikon_get_dataOuput}{a single Eikon$get_data result or a list of Eikon$get_data results.}

\item{SpaceConvertor}{converts spaces in variables name into one of the following characters ".", "," , "-", "_", default is "."}
}
\value{
a list of a data.frame with cleaned output and the resulting error data.frame
}
\description{
As the Python eikon get_data structure contains various list that are null these should be replaced
with NA to prevent disasters when later handling the data.frame. For example when using the unlist function Null elements
are removed from the lists causing shorter vectors than expected.
}
\examples{
\dontrun{"internal function no examples"}
}
\seealso{
EikonNameCleaner
}
\keyword{internal}
