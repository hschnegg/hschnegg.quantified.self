\docType{class}
\name{garminConnect-class}
\alias{garminConnect-class}
\title{Garmin Activity Connect Class}
\description{
This class offers the framework to deal with accessing
Garmin Connect (connect.garmin.com)
}
\section{Methods}{
\itemize{
\item \code{downloadTCX(activityId)}: Download the TCX file for a given activity from Garmin Connect. The file is saved in /inst/extdata

\item \code{login()}: Login to Garmin Connect (website)

\item \code{retrieveActivityList()}: Retrieve a list of the most recent activities posted to Garmin Connect
}}
\examples{
{
gc <- garminConnect$new()
gc$login()
}
}
\keyword{data}
\keyword{fitness,}

