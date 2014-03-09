\docType{class}
\name{garminActivities-class}
\alias{garminActivities-class}
\title{Garmin Activity Class}
\description{
This class offers the framework to deal with Garmin Connect
data
}
\section{Methods}{
\itemize{
\item \code{connectGC()}: Login to Garmin Connect (website)

\item \code{downloadTCX(activityId)}: Download the TCX file for a given activity from Garmin Connect. The file is saved in /inst/extdata

\item \code{retrieveGCactivityList()}: Retrieve a list of the most recent activities posted to Garmin Connect
}}
\examples{
{
act <- garminActivities$new()
act$connectGC()
}
}
\keyword{XXX}

