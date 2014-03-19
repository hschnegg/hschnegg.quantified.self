\docType{class}
\name{garminConnect}
\alias{garminConnect-class}
\title{Garmin Connect Class}
\description{
This class offers the framework to deal with accessing
Garmin Connect (connect.garmin.com)
}
\details{
username field: Your Garmin Connect username

password field: Your Garmin Connect password

login method: Open a connection to the Garmin Connect web
site using the username and password fields to log you in.
Returns the CURL handle that can be used to issue queries
against the Garmin Connect REST API.

retrieveActivityList method: Retrieve a list of the latest
activities stored in Garmin Connect

downloadTcx method: Download the TCX file for a given
activity from Garmin Connect. The file is saved in extdata.
The method expects an Activity Id as a parameter.
}
\section{Methods}{
\itemize{
\item \code{downloadTcx(activityId)}: Download the TCX file for a given activity from Garmin Connect. The file is saved in extdata. Expects the activity id as a parameter. The file is saved in extdata.

\item \code{login()}: Login to Garmin Connect (website)

\item \code{retrieveActivityList()}: Retrieve a list of the most recent activities posted to Garmin Connect
}}
\examples{
\dontrun{
gc <- garminConnect$new()
gc$login()
gc$retrieveActivityList()
gc$download(454818889)
}
}
\keyword{data}
\keyword{fitness,}

