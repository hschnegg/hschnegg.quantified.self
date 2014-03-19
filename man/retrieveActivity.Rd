\name{retrieveActivity}
\alias{retrieveActivity}
\title{retrieveActivity function}
\usage{
retrieveActivity(activityId, GCuser = "stats290_test",
  GCpassword = "stats290_test")
}
\arguments{
  \item{activityId}{Garmin Connect activity id}

  \item{GCuser}{Garmin Connect username}

  \item{GCpassword}{Garmin Connect password}
}
\description{
The function downloads an activity from Garmin Connect,
parses the .tcx file and saves the data into the local
database.
}
\examples{
\dontrun{
retrieveActivity()
}
}
\keyword{data}
\keyword{fitness,}

