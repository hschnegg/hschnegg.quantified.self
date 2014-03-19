\name{listNewActivity}
\alias{listNewActivity}
\title{listNewActivity function}
\usage{
listNewActivity(GCuser = "stats290_test", GCpassword = "stats290_test")
}
\arguments{
  \item{GCuser}{Garmin Connect username}

  \item{GCpassword}{Garmin Connect password}
}
\description{
List activities stored in Garmin Connect that are not yet
stored in the local database. Returns a data frame.
}
\examples{
\dontrun{
listNewActivity()
}
}
\keyword{data}
\keyword{fitness,}

