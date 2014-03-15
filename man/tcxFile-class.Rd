\docType{class}
\name{tcxFile-class}
\alias{tcxFile-class}
\title{TCX File Class}
\description{
Class used to manage a typycal Garmin Connect file (.tcx).
The class has 3 fields used to store the main components
stored in a tcx file: - Activity: as recorded by a Garmin
fitness device - Lap: the higher level of activity
breakdown. A lap is either based on a fixed distance (1
Mile, 1 KM) or defined by when the user is hitting the lap
button on her device - Trackpoint: log of all the points
recorded by the device
}
\details{
The 3 components are stored in 3 data frames which are
fields of the class.

read method: The read method is used to read and parse a
.tcx file. The method expects the Garmin Connect identifier
of the activity as a parameter.
}
\section{Methods}{
\itemize{
\item \code{read(activityId)}: The read method is used to read and parse a .tcx file. The method expects the Garmin Connect identifier of
#' the activity as a parameter.

\item \code{readFromDb()}: Retrieve the 3 activity data frames from the package database.

\item \code{saveToDb()}: Save the 3 activity data frames to the package database.
}}
\examples{
{
f <- tcxFile$new()
f$read(439915418)
}
}
\keyword{data}
\keyword{fitness,}

