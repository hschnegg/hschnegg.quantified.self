\docType{class}
\name{tcxFile}
\alias{tcxFile}
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
local .tcx file. The method expects the Garmin Connect
identifier of an activity or a file name as a parameter.
The file should be stored in the extdata folder.

saveToDb method: Save the 3 activity data frames stored in
the class fields to the package database.

readFromDb method: Retrieve the activity data frames from
the package database. - Parameter activityId: controls
which activity to retrieve (missing is all). - Parameter
dataFrames: controls which data frame to retrieve (missing
or 'all' would retrieve all of them). The data frames are
stored in the class fields
}
\section{Methods}{
\itemize{
\item \code{initialize(...)}: Define the structure of the three data frames.

\item \code{read(activityId<U+00A0>=<U+00A0>"", fileName<U+00A0>=<U+00A0>"")}: The read method is used to read and parse a local .tcx file. The method expects the Garmin Connect identifier of an activity or a file name as a parameter. The file should be stored in the extdata folder.

\item \code{readFromDb(activityId<U+00A0>=<U+00A0>"", dataFrames<U+00A0>=<U+00A0>"all")}: Retrieve the activity data frames from the package database. Parameter activityId controls which activity to retrieve (missing is all). Parameter dataFrames controls which data frame to retrieve (missing or 'all' would retrieve all of them). The data frames are stored in the class fields.

\item \code{saveToDb()}: Save the 3 activity data frames (see fields) to the package database.
}}
\examples{
\dontrun{
f <- tcxFile$new()
f$read(439915418)
f$saveToDb()
f$readFromDb()
}
}
\keyword{data}
\keyword{fitness,}

