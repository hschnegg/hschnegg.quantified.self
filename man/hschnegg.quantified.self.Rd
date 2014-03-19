\docType{package}
\name{hschnegg.quantified.self}
\alias{hschnegg.quantified.self}
\alias{hschnegg.quantified.self-package}
\title{hschnegg.quantified.self}
\description{
This package allows to retrieve activities stored in Garmin
Connect. Once downloaded, activities can be saved in an
internal database for further analysis.
}
\details{
Class garminConnect enables the connection to Garmin
Connect and the download of activity files (.tcx).

Class tcxFile enables the parsing of activity files (.tcx)
and the access to the package internal database. The class
allows to produce data frames from activity data stored in
.tcx files or already available in the database. It also
manages the insertion of new activities into the database.

userInterface.R provides some helper functions for the
user.

constants.R provides functions returning constants used
throughout the package.

The internal database is stored in extdata
(hschnegg.quantified.self.db). A script to recreate the
tables is available in folder sql.
}

