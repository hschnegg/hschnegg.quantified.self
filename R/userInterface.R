#' mapActivity function
#'
#' Plot all the trackpoints of a given activity on a Google Map. The activity has to be
#' available in the local database.
#' 
#' @param activityId A Garmin Connect activity id
#'
#' @export
#' @import ggmap
#' 
#' @keywords fitness, data
#' @examples \dontrun{
#' mapActivity(439915402)
#' }
mapActivity <- function(activityId) {
    # Retrieve data from database
    f <- tcxFile$new()
    f$readFromDb(activityId, c("activity", "trackpoint"))

    # Retrieve Google map
    boundingBox <- make_bbox(lon = f$trackpoint$longitude, lat = f$trackpoint$latitude, f = .1)
    googleMap <- get_map(location = boundingBox,
                         source = "google",
                         maptype = c("roadmap"))

    # Plot trackpoints
    ggmap(googleMap,
          extent = "device") +
              geom_point(
                  aes(x = get("longitude"),
                      y = get("latitude")),
                         data = f$trackpoint,
                         colour = "red",
                         size = 1,
                         pch = 20) +
              ggtitle(paste(f$activity$sport, f$activity$timestamp, sep=" - "))
}


#' listLoadedActivity function
#'
#' List all the activities already loaded into the package internal database. Return
#' a data frame.
#'
#' @export
#' 
#' @keywords fitness, data
#' @examples \dontrun{
#' listLoadedActivity()
#' }
listLoadedActivity <- function() {
    # Retrieve data from database
    f <- tcxFile$new()
    f$readFromDb(dataFrames = "activity")

    f$activity
}


#' listGarminConnectActivity function
#'
#' List activities stored in Garmin Connect. Return a data frame.
#'
#' @param GCuser Garmin Connect username
#' @param GCpassword Garmin Connect password
#'
#' @export
#' 
#' @keywords fitness, data
#' @examples \dontrun{
#' listGarminConnectActivity()
#' }
listGarminConnectActivity <- function(GCuser = "stats290_test", GCpassword = "stats290_test") {
    # Retrieve data from Garmin Connect
    gc <- garminConnect$new()
    gc$username <- GCuser
    gc$password <- GCpassword
    
    gc$retrieveActivityList()
}


#' listNewActivity function
#'
#' List activities stored in Garmin Connect that are not yet stored in the local database.
#'
#' @param GCuser Garmin Connect username
#' @param GCpassword Garmin Connect password
#'
#' @export
#' 
#' @keywords fitness, data
#' @examples \dontrun{
#' listGarminConnectActivity()
#' }
listNewActivity <- function(GCuser = "stats290_test", GCpassword = "stats290_test") {
    GCactivity <- listGarminConnectActivity(GCuser = GCuser, GCpassword = GCpassword)
    GCactivityId <- as.character(GCactivity$id)
    localActivityId <- as.character(listLoadedActivity()$activity_id)

    GCactivity[!(GCactivityId %in% localActivityId), ]
}


saveGarminActivity <- function(activityId, GCuser = "stats290_test", GCpassword = "stats290_test") {
    gc <- garminConnect$new()
    gc$username <- GCuser
    gc$password <- GCpassword

    gc$downloadTcx(activityId)

    f <- tcxFile$new()
    f$read(activityId)
    f$saveToDb()
}
    
