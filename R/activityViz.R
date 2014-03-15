#' mapActivity function
#'
#' Plot all the trackpoints of a given activity on a Google Map
#' 
#' @param activityId A Garmin Connect activity id
#'
#' @export
#' @import ggmap
#' 
#' @keywords fitness, data
#' @examples \dontrun{
#' mapActivity(439915418)
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
                  aes(x = longitude,
                      y = latitude),
                         data = f$trackpoint,
                         colour = "red",
                         size = 1,
                         pch = 20) +
              ggtitle(paste(f$activity$sport, f$activity$timestamp, sep=" - "))
}
