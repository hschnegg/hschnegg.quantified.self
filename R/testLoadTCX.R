library(XML)

saxHandlers <- function() {
    trackpointId <- 0
    timestamp <- character()
    latitude <- character()
    longitude <- character()
    altitude <- character()
    distance <- character()
    speed <- character()
    cadence <- character()
    currentNode <- ""

    startElement <- function(node, atts) {
        if (node == "Trackpoint")
            trackpointId <<- trackpointId + 1
        if (node == "Time")
            currentNode <<- "time"
        if (node == "LatitudeDegrees")
            currentNode <<- "latitude"
        if (node == "LongitudeDegrees")
            currentNode <<- "longitude"
        if (node == "AltitudeMeters")
            currentNode <<- "altitude"
        if (node == "DistanceMeters")
           currentNode <<- "distance"
        if (node == "Speed")
            currentNode <<- "speed"
        if (node == "RunCadence")
            currentNode <<- "cadence"
    }

    endElement <- function(node, atts) {
        if (node == "Time")
            currentNode <<- ""
        if (node == "LatitudeDegrees")
            currentNode <<- ""
        if (node == "LongitudeDegrees")
            currentNode <<- ""
        if (node == "AltitudeMeters")
            currentNode <<- ""
        if (node == "DistanceMeters")
           currentNode <<- ""
        if (node == "Speed")
            currentNode <<- ""
        if (node == "RunCadence")
            currentNode <<- ""
    }

    text <- function(x, ...) {
        if (currentNode == "time")
            timestamp[trackpointId] <<- x
        if (currentNode == "latitude")
            latitude[trackpointId] <<- x
        if (currentNode == "longitude")
            longitude[trackpointId] <<- x
        if (currentNode == "altitude")
            altitude[trackpointId] <<- x
        if (currentNode == "distance")
           distance[trackpointId] <<- x
        if (currentNode == "speed")
            speed[trackpointId] <<- x
        if (currentNode == "cadence")
            cadence[trackpointId] <<- x
    }

    getValues <- function() {
        data.frame(timestamp, latitude, longitude, altitude, distance, speed, cadence, stringsAsFactors=FALSE)
    }
    
    list(startElement = startElement,
         endElement = endElement,
         text = text,
         getValues = getValues)
}

listTcxData <- xmlEventParse(file = "../inst/extdata/activity_439915418.tcx", handlers = saxHandlers())

tcxData <- listTcxData$getValues()

tcxData <- transform(tcxData,
                     timestamp = as.POSIXct(timestamp, format="%Y-%m-%dT%H:%M:%S"),
                     latitude = as.numeric(latitude),
                     longitude = as.numeric(longitude),
                     altitude = as.numeric(altitude),
                     distance = as.numeric(distance),
                     speed = as.numeric(speed),
                     cadence = as.numeric(cadence))

save(tcxData, file="../data/tcxData.Rda")
