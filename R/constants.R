.global.constants <- function() {
    "Constants used throughout the package"

    list(packageName = "hschnegg.quantified.self")

}

.database.constants <- function() {
    "Constants used to work with package database"

    dbFileName <- "hschnegg.quantified.self.db"
    db <- system.file(package = .global.constants()$packageName, "extdata", dbFileName)

    list(db = db)
    
}

.garmin.constants <- function() {
    "Constants used to work with Garmin Connect data"

    list(urlGCsignin = "https://sso.garmin.com/sso/login?service=http%3A%2F%2Fconnect.garmin.com%2Fpost-auth%2Flogin&webhost=olaxpw-connect07.garmin.com&source=http%3A%2F%2Fconnect.garmin.com%2Fde-DE%2Fsignin&redirectAfterAccountLoginUrl=http%3A%2F%2Fconnect.garmin.com%2Fpost-auth%2Flogin&redirectAfterAccountCreationUrl=http%3A%2F%2Fconnect.garmin.com%2Fpost-auth%2Flogin&gauthHost=https%3A%2F%2Fsso.garmin.com%2Fsso&locale=de&id=gauth-widget&cssUrl=https%3A%2F%2Fstatic.garmincdn.com%2Fcom.garmin.connect%2Fui%2Fsrc-css%2Fgauth-custom.css&clientId=GarminConnect&rememberMeShown=true&rememberMeChecked=false&createAccountShown=true&openCreateAccount=false&usernameShown=true&displayNameShown=false&consumeServiceTicket=false&initialFocus=true&embedWidget=false",

         urlGCvalidate = "http://connect.garmin.com/user/username",

         urlGCactivityList = "http://connect.garmin.com/proxy/activity-search-service-1.0/json/activities",

         urlGCtcxFile = "http://connect.garmin.com/proxy/activity-service-1.0/tcx/activity/XXX?full=true"

         )
}
