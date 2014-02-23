#### References
## https://forums.garmin.com/showthread.php?29754-REST-API-to-connect-garmin-com
## http://www.ciscomonkey.net/gc-to-dm-export/


library(RCurl)


username      <- "";
password      <- "";

#urlGCLogin    <- "http://connect.garmin.com/signin";
urlGCLogin    <- "http://connect.garmin.com/en-US/signin";
urlGCSearch   <- "http://connect.garmin.com/proxy/activity-search-service-1.0/json/activities?";
urlGCActivity <- "http://connect.garmin.com/proxy/activity-service-1.1/gpx/activity/439915418";

curlHandle <- getCurlHandle()

curlSetOpt(curl = curlHandle,
           #ssl.verifypeer = FALSE,
           #useragent = "Mozilla/5.0",
           #timeout = 60,
           followlocation = TRUE,
           #cookiejar = "../inst/extdata/cookies",
           cookiefile = "../inst/extdata/cookies")

html <- getURL(url = urlGCLogin, curl = curlHandle, .opts=list(verbose=TRUE))
 
ret <- postForm(uri=urlGCLogin,
                .params=list(username=username, password=password),
                curl= curlHandle,
                style="HTTPPOST",
                .opts=list(verbose=TRUE)
                )

json <- rawToChar(getURL(url = urlGCSearch, curl = curlHandle, .opts=list(verbose=TRUE)))
