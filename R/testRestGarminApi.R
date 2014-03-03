# https://github.com/cpfair/tapiriik/blob/master/tapiriik/services/GarminConnect/garminconnect.py
# https://forums.garmin.com/showthread.php?72150-connect-garmin-com-signin-question&p=264580

library(RCurl)

username      <- ""
password      <- ""

urlGCsignin1 <- "https://sso.garmin.com/sso/login?service=http%3A%2F%2Fconnect.garmin.com%2Fpost-auth%2Flogin&webhost=olaxpw-connect07.garmin.com&source=http%3A%2F%2Fconnect.garmin.com%2Fde-DE%2Fsignin&redirectAfterAccountLoginUrl=http%3A%2F%2Fconnect.garmin.com%2Fpost-auth%2Flogin&redirectAfterAccountCreationUrl=http%3A%2F%2Fconnect.garmin.com%2Fpost-auth%2Flogin&gauthHost=https%3A%2F%2Fsso.garmin.com%2Fsso&locale=de&id=gauth-widget&cssUrl=https%3A%2F%2Fstatic.garmincdn.com%2Fcom.garmin.connect%2Fui%2Fsrc-css%2Fgauth-custom.css&clientId=GarminConnect&rememberMeShown=true&rememberMeChecked=false&createAccountShown=true&openCreateAccount=false&usernameShown=true&displayNameShown=false&consumeServiceTicket=false&initialFocus=true&embedWidget=false"

urlGCsearch <- "http://connect.garmin.com/user/username"

curlHandle <- getCurlHandle()

# Initial query
curlSetOpt(curl = curlHandle,
           cookiefile = "../inst/extdata/cookies.txt",
           followlocation = FALSE,
           verbose = TRUE,
           header = TRUE)

gcPreResp <- getURL(url = urlGCsignin1, curl = curlHandle)
substr(x=gcPreResp, start=10, stop=12)

flowExecKey <- substr(x=gcPreResp, gregexpr(pattern="flowExecutionKey", text=gcPreResp)[[1]][1] + 19, gregexpr(pattern="flowExecutionKey", text=gcPreResp)[[1]][1] + 22)

formResp <- postForm(uri=urlGCsignin1,
                .params=list(
                    "_eventId"="submit",
                    "displayNameRequired"="false",
                    "embed"="true",
                    "lt"=flowExecKey,
                    "password"=password,
                    "username"=username),
                curl=curlHandle,
                style="POST",
                .checkParams=TRUE
                )

url <- substr(x=formResp[1], gregexpr(pattern="response_url", text=formResp[1])[[1]][1], gregexpr(pattern="response_url", text=formResp[1])[[1]][1] + 200)
url <- substr(x=url, gregexpr(pattern="'", text=url)[[1]][1] + 1, gregexpr(pattern="'", text=url)[[1]][2] - 1)
url

curlSetOpt(curl = curlHandle,
           cookiefile = "../inst/extdata/cookies.txt",
           followlocation = TRUE,
           verbose = TRUE,
           header = TRUE)

html <- getURL(url = url, curl = curlHandle)



urlGCsignin2 <- "https://sso.garmin.com/sso/login?service=http%3A%2F%2Fconnect.garmin.com%2Fpost-auth%2Flogin&webhost=olaxpw-connect07.garmin.com&source=http%3A%2F%2Fconnect.garmin.com%2Fde-DE%2Fsignin&redirectAfterAccountLoginUrl=http%3A%2F%2Fconnect.garmin.com%2Fpost-auth%2Flogin&redirectAfterAccountCreationUrl=http%3A%2F%2Fconnect.garmin.com%2Fpost-auth%2Flogin&gauthHost=https%3A%2F%2Fsso.garmin.com%2Fsso&locale=de&id=gauth-widget&cssUrl=https%3A%2F%2Fstatic.garmincdn.com%2Fcom.garmin.connect%2Fui%2Fsrc-css%2Fgauth-custom.css&clientId=GarminConnect&rememberMeShown=true&rememberMeChecked=false&createAccountShown=true&openCreateAccount=false&usernameShown=true&displayNameShown=false&consumeServiceTicket=false&initialFocus=true&embedWidget=false"

#h = basicHeaderGatherer()
ssoResp <- postForm(uri=urlGCsignin2,
                .params=list(
                    "username"=username,
                    "password"=password,
                    "_eventId"="submit",
                    "embed"="true"),
                curl=curlHandle,
                style="POST",
                .checkParams=TRUE
                #.opts=curlOptions(headerfunction=h$update)
                )
#h$value()

# Post auth login
postAuth <- getURL(url = urlGCsignin3, curl = curlHandle)
substr(x=postAuth, start=10, stop=12)


json <- getURLContent(url = urlGCsearch, curl = curlHandle)
json

rm(curlHandle)


https://github.com/braiden/python-ant-downloader/blob/master/antd/connect.py
