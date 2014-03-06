garminActivities <- setRefClass(Class = "garminActivities",
                                fields = list(username = "character",
                                    password = "character"))

garminActivities$methods(
    connectGC = function(username = "stats290_test",
                          password = "stats290_test") {

        curlHandle <- getCurlHandle()

        curlSetOpt(curl = curlHandle,
                   cookiefile = system.file(package = .global.constants()$packageName, "inst", "extdate", "gc_cookies.txt"),
                   followlocation = FALSE,
                   verbose = FALSE,
                   header = TRUE)
        
        # Initial query to retrieve session and "flow execution key"
        gcPreResp <- getURL(url = .garmin.constants()$urlGCsignin, curl = curlHandle)

        flowExecKey <- substr(x=gcPreResp, gregexpr(pattern="flowExecutionKey", text=gcPreResp)[[1]][1] + 19, gregexpr(pattern="flowExecutionKey", text=gcPreResp)[[1]][1] + 22)

        # Submit form
        formResp <- postForm(uri=.garmin.constants()$urlGCsignin,
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

        html <- getURL(url = url, curl = curlHandle, .opts=list(followlocation = TRUE))

        json <- getURLContent(url = .garmin.constants()$urlGCsearch, curl = curlHandle)
        json
    })
