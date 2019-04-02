#================================================================CR
#  0) Configuring Proxy server
#================================================================CR
# At JRC we have now a proxy that block downlaoding from github, using the following code, we can download from Github
### Shiny input  Window 1: PROXY, url, port, login and password ###
PROXY = TRUE     
URL      = "10.168.209.72"; PORT     = 8012; LOGIN    = NULL; PASSWORD = NULL
# no login and no password on our proxy 
### END of Shiny input ###
if (PROXY) {
    
    # checking that we have the httr package to use function Set_Config()
    if ("httr" %in% rownames(installed.packages()) == FALSE) {
        cat("[ASEConfig] INFO Installing httr", sep = "\n")
        install.packages("httr")
    } else cat("[ASEConfig] INFO Package httr already installed", sep = "\n")
    require("httr")
    cat("[ASEConfig] INFO Package httr loaded", sep = "\n")
    
    # implement PROXY 
    if (is.null(LOGIN)) {
        set_config(use_proxy(url=URL, port=PORT))
    } else {
        set_config( use_proxy(url=URL, port=PORT, username    = LOGIN, password = PASSWORD))
    }
    
} else reset_config()

# Using R to download and parse JSON: an example using data from an open data portal
# http://zevross.com/blog/2015/02/12/using-r-to-download-and-parse-json-an-example-using-data-from-an-open-data-portal/
#================================================================CR
# 1) Grab the data ====
#================================================================CR
# CUrl needed see http://appliedmathbytes.com/2015/08/using-json-in-r/
# Otherwise we will get error Error in file(con, “r”) : cannot open the connection
library(RCurl)
library(curl)
library(jsonlite)
library(httr)           # httr::content
library(lubridate)      # ymd_hms
library(openair)
library(tidyverse)
library(data.table)

# from the website
URL          <- "https://ubis-air.a-i-p.com/UidepService/values/simple?"
username     <- "datareq"
password     <- "aip2jrc"
organisation <- "ABCIS"
station      <- "JRC+Ispra"
start        <- "2019-03-01-00-00-00"
end          <- "2019-03-05-10-00-00"
# Reference.JSON <-jsonlite::fromJSON(txt = paste0("aip2jrc",
#                                 "username=", username,"&",
#                                 "password=", password,"&",
#                                 "organisation=", organisation,"&",
#                                 "station=", station,"&",
#                                 "start=", start,"&",
#                                 "end=", end),
#                                 simplifyDataFrame = TRUE,
#                                 flatten = TRUE
# )

Reference.JSON.httr <- httr::GET(URLencode(paste0("https://ubis-air.a-i-p.com/UidepService/values/simple?",
                           "username=", username,"&",
                           "password=", password,"&",
                           "organisation=", organisation,"&",
                           "station=", station,"&",
                           "start=", start,"&",
                           "end=", end)
                    )
)

a_i_p_param <- function(URL, username, password, organisation, station, start, end = NULL) {
    
    #================================================================CR
    # 1) Grab the data
    #================================================================CR
    # CUrl needed see http://appliedmathbytes.com/2015/08/using-json-in-r/
    # Otherwise we will get error Error in file(con, “r”) : cannot open the connection
    library(RCurl)
    library(curl)
    library(jsonlite)
    library(httr)           # httr::content
    library(lubridate)      # ymd_hms
    # End date
    end <- format(as.Date(lubridate::ymd_hms(start)) + 3, "%Y-%m-%d-%H-%M-%S")
    Reference.JSON.httr <- httr::GET(URLencode(paste0(URL, "username=", username,"&",
                                                      "password=", password,"&",
                                                      "organisation=", organisation,"&",
                                                      "station=", station,"&",
                                                      "start=", start,"&",
                                                      "end=", end)))
    
    #================================================================CR
    # 2) Extract the data from the JSON file ====
    #================================================================CR
    # extract the data node
    Reference <- content(Reference.JSON.httr, type = "application/json", as = 'parsed')
    Reference <- Reference$Stations[[1]]$Devices
    
    #================================================================CR
    # 3) Returning parameters
    #================================================================CR
    return(sapply(seq_along(Reference), function(i) Reference[[i]]$Components[[1]]$Component  ))
}
param <- a_i_p_param(URL, username, password, organisation, station, start) 

a_i_p_data  <- function(URL, username, password, organisation, station, start, end, param) {
    # URL          character string indicating the a-i-p URL for data transmission
    # username     character string <- "datareq"
    # password     character string <- "aip2jrc"
    # organisation character string <- "ABCIS"
    # station      character string indicating <- "JRC+Ispra"
    # start        character string indicating the starting date for data downlaod, format: "2019-03-01-00-00-00"
    # end          character string indicating the endinding date for data downlaod, format: "2019-03-01-00-00-00"
    
    #================================================================CR
    # 1) Grab the data
    #================================================================CR
    Reference.JSON.httr <- httr::GET(URLencode(paste0(URL, "username=", username,"&",
                                                      "password=", password,"&",
                                                      "organisation=", organisation,"&",
                                                      "station=", station,"&",
                                                      "start=", start,"&",
                                                      "end=", end)))
    
    #================================================================CR
    # 2) Extract the data from the JSON file ====
    #================================================================CR
    # extract the data node
    Reference <- content(Reference.JSON.httr, type = "application/json", as = 'parsed')
    Reference <- Reference$Stations[[1]]$Devices
    
    # determining Component
    Components <- sapply(seq_along(Reference), function(i) Reference[[i]]$Components[[1]]$Component)
    # determining Units
    Units      <- sapply(seq_along(Reference), function(i) Reference[[i]]$Components[[1]]$Unit)
    # counts of data per parameter
    Counts     <- sapply(seq_along(Reference), function(i) length(Reference[[i]]$Components[[1]]$MeasuredValues))
    
    # Reference data
    MeasuredValues <- lapply(seq_along(Reference), function(i) {
        
        if (Components[i] %in% param) {
            cat(paste0("Component: ",Components[i]," is being downloadedded\n"))
            
            # Downloading data, taking only valid measurements, dropping Valid, Convert Time to Posix format   
            Param.i <- data.table::rbindlist(lapply(Reference[[i]]$Components[[1]]$MeasuredValues, as.data.frame.list), fill = T) %>% 
                dplyr::filter(Valid == TRUE) %>% 
                dplyr::select(Time,Value) %>% 
                dplyr::mutate(Time = ymd_hms(Time, tz = "UTC"))
                
            colnames(Param.i) <- c("date", Components[i])
            
            # convert to xts
            #Param.i <- xts::xts(x = Param.i[,2], order.by = Param.i$date) #, tzone = threadr::time_zone(Param.i$date)
        }
        return(Param.i)
    })
    # Creating RefData
    for (i in seq_along(MeasuredValues)[-length(MeasuredValues)]) {
        if (exists("RefData")) RefData <- merge(x = RefData, y = MeasuredValues[[i]], by = "date") else RefData <- MeasuredValues[[i]]
    }
    
    # Deleting unecessary dataFrame
    if (exists("Reference"))           remove(Reference)
    if (exists("Reference.JSON.httr")) remove(Reference.JSON.httr)
    if (exists("MeasuredValues"))      remove(MeasuredValues)
    
    #================================================================CR
    # 3) convert Reference names
    #================================================================CR
    # list of equivalent reference names
    Ref.names <- list(NO2    = c("NO2","nitrogen dioxide"),
                      CO_ppm = c("CO_ppm", "carbon monoxide"),
                      O3     = c("O3", "ozone"),
                      NO     = c("NO", "nitrogen monoxide"),
                      NOx    = c("NOx", "nitrogen oxides"),
                      SO2    = c("SO2", "sulfur dioxide"),
                      Temp   = c("Temperature", "Temp", "Sample_air temperature")
    ) # Add reference names to be recognized if needed
    
    # Converting reference names
    for (i in seq_along(Ref.names)) {
        for (j in seq_along(Ref.names[[i]])) {
            if (any(Ref.names[[i]][j] == colnames(RefData))) {
                colnames(RefData)[which(Ref.names[[i]][j] == colnames(RefData))] <- names(Ref.names)[i]
            }
            if (any(grepl(pattern = Ref.names[[i]][j], colnames(RefData)))) {
                colnames(RefData)[which(grepl(pattern = Ref.names[[i]][j], colnames(RefData)))] <- sub(pattern = Ref.names[[i]][j], 
                                                                                                                     replacement = names(Ref.names)[i], 
                                                                                                                     x = colnames(RefData)[which(grepl(pattern = Ref.names[[i]][j], colnames(RefData)))] )
            }
        }
    }
    
    #================================================================CR
    # Plotting
    #================================================================CR
    # names of Pollutant to plot
    # if (exists("name.pol")) remove(name.pol)
    # for (i in which(colnames(RefData) %in% Pollutant)) {
    #     if (exists("name.pol")) {
    #         name.pol <- c(name.pol, paste0(colnames(RefData)[i], " in ", Units[i - 1]))
    #     } else {
    #         name.pol <- paste0(colnames(RefData)[i], " in ", Units[i - 1])
    #     }
    # }
    # timePlot(mydata = RefData %>% 
    #              dplyr::filter(date >= start && date <= end) %>% 
    #              dplyr::select(colnames(RefData)[-which(colnames(RefData) %in% c("manifold flow","temperature cpu"))]),
    #          pollutant = colnames(RefData)[colnames(RefData) %in% names(Ref.names)],
    #          date.pad = TRUE,
    #          y.relation = "free",
    #          name.pol = name.pol)
    
    rm(Ref.names)
    return(RefData)
}
a_i_p_data(URL, username, password, organisation, station, start, end, param)
#================================================================CR
# 2) Extract the data from the JSON file ====
#================================================================CR
# extract the data node
Reference <- parsed_content(Reference.JSON.httr, type = "application/json")
# Reference <- jsonlite::fromJSON(Reference,
#                               simplifyDataFrame = TRUE,
#                               flatten = TRUE
#)
# For jsonlite::fromJSON : Reference <- Reference$Stations[[1]]$Devices
# For httr::GET(URLencode(paste0(
Reference <- Reference$Stations[[1]]$Devices

#================================================================CR
# 3) Orient yourself to the data ====
#================================================================CR
# Working with JSON in R can be a bit disorienting because you end up with lists within lists within lists 
# so let's break it down a bit. In this dataset we have an outer list where each list item is an individual 
# air pollutants (you can see this from the sample data above. 

#Reference.JSON[[1]]$Devices[[1]]$Components[[6]]
#Reference.JSON[[1]]$Devices[[1]]$Components[[8]]

#================================================================CR
# 4b) Assemble the data frame: extracting all the variables ====
#================================================================CR
# delelting exiting variables
if (exists("MeasuredValues")) remove(MeasuredValues)
if (exists("Components"))     remove(Components)
if (exists("Units"))          remove(Units)# setting names, units and values

# Creating Measuredvalues
for (i in 1:length(Reference)) {
    
    # determining Component
    if (exists("Components")) {
        Components <- c(Components, Reference[[i]]$Components[[1]]$Component)
    } else {
        Components <- Reference[[i]]$Components[[1]]$Component
    }
    
    # determining Units
    if (exists("Units")) {
        Units <- c(Units, Reference[[i]]$Components[[1]]$Unit)
    } else {
        Units <- Reference[[i]]$Components[[1]]$Unit
    } 
    
    # Collecting values
    if (!exists("MeasuredValues")) {
        for (j in 1:length(Reference[[i]]$Components[[1]]$MeasuredValues)) {
            if (!exists("MeasuredValues")) {
                
                # look for data only if valid
                if (as.logical(Reference[[i]]$Components[[1]]$MeasuredValues[[j]]$Valid)) {
                    MeasuredValues <- data.frame("date" = Reference[[i]]$Components[[1]]$MeasuredValues[[j]]$Time,
                                                 "Value" = as.numeric(Reference[[i]]$Components[[1]]$MeasuredValues[[j]]$Value),
                                                 stringsAsFactors = FALSE
                    )
                }
            } else {
                if (as.logical(Reference[[i]]$Components[[1]]$MeasuredValues[[j]]$Valid)) {
                    MeasuredValues <- rbind(MeasuredValues, 
                                                       data.frame("date" = Reference[[i]]$Components[[1]]$MeasuredValues[[j]]$Time,
                                                                  "Value" = as.numeric(Reference[[i]]$Components[[1]]$MeasuredValues[[j]]$Value),
                                                                  stringsAsFactors = FALSE
                                                        )
                                      )
                }
                
            } 
        }
        # MeasuredValues <- sapply(Reference[[i]]$Components[[1]]$MeasuredValues, 
        #                          function(x) return(c(x$Time, x$Value, x$Valid)))
        colnames(MeasuredValues) <- c("date", Components[i]) # , paste0("Valid.",Components[i])
        #MeasuredValues <- t(MeasuredValues)
        
    } else {

        # Deleting i.MeasuredValues to start a new variable
        if (exists("i.MeasuredValues")) remove(i.MeasuredValues)

        for (j in 1:length(Reference[[i]]$Components[[1]]$MeasuredValues)) {
            if (!exists("i.MeasuredValues")) {
                if (as.logical(Reference[[i]]$Components[[1]]$MeasuredValues[[j]]$Valid)) {
                    i.MeasuredValues <- data.frame("date" = Reference[[i]]$Components[[1]]$MeasuredValues[[j]]$Time,
                                                   Value = as.numeric(Reference[[i]]$Components[[1]]$MeasuredValues[[j]]$Value),
                                                   stringsAsFactors = FALSE
                    )
                }
            } else {
                if (as.logical(Reference[[i]]$Components[[1]]$MeasuredValues[[j]]$Valid)) {
                    i.MeasuredValues <- rbind(i.MeasuredValues, 
                                              data.frame("date" = Reference[[i]]$Components[[1]]$MeasuredValues[[j]]$Time,
                                                         Value = as.numeric(Reference[[i]]$Components[[1]]$MeasuredValues[[j]]$Value),
                                                         stringsAsFactors = FALSE
                                              )
                    )
                }
                
            } 
        }
        # i.MeasuredValues  <- sapply(Reference[[i]]$Components[[1]]$MeasuredValues, 
        #                                function(x) return(data.frame(unlist(x$Time), unlist(x$Value), unlist(x$Valid), stringsAsFactors = FALSE)) )
        colnames(i.MeasuredValues) <- c("date", Components[i]) #, paste0("Valid.",Components[i])
        #i.MeasuredValues <- t(i.MeasuredValues)
        MeasuredValues <- merge(MeasuredValues, i.MeasuredValues, by = "date", all = TRUE )
    }
}

# Deleting unecessary dataFrame
if (exists("i.MeasuredValues"))  remove(i.MeasuredValues)
if (exists("Reference.JSON"))      remove(Reference.JSON)
if (exists("Reference"))           remove(Reference)
if (exists("Reference.JSON.httr")) remove(Reference.JSON.httr)

#================================================================CR
# 5) convert Time to Posix format and rename Time in date for opean air
#================================================================CR
colnames(MeasuredValues)[which(colnames(MeasuredValues) == "Time")] <- "date"
MeasuredValues$date <- ymd_hms(MeasuredValues$date, tz = "UTC")

#================================================================CR
# 6) convert Reference names
#================================================================CR
# list of equivalent reference names
Ref.names <- list(NO2    = c("NO2","nitrogen dioxide"),
                  CO_ppm = c("CO_ppm", "carbon monoxide"),
                  O3     = c("O3", "ozone"),
                  NO     = c("NO", "nitrogen monoxide"),
                  NOx    = c("NOx", "nitrogen oxides"),
                  SO2    = c("SO2", "sulfur dioxide")
) # Add reference names to be recognized if needed
#------------------------------------------------------------------------------CR
# Converting reference names
#------------------------------------------------------------------------------CR
for (i in 1:length(Ref.names)) {
    for (j in 1: length(Ref.names[[i]])) {
        if (any(Ref.names[[i]][j] == colnames(MeasuredValues))) {
            colnames(MeasuredValues)[which(Ref.names[[i]][j] == colnames(MeasuredValues))] <- names(Ref.names)[i]
        }
        if (any(grepl(pattern = Ref.names[[i]][j], colnames(MeasuredValues)))) {
            colnames(MeasuredValues)[which(grepl(pattern = Ref.names[[i]][j], colnames(MeasuredValues)))] <- sub(pattern = Ref.names[[i]][j], 
                                                                                                                 replacement = names(Ref.names)[i], 
                                                                                                                 x = colnames(MeasuredValues)[which(grepl(pattern = Ref.names[[i]][j], colnames(MeasuredValues)))] )
        }
    }
}
remove(Ref.names)

#================================================================CR
# 6) plotting
#================================================================CR
library(openair)

#? Select Pollutant to plot
Pollutant <- colnames(MeasuredValues)[4:length(colnames(MeasuredValues))]
if (exists("name.pol")) remove(name.pol)
for (i in which(colnames(MeasuredValues) %in% Pollutant)) {
    if (exists("name.pol")) {
        name.pol <- c(name.pol, paste0(colnames(MeasuredValues)[i], " in ", Units[i-1]))
    } else {
        name.pol <- paste0(colnames(MeasuredValues)[i], " in ", Units[i-1])
    }
}

# Selected date to plot
DateIN  <- ymd("2018-04-08", tz = "UTC" )
DateEND <- ymd("2018-04-18", tz = "UTC" )

timePlot(mydata = selectByDate(MeasuredValues, start = DateIN, end = DateEND),
         pollutant = Pollutant,
         date.pad = TRUE,
         y.relation = "free",
         name.pol = name.pol)


