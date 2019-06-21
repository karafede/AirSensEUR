#================================================================CR
#  0) Configuring Proxy server
#================================================================CR
# At JRC we have now a proxy that block downlaoding from github, using the following code, we can download from Github
### Shiny input  Window 1: PROXY, url, port, login and password ###
PROXY = TRUE     
URL      = "10.168.209.72"; PORT     = 8012; LOGIN    = NULL; PASSWORD = NULL
# no login and no password on our proxy 
### END of Shiny input ###
if(PROXY){
    
    # checking that we have the httr package to use function Set_Config()
    if("httr" %in% rownames(installed.packages()) == FALSE) {
        cat("[ASEConfig] INFO Installing httr", sep = "\n")
        install.packages("httr")
    } else cat("[ASEConfig] INFO Package httr already installed", sep = "\n")
    require("httr")
    cat("[ASEConfig] INFO Package httr loaded", sep = "\n")
    
    # implement PROXY 
    if(is.null(LOGIN)) set_config(use_proxy(url=URL, port=PORT)) else set_config( use_proxy(url=URL, port=PORT, username    = LOGIN, password = PASSWORD))
    
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
# from the website
username     <- "datareq"
password     <- "aip2jrc"
organisation <- "ABCIS"
station      <- "JRC+Ispra"
start        <- "2018-03-01-14-30-00"
end          <- "2018-03-03-15-30-00"
# General.JSON <-jsonlite::fromJSON(txt = paste0("https://ubis-air.a-i-p.com/UidepService/values/simple?",
#                                 "username=", username,"&",
#                                 "password=", password,"&",
#                                 "organisation=", organisation,"&",
#                                 "station=", station,"&",
#                                 "start=", start,"&",
#                                 "end=", end),
#                                 simplifyDataFrame = TRUE,
#                                 flatten = TRUE
# )
General.JSON.httr <- httr::GET(URLencode(paste0("https://ubis-air.a-i-p.com/UidepService/values/simple?",
                           "username=", username,"&",
                           "password=", password,"&",
                           "organisation=", organisation,"&",
                           "station=", station,"&",
                           "start=", start,"&",
                           "end=", end)
                    )
)

#================================================================CR
# 2) Extract the data from the JSON file ====
#================================================================CR
# extract the data node
General <- parsed_content(General.JSON.httr, type = "application/json")
# General <- jsonlite::fromJSON(General,
#                               simplifyDataFrame = TRUE,
#                               flatten = TRUE
#)
# For jsonlite::fromJSON : General <- General$Stations[[1]]$Devices
# For httr::GET(URLencode(paste0(
General <- General$Stations[[1]]$Devices

#================================================================CR
# 3) Orient yourself to the data ====
#================================================================CR
# Working with JSON in R can be a bit disorienting because you end up with lists within lists within lists 
# so let's break it down a bit. In this dataset we have an outer list where each list item is an individual 
# air pollutants (you can see this from the sample data above. 

#General.JSON[[1]]$Devices[[1]]$Components[[6]]
#General.JSON[[1]]$Devices[[1]]$Components[[8]]

#================================================================CR
# 4b) Assemble the data frame: extracting all the variables ====
#================================================================CR
if(exists("MeasuredValues")) remove(MeasuredValues)
if(exists("Components")) remove(Components)
if(exists("Units")) remove(Units)# setting names, units and values
for(i in 1:length(General)){
    if(exists("Components")){
        Components <- c(Components, General[[i]]$Components[[1]]$Component)
    } else {
        Components <- General[[i]]$Components[[1]]$Component
    } 
    if(exists("Units")){
        Units <- c(Units, General[[i]]$Components[[1]]$Unit)
    } else {
        Units <- General[[i]]$Components[[1]]$Unit
    } 
    if(!exists("MeasuredValues")) {
        for(j in 1:length(General[[i]]$Components[[1]]$MeasuredValues)){
            if(!exists("MeasuredValues")){
                # look for data only if valid
                if(as.logical(General[[i]]$Components[[1]]$MeasuredValues[[j]]$Valid)){
                    MeasuredValues <- c( General[[i]]$Components[[1]]$MeasuredValues[[j]]$Time,
                                         as.numeric(General[[i]]$Components[[1]]$MeasuredValues[[j]]$Value),
                                         as.logical(General[[i]]$Components[[1]]$MeasuredValues[[j]]$Valid))
                }
            } else {
                if(as.logical(General[[i]]$Components[[1]]$MeasuredValues[[j]]$Valid)){
                    MeasuredValues <- rbind(MeasuredValues, c( General[[i]]$Components[[1]]$MeasuredValues[[j]]$Time,
                                                               as.numeric(General[[i]]$Components[[1]]$MeasuredValues[[j]]$Value),
                                                               as.logical(General[[i]]$Components[[1]]$MeasuredValues[[j]]$Valid)))
                }
                
            } 
        }
        # MeasuredValues <- sapply(General[[i]]$Components[[1]]$MeasuredValues, 
        #                          function(x) return(c(x$Time, x$Value, x$Valid)))
        colnames(MeasuredValues) <- c("Time", Components[i], paste0("Valid.",Components[i]))
        #MeasuredValues <- t(MeasuredValues)
    } else {
        for(j in 1:length(General[[i]]$Components[[1]]$MeasuredValues)){
            if(!exists("i.MeasuredValues")){
                if(as.logical(General[[i]]$Components[[1]]$MeasuredValues[[j]]$Valid)){
                    i.MeasuredValues <- c( General[[i]]$Components[[1]]$MeasuredValues[[j]]$Time,
                                           as.numeric(General[[i]]$Components[[1]]$MeasuredValues[[j]]$Value),
                                           as.logical(General[[i]]$Components[[1]]$MeasuredValues[[j]]$Valid))
                }
            } else {
                if(as.logical(General[[i]]$Components[[1]]$MeasuredValues[[j]]$Valid)){
                    i.MeasuredValues <- rbind(i.MeasuredValues, c( General[[i]]$Components[[1]]$MeasuredValues[[j]]$Time,
                                                                   as.numeric(General[[i]]$Components[[1]]$MeasuredValues[[j]]$Value),
                                                                   as.logical(General[[i]]$Components[[1]]$MeasuredValues[[j]]$Valid))
                    )
                }
                
            } 
        }
        # i.MeasuredValues  <- sapply(General[[i]]$Components[[1]]$MeasuredValues, 
        #                                function(x) return(data.frame(unlist(x$Time), unlist(x$Value), unlist(x$Valid), stringsAsFactors = FALSE)) )
        colnames(i.MeasuredValues) <- c("Time", Components[i], paste0("Valid.",Components[i]))
        #i.MeasuredValues <- t(i.MeasuredValues)
        MeasuredValues <- merge(MeasuredValues, i.MeasuredValues, by = "Time" )
    }
}
remove(i.MeasuredValues)
remove(General, General.JSON,General.JSON.httr)

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
# Convertinng reference names
#------------------------------------------------------------------------------CR
for(i in 1:length(Ref.names)) {
    for(j in 1: length(Ref.names[[i]])) {
        if(any(Ref.names[[i]][j] == colnames(MeasuredValues))){
            colnames(MeasuredValues)[which(Ref.names[[i]][j] == colnames(MeasuredValues))] <- names(Ref.names)[i]
        }
        if(any(grepl(pattern = Ref.names[[i]][j], colnames(MeasuredValues)))){
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
remove(name.pol)
for(i in 1:length(colnames(MeasuredValues)[c(6, 8,10,12,14,16)])){
    if(exists("name.pol")){
        name.pol <- c(name.pol, paste0(colnames(MeasuredValues)[c(6, 8,10,12,14,16)][i], " in ", Units[3+i]))
    } else {
        name.pol <- paste0(colnames(MeasuredValues)[c(6, 8,10,12,14,16)][i], " in ", Units[3+i])
    }
} 
timePlot(mydata = MeasuredValues, 
         pollutant = colnames(MeasuredValues)[c(6, 8,10,12,14,16)],
         date.pad = TRUE,
         y.relation = "free",
         name.pol = name.pol)


