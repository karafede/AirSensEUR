rm(list = ls())

#------------------------------------C
# libraries
#------------------------------------C
library(tidyverse)
library(parallel)
library(zoo)
library(xts)
library(rChoiceDialogs)

#------------------------------------C
# reading data
#------------------------------------C
General.Rdata.File <- "D:\\Profil_Michel\\Desktop\\Bureau\\Diffusion\\Box Sync\\AirSensEUR\\Fieldtests\\Shiny\\NL_01min\\General_data\\General.Rdata"
if(file.exists(General.Rdata.File)) load(General.Rdata.File) else load(rChoiceDialogs::jchoose.files(default = getwd(),
                                                                                                     caption = "Select General Rdata files", 
                                                                                                     multi = FALSE,
                                                                                                     filters = "*.Rdata", 
                                                                                                     modal = canUseJavaModal()))
list.gas.sensors <- c("Carbon_monoxide", "Nitric_oxide","Nitrogen_dioxide", "Ozone")

#------------------------------------C
# Functions to remove outliers
#------------------------------------C
My.rm.Outliers <- function( date, y,  window, threshold, 
                            ymin = NULL, ymax = NULL, ThresholdMin = NULL, Sensor.name = NULL) {
    ### Extracting index of the values outside [ymin, ymax] and the outliers tolerance with rollapply (zmax and zmin together)
    
    # date                  = Vector of POSIXCt giving x axis, date values in Posixct class
    # y                     = name of column of General.df with time series on which ouliers are detected
    # window                = width of the window used to compute median and average in number of data
    # threshold             = coefficient that muliplied by the difference between median and average that is exceeded result in outlier
    # ymin                  = minimum values for y, for example to remove negative values
    # ymax                  = maximum values for y, for example to limit highest values
    # ThresholdMin          = minimum values for zmin, the minimum values that evidence outliers when exceeded
    # Sensor.name           = Sensor name currently being scanned for outliers
    # utmaxmin              = External function computing the Median Average Deviation     
    
    # Return                = dataframe with date, then logical values Low_values (<min), High_values(>ymax), 
    #                         OutliersMin(< Median - threshold * MAD, OutliersMax (> Median + threshold *MAD, 
    #                         and values zmin (Median - threshold * MAD) and zmax (Median + threshold * MAD)
    
    if (!is.null(Sensor.name)) print(paste0("[My.rm.Outliers] INFO, sensor: ",Sensor.name),quote = FALSE)
    
    # Removing values lower than ymin
    if (!is.null(ymin)) Low_values  <- (y < ymin) else Low_values  <- rep(FALSE, length(y))
    if (!is.null(ymax)) High_values <- (y > ymax) else High_values <- rep(FALSE, length(y))
    
    # max and max limits
    zmax.zmin <- zoo::rollapply(zoo(y), width = window, FUN = utmaxmin, threshold = threshold, align = "center", partial = TRUE)
    
    #zmax <- c(rep(zmax[1], window-1), zmax) # Use z[1] throughout the initial period. # Not needed if align center and partial = TRUE
    OutliersMax <- y > zmax.zmin[,1]
    
    # Changing the low values of the minimum of the interval of tolerance by ThresholdMin
    Index.Lower <- which(zmax.zmin[,2] < ThresholdMin)
    if (!is.null(ThresholdMin) && !is.na(ThresholdMin)) {zmax.zmin[Index.Lower,2] <- rep(ThresholdMin, length.out = length(Index.Lower))}
    
    OutliersMin <- y < zmax.zmin[,2]
    
    # data frame to return
    df <- data.frame(date = date, 
                     Low_values  = Low_values, 
                     High_values = High_values, 
                     OutliersMin = OutliersMin, 
                     OutliersMax = OutliersMax, 
                     zmin        = zmax.zmin[,2], 
                     zmax        = zmax.zmin[,1]) 
    return(df)
}
utmaxmin <- function(y, threshold = 1) {
    # y            : vector of numeric (NA possible) on which to calculate Median - threshold * MAD amd Median + threshold * MAD
    #                where MAD is the Median Absolute Deviation i. e. median of the absolute deviations from the median,
    # Threshold    : numeric, default 1scale factor.
    # Return a numeric vector c(Median - threshold * cMedian(abs(y - Median(y))), Median + threshold * cMedian(abs(y - Median(y))) )
    Median <- stats::median(y, na.rm = TRUE)
    MAD    <- stats::mad(y, constant = threshold, na.rm = TRUE)
    return(c(Median + MAD, Median - MAD))
}

#------------------------------------C
# loop for
#------------------------------------C
system.time(for (i in list.gas.sensors) {
    
    Outli <- My.rm.Outliers(ymin         = NULL,
                            ymax         = NULL,
                            ThresholdMin = NULL,
                            date         = General.df$date,
                            y            = General.df[,paste0("Out.Warm.TRh.Inv.",i)],
                            window       = 19,
                            threshold    = 14,
                            Sensor.name  = i)
    # nameInd      <- paste0(i)
    # OutlinameInd <- paste0(i,".Outli")
    # assign(nameInd , data.frame(date = Outli$date, Outliers = apply(Outli[,c("Low_values","High_values","OutliersMin","OutliersMax")], 1, any), stringsAsFactors = FALSE))
    # #if (length(get(nameInd))==0) break # stopping if there are no outliers in the current iteration
    # if (exists("return.ind.sens.out")) return.ind.sens.out[[nameInd]] <- get(nameInd) else {
    #     return.ind.sens.out <- list(get(nameInd)); names(return.ind.sens.out) <- nameInd
    # }
    # return.ind.sens.out[[OutlinameInd]] <- Outli
})
#    user  system elapsed 
#  204.62    0.81  205.58  

#------------------------------------C
# loop purr::map and lapply
#------------------------------------C
system.time(Outliers <- purrr::map(as.list(General.df[,list.gas.sensors]),~ My.rm.Outliers(ymin         = NULL,
                                                                                           ymax         = NULL,
                                                                                           ThresholdMin = NULL,
                                                                                           date         = General.df$date,
                                                                                           y            = .x,
                                                                                           window       = 19,
                                                                                           threshold    = 14,
                                                                                           Sensor.name  = names(.x))))
# user  system elapsed 
#218.20    0.62  219.31  

system.time(Outliers <-lapply(as.list(General.df[,list.gas.sensors]), My.rm.Outliers, date = General.df$date, window = 19, threshold = 14))
# user  system elapsed 
# 207.71    0.48  208.77  

#------------------------------------C
# loop parallel computing
#------------------------------------C
no_cores <- detectCores()-1
c1 <- makeCluster(no_cores)
clusterEvalQ(c1, {
    
    library(zoo)
    utmaxmin <- function(y, threshold = 1) {
        # y            : vector of numeric (NA possible) on whcih to calculate Median - threshold * MAD amd Median + threshold * MAD
        #                where MAD is the Median Absolute Deviation i. e. median of the absolute deviations from the median,
        # Threshold    : numeric, default 1scale factor.
        # Return a numeric vector c(Median - threshold * Median(abs(y - Median(y))), Median + threshold * Median(abs(y - Median(y))) )
        
        browser()
        Median <- stats::median(y, na.rm = TRUE)
        MAD    <- stats::mad(y, constant = threshold, na.rm = TRUE)
        return(c(Median - MAD, Median + MAD))
    }
    
    My.rm.Outliers <- function( date = NULL, y,  window, threshold, 
                                ymin = NULL, ymax = NULL, ThresholdMin = NULL, Sensor.name = NULL) {
        ### Extracting index of the values outside [ymin, ymax] and the outliers tolerance with rollapply (zmax and zmin together)
        
        # date                  = Vector of POSIXCt giving x axis, date values in Posixct class or NULL if y is of class zoo
        # y                     = zoo object or numerical vector with time series on which ouliers are detected
        # window                = width of the window used to compute median and average in number of data
        # threshold             = coefficient that muliplied by the difference between median and average that is exceeded result in outlier
        # ymin                  = minimum values for y, for example to remove negative values
        # ymax                  = maximum values for y, for example to limit highest values
        # ThresholdMin          = minimum values for zmin, the minimum values that evidence outliers when exceeded
        # Sensor.name           = Sensor name currently being scanned for outliers
        # utmaxmin              = External function computing the Median Average Deviation     
        
        # Return                = dataframe with date, then logical values Low_values (<min), High_values(>ymax), 
        #                         OutliersMin(< Median - threshold * MAD, OutliersMax (> Median + threshold *MAD, 
        #                         and values zmin (Median - threshold * MAD) and zmax (Median + threshold * MAD)
        
        browser()
        if (!is.null(Sensor.name)) print(paste0("[My.rm.Outliers] INFO, sensor: ",Sensor.name),quote = FALSE)
        
        # Removing values lower than ymin
        if (!is.null(ymin)) Low_values  <- (y < ymin) else Low_values  <- rep(FALSE, length(y))
        if (!is.null(ymax)) High_values <- (y > ymax) else High_values <- rep(FALSE, length(y))
        
        # max and max limits
        if (class(y) != "zoo") y = zoo(y, order.by = date)
        zmax.zmin <- zoo::rollapply(y, width = window, FUN = utmaxmin, threshold = threshold, align = "center", partial = TRUE)
        
        #zmax <- c(rep(zmax[1], window-1), zmax) # Use z[1] throughout the initial period. # Not needed if align center and partial = TRUE
        OutliersMax <- y > zmax.zmin[,1]
        
        # Changing the low values of the minimum of the interval of tolerance by ThresholdMin
        Index.Lower <- which(zmax.zmin[,2] < ThresholdMin)
        if (!is.null(ThresholdMin) && !is.na(ThresholdMin)) {zmax.zmin[Index.Lower,2] <- rep(ThresholdMin, length.out = length(Index.Lower))}
        
        OutliersMin <- y < zmax.zmin[,2]
        
        # data frame to return
        df <- data.frame(date = index(y), 
                         Low_values  = Low_values, 
                         High_values = High_values, 
                         OutliersMin = OutliersMin, 
                         OutliersMax = OutliersMax, 
                         zmin        = zmax.zmin[,2], 
                         zmax        = zmax.zmin[,1]) 
        return(df)
    }
    
    roll <- function(y, General.df) {
        zoo::rollapply(zoo(General.df[,y]), width = 19, FUN = utmaxmin, threshold = 14, align = "center", partial = TRUE)
    }
})
tm0 <- system.time(
    Outliers <- parallel::parLapply(c1, as.list(General.df[,list.gas.sensors]), My.rm.Outliers, date = General.df$date, window = 19, threshold = 14)
    
)
# user  system elapsed 
# 0.50    0.62   67.83

# Divide in chunks
Chunk_List <- lapply(seq(no_cores), function (x) {
    Chunk <- as.list(zoo(General.df[((x-1) * nrow(General.df)/no_cores): (x * nrow(General.df)/no_cores), list.gas.sensors],
                         order.by = General.df[((x-1) * nrow(General.df)/no_cores): (x * nrow(General.df)/no_cores), "date"]))
    names(Chunk) <- paste0(names(Chunk), ".",x)
    return(Chunk)
})
# > str(Chunk_List)
# List of 7
# $ :List of 4
# ..$ Carbon_monoxide.1 : num [1:54060] NaN NA NA NA NA NA NA NA NA NA ...
# ..$ Nitric_oxide.1    : num [1:54060] NaN NA NA NA NA NA NA NA NA NA ...
# ..$ Nitrogen_dioxide.1: num [1:54060] NaN NA NA NA NA NA NA NA NA NA ...
# ..$ Ozone.1           : num [1:54060] NaN NA NA NA NA NA NA NA NA NA ...
# $ :List of 4
# ..$ Carbon_monoxide.2 : num [1:54061] NA NA NA NA NA NA NA NA NA NA ...
# ..$ Nitric_oxide.2    : num [1:54061] NA NA NA NA NA NA NA NA NA NA ...
for (x in 2:no_cores) Chunk_List[[1]][names(Chunk_List[[x]])] <- Chunk_List[[x]]
Chunk_List <- Chunk_List[[1]]

tm1 <- system.time(
    Outliers <- parallel::parLapply(c1, Chunk_List, My.rm.Outliers, date = NULL, window = 19, threshold = 14)
)
# user  system elapsed 
# 0.81    0.67  155.97
stopCluster(c1)
names(Outliers) <- paste0(names(Outliers), ".1.Outli")
assign(nameInd , data.frame(date = Outli$date, Outliers = apply(Outli[,c("Low_values","High_values","OutliersMin","OutliersMax")], 1, any), stringsAsFactors = FALSE))


