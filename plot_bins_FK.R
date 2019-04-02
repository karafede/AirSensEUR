
rm(list = ls())

library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(broom)
library(minpack.lm)
library(mgcv)
library(threadr)
library(dygraphs)
library(ggpmisc)
library(plotly)
library(GGally)
library(htmlwidgets)
library(htmltools)
library(webshot)



# saving the original graphical parameters
op <- par(no.readonly = TRUE)
# Restoring graphical parameters on exit of function, even if an error occurs
# on.exit(par(op)) # it reset the par(mfrow) allways plotting on the upper left plot
# par(mfrow=c(2,2))

# load bin counts from OPC-N3
# setwd
# WD <- "C:/JRC_CA/AirSense/Shiny"
WD <- "L:/ERLAP/Diffusion/Box Sync/AirSensEUR/Fieldtests/Shiny"
setwd(paste0(WD,"/JRC_11/General_data"))
# setwd(choose.dir())
# load fitting functions repository
source(paste0(WD,"/151016 Sensor_Toolbox.R"))
source(paste0(WD,"/Functions4ASE_FK.R"))
# source(choose.files())
output_folder <- paste0(WD,"/JRC_11/Retrieved_plots/")

# read Bin names
bin_file <- read.table(paste0(WD,"/11022019_OPCN3_data.csv"),
                      header = T, skip = 6, sep=",", nrows = 5)
# remove columns with NA values
bin_file <- bin_file[, colSums(is.na(bin_file)) != nrow(bin_file)]

bins_diameters <- data.frame(bin_file[2,])[-1]
bins_volume <- data.frame(bin_file[4,])[-1]  # volume is in um3
bins_volume <- bins_volume[,colSums(is.na(bins_volume))<nrow(bins_volume)]
bins_weight <- data.frame(bin_file[5,])[-1]  # weigthing factor assigned to each Bin
bins_weight <- bins_weight[,colSums(is.na(bins_weight))<nrow(bins_weight)]

# rename upper boundary column (Bin23 (upper boundary))
names(bins_diameters) <- paste0("Bin", seq(1:length(names(bins_diameters)))-1)
names(bins_volume) <- paste0("Bin", seq(1:length(names(bins_volume)))-1)
names(bins_weight) <- paste0("Bin", seq(1:length(names(bins_weight)))-1)

#########################################
# load DMPS and APS data ################
#########################################
# The counts of the OPC-N3 and PM10, PM2.5 ..., temperature and humidity are in General.Rdata.
# When loaded, it creates a dataFrame: General.df with all AirSensEUR sensor data
load("General.Rdata")

# The reference counts of the DMPS and APS are in RefData.Rdata
# It creates a dataframe RefData when loaded with all reference data
load("RefData.Rdata")
RefData <- RefData %>%
    filter(date %in% General.df$date)

#########################################
## Quick dynamic time series ############
#########################################
# Build timeseries for plots
time_series_sensor <- data_frame_to_timeseries(General.df %>%
                                                   select(date,
                                                          Particulate_Matter_1,
                                                          Particulate_Matter_25,
                                                          Particulate_Matter_10))

ts_sensor <- cbind(time_series_sensor$Particulate_Matter_25, 
                      time_series_sensor$Particulate_Matter_10,
                      time_series_sensor$Particulate_Matter_1)
names(ts_sensor) <- c("time_series_sensor.Particulate_Matter_25",
                      "time_series_sensor.Particulate_Matter_10",
                      "time_series_sensor.Particulate_Matter_1")

plot_sensors <- dygraph(ts_sensor) %>% 
    dygraphs::dySeries("time_series_sensor.Particulate_Matter_25",label = "PM2.5", color = "red") %>%
    dygraphs::dySeries("time_series_sensor.Particulate_Matter_10",label = "PM10", color = "blue") %>%
    dygraphs::dySeries("time_series_sensor.Particulate_Matter_1",label = "PM1", color = "black") %>%
    dyAxis("y", label = "PM<sub></sub> (&#956;g m<sup>-3</sup>)") %>% 
    dyRangeSelector()
plot_sensors


#############################################################################
### Fitting the overlap between DMPS and APS darta to find density value ####
#############################################################################

### input parameters to be inserted by the USER
# Start and end date for selection of reference data
Start_Time <- as.POSIXct("2018-09-30 01:00:01", tz = "UTC") #"2018-09-30 01:00"
Stop_Time   <- as.POSIXct("2018-10-07 01:00:01", tz = "UTC") # "2018-10-07 01:00"

#### TO BE integrated in the Function4ASE.R ################################

Final_function_OPC <- function(Begin, End, 
                               RefData = RefData, General.df = General.df, Sensor_dates = NULL, 
                               verbose = TRUE, Min_APS = 0.62, Max_APS =  0.800) {
    
    # Refdata dataframe with timestamp and reference counts
    # DateBegin,   The start and ending selected dates in POSIXCt format. Default values are NULL, 
    # DateEnd      It is possible to have only one date limit, DateBegin or DateEnd. DateBegin or DateEnd are not discarded.
    # Sensor_dates  vector of POSIXCt: the dates for which the PM sensor provides binned counts. 
    #               In case the sensor PM data series is not complete, dates with missing PM sensor data will be discared from RefData
    
    # Min_APS,      Numeric, Minimum and maximum diameters of reference counts to be selected 
    # Max_APS       for reference counts. Default values are NULL. It is possible to have only one diameter limit,  Min_APS or Max_APS.
    #               Min_APS and Max_APS are discarded.
    
    # gather data in list and dataframe
    # Compute mean of reference count
    # browser()
    Dist_Ref.d_TS <- Distribution_Ref_TS(RefData = RefData, Begin, End, 
                                         Min_APS = Min_APS, Max_APS = Max_APS) #
    
    # Plot of distribution of reference counts versus diameter in log/log
    # lognormal distribution (plot)
    (Plot_Dist_Ref_log(Dist_Ref.d_TS$counts_Ref))
    # normal distribution (plot)
    (Plot_Dist_Ref(Dist_Ref.d_TS$counts_Ref))
    
    # Determining of density with 5 last DMPS diameter, and 3rd to 6th APS counts
    # Diameters overlapping of DMPS and APS
    
    
    if (!is.character(Dist_Ref.d_TS)) Diam_APS_to_keep  <- Dist_Ref.d_TS$Diam_APS else stop("Reference data is empty!") 
    Diam_DMPS_to_keep <- Dist_Ref.d_TS$Diam_DMPS[(length(Dist_Ref.d_TS$Diam_DMPS)-3):length(Dist_Ref.d_TS$Diam_DMPS)]
    
    DataXY.DMPS <- Dist_Ref.d_TS$counts_Ref %>% filter(diameters %in% Diam_DMPS_to_keep)
    DataXY.DMPS <- data.frame(x = log10(DataXY.DMPS$diameters),
                              y = log10(DataXY.DMPS$counts))
    Model <- lm(y ~ x, data = DataXY.DMPS, model = TRUE, x = TRUE, y = TRUE)
    Model.i <- list(Tidy = tidy(Model), 
                    Augment = augment(Model), 
                    Glance = glance(Model), 
                    Call = Model$call, Coef = coef(Model),
                    Model = Model)
    
    DataXY.APS <- Dist_Ref.d_TS$counts_Ref %>% filter(diameters %in% Diam_APS_to_keep)
    DataXY.APS <- data.frame(x = log10(DataXY.APS$diameters),
                             y = log10(DataXY.APS$counts))
    
    # find optimal value for the "density" of the particles
    # return the minimum difference between modeled data and raw data (density_fun$minimum)
    density_fun <- optimise(f_Error, c(0.3, 3), DataXY.APS = DataXY.APS, Model.i = Model.i) # limit ranges for the density
    if (verbose) {
        
        # fitted DMPS reference data
        Plot.DMPS       <- Plot_Dist_Ref(Dist_Ref.d_TS$counts_Ref %>% filter(diameters %in% Diam_DMPS_to_keep), Model.i = Model.i)
        # add raw APS reference data
        Plot.DMPS.APS   <- Plot.DMPS + geom_point(data = DataXY.APS, 
                                                      aes(x, y), stat = "identity", col = "red")
        # raw data + minimized data (APS)
        Plot.DMPS.APS.d <- Plot.DMPS.APS + geom_point(data = DataXY.APS, 
                                                  aes(log10(10^x/density_fun$minimum), y), stat = "identity", shape = 17, col = "red", size = 3)
        # plot 
        (Plot.DMPS.APS.d)
    }
    

    ############################################################################################################################################ 
    ####### make difference MinAPS/MaxAPS for density determination and for fitting
    ############################################################################################################################################
    ### we have the correct density in Model.i and the correct diameters of APS in Dist_ref
    # return a list with "RefData_filtered" "counts_Ref"   "Diam_DMPS"         "Diam_APS" with the full diameters
    Dist_Ref.full <- Distribution_Ref_TS(RefData = RefData, DateBegin = Start_Time, DateEnd = Stop_Time, 
                                         Min_APS = 0.777, Max_APS =  14.86000) #
    
    ## correction of APS diamters with density in Dist_Ref.d determined in Model.i  <- Density_Ref_log
    Dist_Ref.full$counts_Ref$diameters <- Diam_APS_Corr(Dist_Ref.full$Diam_APS, Dist_Ref.full$counts_Ref$diameters, density_fun)
    # make a plot of Reference distribution of Particle COUNTS by diameters (averaged)
    if (verbose) (Plot_Dist_Ref_log(Dist_Ref.full$counts_Ref))
    
    # browser()
    # calculate VOLUME in each Bin for APS
    # retrieve names of the APS diameters (this is a vector of names)
    names.Diam_APS <- as.character(Dist_Ref.full$Diam_APS )
   
    # define empty matrix
    Volume_APS <- matrix(rep(0, nrow(Dist_Ref.full$RefData_filtered) * length(names.Diam_APS)), 
                         nrow = nrow(Dist_Ref.full$RefData_filtered),
                         ncol = length(names.Diam_APS))
    colnames(Volume_APS) <- names.Diam_APS

    density <- density_fun$minimum
    # calculate the volume (in ml) for each bin (diameter of the APS data) 
    # APS diameters are corrected with the density calculated within the selected time-range
    Volume_APS <- sapply(names.Diam_APS, f_Volume_APS, Dist_Ref.full = Dist_Ref.full, density = density)
    
    
    # calculate only Volume_APS up to 2.5 um 
    names.Diam_APS_PM25 <- as.character( as.numeric(names.Diam_APS)[which (as.numeric(names.Diam_APS) <= 2.5 )])
    Volume_APS_PM25 <- Volume_APS[, names.Diam_APS_PM25]
    
    # calculate VOLUME in each Bin for DMPS
    # retrieve names of the DMPS diameters (this is a vector of names)
    names.Diam_DMPS <- as.character(Dist_Ref.full$Diam_DMPS )
    
    # define empty matrix
    Volume_DMPS <- matrix(rep(0, nrow(Dist_Ref.full$RefData_filtered) *  length(names.Diam_DMPS)), 
                         nrow = nrow(Dist_Ref.full$RefData_filtered),
                         ncol = length(names.Diam_DMPS))
    colnames(Volume_DMPS) <- names.Diam_DMPS
    
    # calculate the volume (in ml) for each bin (diameter of the APS data)
    Volume_DMPS <- sapply(names.Diam_DMPS, f_Volume_DMPS, Dist_Ref.full = Dist_Ref.full)
    
    # put Volume DMPS together with DMPS volume (um3/cm3) for ALL diameters size
    Volume_DMPS_APS <- cbind(Volume_DMPS, Volume_APS)
    
    # calculate Volume DMPS + APS for up 2.5 um
    Volume_DMPS_APS_PM25 <- cbind(Volume_DMPS, Volume_APS_PM25)
    
    # make means of all Volumes over the time 
    Volume_Ref <- colMeans(Volume_DMPS_APS, na.rm = T)
    Volume_Ref <- as.data.frame((Volume_Ref))
    Volume_Ref$diameters <- as.numeric(rownames(Volume_Ref))
    rownames(Volume_Ref) <- NULL
    names(Volume_Ref) <- c("volume", "diameters")
    Volume_Ref <- Volume_Ref %>%
        dplyr::filter(volume > 0)
    
    
    # browser()
    
    # calculate reference PM2.5 (mass) according to the "Validated_Annual daily averages" formula for 2018 (JRC-EMEP)
    Volume_PM25_ref <- as.data.frame(cbind(Dist_Ref.full$RefData_filtered$date, rowSums(Volume_DMPS_APS_PM25, na.rm = T)))
    names(Volume_PM25_ref) <- c("date", "Volume_PM25_ref")
    Volume_PM25_ref$date <-  as.POSIXct(Volume_PM25_ref$date , tz = "UTC", origin = strptime("1970-01-01", "%Y-%m-%d"))
    Volume_PM25_ref$MASS_PM25_ref <- Volume_PM25_ref$Volume_PM25_ref*1.1 - 0.93

    # browser()
    # Fitting of distribution of reference counts versus diameter in log/log with fitted density (using corrected diameters)
    Model.i.Gam  <- Density_Ref_log(Dist_Ref.full$counts_Ref, Density = NULL, Mod_type = 'GAM_GAUSS' )
    Model.i.Vol  <- Density_Vol_Ref_log(Volume_Ref, Density = NULL, Mod_type = 'GAM_GAUSS' )
    
    # calculate the distribution of time-averaged data for OPC counts and Volume without using the Growing factor (K --> Kohler coefficient = NULL )
    Dist_OPC_Sensor_wet <- Distribution_OPC_Sensor(General.df = General.df, DateBegin = Start_Time, DateEnd = Stop_Time, 
                                               bins_diameters = bins_diameters, K = NULL) 
    

    # Correction of OPS diameter with density
    # use of Gam distribution to predict the reference counts at the OPC diameters
    # return predicted values of the OPC
    Predict_Dist_OPC_Sensor <- Density_OPC_predict(Dist_OPC_Sensor_wet$counts_OPC, Mod_type = 'GAM_GAUSS', 
                                                   density = density_fun$minimum, Model.i  = Model.i.Gam)
    
    Predict_Dist_OPC_Volume <- Density_Vol_predict(Dist_OPC_Sensor_wet$Volume_OPC, Mod_type = 'GAM_GAUSS',
                                                   density = NULL, Model.i  = Model.i.Vol)
    

    # try finding a fitted value for K in the Growing factor formula
    # error function for the difference between raw OPC particles Volume and their predicted values
    
f_Error_Vol <- function(K, Dist_OPC_Sensor_wet, Model.i.Vol){
    
    DataXY <- data.frame(x = log10(Dist_OPC_Sensor_wet$Volume_OPC$diameters),
                        y = log10(Dist_OPC_Sensor_wet$Volume_OPC$volume))

     RH <- Dist_OPC_Sensor_wet$Met_OPC$Hum
     sampling_period <- mean(Dist_OPC_Sensor_wet$sampling_period, na.rm = T)
     ## !!!! Bin_mean_wet
     Bin_mean_dry <- (Dist_OPC_Sensor_wet$Volume_OPC_mean$mean_diameters)/ ( ( 1 + K* (RH/ (100-RH) ) )^(1/3))
     bins_vol_dry <- round((Bin_mean_dry^3)*(pi/6), digits =3)
     # compute the "Dry" Volume of each particle sampled in each Bin 
     V_OPC_dry_K <- ( (Dist_OPC_Sensor_wet$counts_OPC$counts) / sampling_period) * bins_vol_dry * Dist_OPC_Sensor_wet$Volume_OPC$weight
     
     # difference between "dry" volume (depending from K) and the predicted Volume (at the ref value...that is also dry volume)
     return(sum(V_OPC_dry_K - 10^predict(Model.i.Vol$Model, DataXY) )^2)
}
    
    # browser()
    # optimizing function to find the K value (Khoeler coefficient) for to be used in the Growing Factor (GF)
    Kohler_fun <- optimize(f_Error_Vol, c(0.3, 0.6), Dist_OPC_Sensor = Dist_OPC_Sensor_wet, Model.i.Vol = Model.i.Vol) 

    # calculate the distribution of time-averaged data for OPC counts and Volume using the Growing factor (K = Kohler coeff. --> Kohler_fun$minimum )
    Dist_OPC_Sensor_dry <- Distribution_OPC_Sensor(General.df = General.df, DateBegin = Start_Time, DateEnd = Stop_Time, 
                                               bins_diameters = bins_diameters, K = Kohler_fun$minimum) 
    
 
    ## Plot of distribution of OPC Sensor counts versus diameter in dNlog/dlog_Dp
    if (verbose) print(Plot_Distribution_OPC_Sensor(Dist_OPC_Sensor_wet$counts_OPC, Dist_Ref.full$counts_Ref, Predict_Dist_OPC_Sensor,
                                                    DateBegin = Start_Time, DateEnd = Stop_Time))

    ## Plot of WET distribution of the Volume of OPC, DMPS and APS versus diameter in log/log
    if (verbose) print(Plot_Dist_Vol_log(Volume_Ref, Volume_OPC = Dist_OPC_Sensor_dry$Volume_OPC, Predict_Dist_OPC_Volume,
                                         DateBegin = Start_Time, DateEnd = Stop_Time))

    # Put together timeInterval, density, predict of reference counts at OPC corrected diameters, OPC counts,  OPCHUM, Temp, VOL, sampling rate
    
    # find Bin assignation for each diameter according to the Max APS diameter
    Bins <- as.data.frame(t(bins_diameters))
    Bins$Bins <- as.character(rownames(Bins))
    rownames(Bins) <- NULL
    names(Bins) <- c("diameters", "Bins")
    # Bins <- Bins %>%
    #     filter(diameters < max(Dist_Ref.full$Diam_APS, na.rm = T))
    
    # Determine the Bins used by the OPC and mantain their order (missing Bins will be replaced by NA)
    # Dist_OPC_Sensor$counts_OPC$ID <- 1: nrow(Dist_OPC_Sensor$counts_OPC)
    Bins$ID <- 1: nrow(Bins)
    
    
    # browser()
    OPC_all <- cbind(Dist_OPC_Sensor_wet$counts_OPC,
                     Dist_OPC_Sensor_wet$Volume_OPC$volume,
                     Predict_Dist_OPC_Sensor$predict_counts)
    names(OPC_all) <- c("diameters", "counts", "volume", "pred_counts")

    OPC_all <- Bins %>%
        left_join(OPC_all, by = "diameters") %>%
        select(- diameters)
    
    # raw counts for each bin
    df_OPC_counts <- as.data.frame(t( round(OPC_all$counts, digits = 4)))
    names(df_OPC_counts) <- paste0(c(OPC_all$Bins), "_sensor")
    
    # OPC raw volumes for each bin
    df_OPC_volume <- as.data.frame(t( round(OPC_all$volume, digits = 4)))
    names(df_OPC_volume) <- paste0(c(OPC_all$Bins), "_Vol_Sens")
    
    df_OPC_pred <- as.data.frame(t (round(OPC_all$pred_counts, digits = 4)))
    names(df_OPC_pred) <- paste0(c(OPC_all$Bins), "_ref")
    
    # data frame (TS) ###
    df_OPC <- cbind(Start_Time, Stop_Time, 
                        density = density_fun$minimum,
                        K_GF = Kohler_fun$minimum,
                        df_OPC_pred, 
                        df_OPC_counts,
                        df_OPC_volume,
                        Dist_OPC_Sensor_wet$Met_OPC,
                        Dist_OPC_Sensor_wet$PM_data)
    
    return(df_OPC)
    
    
}

interval <- 60  # minutes
TS <- seq(from = Start_Time, by = interval*60, to = Stop_Time) +1
#Determining number of columns in the Final dataframe (this is the average between the Start_Time and the End_Time)
df_OPC1 <- Final_function_OPC(RefData, General.df, Begin = Start_Time, End = Stop_Time, 
                              Sensor_dates = NULL, Min_APS = 0.62, Max_APS =  0.800, verbose = TRUE)
Columns <- length(names(df_OPC1))
Rows    <- length(TS) -1
df_OPC  <- as.data.frame(matrix( rep(0, Columns * Rows), nrow = Rows, ncol = Columns))
names(df_OPC) <- names(df_OPC1)
rm(df_OPC1)


# loop over DateBegin and DateEnd
for (i in 1:(length(TS)-1)) {
    
    Start_Time <- TS[i]
    Stop_Time   <- TS[i+1]
    
    df_OPC[i,] <- Final_function_OPC(RefData = RefData, General.df = General.df, Begin = Start_Time, End = Stop_Time, 
                                     Sensor_dates = NULL, Min_APS = 0.62, Max_APS =  0.800, verbose = TRUE)
    print(i)

}

df_OPC$Start_Time  <- as.POSIXct(df_OPC$Start_Time , tz = "UTC", origin = strptime("1970-01-01", "%Y-%m-%d"))
df_OPC$Stop_Time   <- as.POSIXct(df_OPC$Stop_Time  , tz = "UTC", origin = strptime("1970-01-01", "%Y-%m-%d"))

# save data
write.csv(df_OPC, "summary_OPC_Counts_Volume.csv")


########################################################################
########################################################################

# Start_Time <- as.POSIXct("2018-09-30 01:00", tz = "UTC")
# Stop_Time   <- as.POSIXct("2018-10-07 01:00", tz = "UTC") #"2018-10-07"  #"2018-09-30 01:00"

# library(purrr)
# DateBegin_list <- TS
# DateEnd_list <- seq(from = Start_Time +interval*60, by = interval*60, to = Stop_Time + interval*60)
# AAA <- map2_dfr(.x = DateBegin_list, .y = DateEnd_list, Final_function_OPC)

# list_date <- list(Begin = DateBegin_list, End = DateEnd_list)
# map_list  <- pmap(list_date, Final_function_OPC)

#outer(list_date$Begin, list_date$End, Final_function_OPC)


# map2(as.list(DateBegin_list), as.list(DateEnd_list), Final_function_OPC)

###############################################################################
###############################################################################

##########################################
# CORRELATION PLOTS ######################
##########################################

output_folder <- paste0(WD,"/JRC_11/Retrieved_plots/")

# reload all OPC data with OPC COUNTS for each bin, VOLUME for each bin and predicted OPC  COUNTS data according to the reference  
df_OPC <- read.csv("summary_OPC_Counts_Volume.csv", header = T)
df_OPC$Start_Time <- as.POSIXct(df_OPC$Start_Time , tz = "UTC", origin = strptime("1970-01-01", "%Y-%m-%d"))
df_OPC$Stop_Time <- as.POSIXct(df_OPC$Stop_Time , tz = "UTC", origin = strptime("1970-01-01", "%Y-%m-%d"))

# make a loop for all the plots
# select only sensors counts and reference counts of the OPC
df_OPC_all <-  df_OPC[, !names(df_OPC) %in% c("Start_Time", "Stop_Time", "density", 
                                              "Temp", "Hum", "OPCTemp", "OPCHum", "OPCVol","OPCTsam",
                                              "OPC_PM1", "OPC_PM25", "OPC_PM10", "Ref_PM10")] 
df_OPC_ref_counts <- df_OPC_all[grep(patter = "_ref", x = names(df_OPC_all))]
# df_OPC_counts <- df_OPC_all[ , !names(df_OPC_all) %in% names(df_OPC_ref_counts)]

# select only COUNTS from the OPC sensor for each BIn
df_OPC_counts <- df_OPC_all[grep(patter = "_sensor", x = names(df_OPC_all))]
# select only VOlumes from the OPC sensor for each Bin
df_OPC_volume <- df_OPC_all[grep(patter = "_Vol", x = names(df_OPC_all))]

# Calculate residuals (between sensor COUNTS and reference COUNTS)
df_OPC_residuals <- df_OPC_counts - df_OPC_ref_counts
names(df_OPC_residuals) <- names(df_OPC_residuals)[grep(pattern = "_sensor", x = names(df_OPC_residuals) )] %>%
    gsub(pattern = "_sensor", replacement = "_residual", x = .)


# select Start_Time, density, Temperature and RH
df_OPC_density <- df_OPC[, c("Start_Time", "density", "OPCTemp", "OPCHum", "Temp", "Hum", "OPCVol")]
n_Bins <- ncol(bins_diameters) # number of Bins



# CORRELATION PLOTS #######################################################################
# relationship between count, predicted counts, Temperature, RH and density for EACH Bin ##

# set START and END time again
Start_Time <- as.POSIXct("2018-09-30 01:00:01", tz = "UTC") #"2018-09-30 01:00"
Stop_Time   <- as.POSIXct("2018-10-07 01:00:01", tz = "UTC") # "2018-10-07 01:00"


for (i in 1:n_Bins) {
    
    # Plot Correlation from each Bin with COUNTS and VOLUME
    Bin_data <- cbind(df_OPC_density, df_OPC_counts[i], df_OPC_ref_counts[i], df_OPC_volume[i], df_OPC_residuals[i])
    Bin_data <- Bin_data %>%
        filter(density < 2)
    
    
    # attach DateTime label in the filename
    year_start <- str_sub(Start_Time, start = 0, end = -16)
    month_start <- str_sub(Start_Time, start = 6, end = -13)
    day_start <- str_sub(Start_Time, start = 9, end = -10) 
    hour_start <- str_sub(Start_Time, start = 12, end = -7) 
    Begin <- paste0(year_start,"_",month_start,"_", day_start,"_",hour_start)
    
    year_end <- str_sub(Stop_Time, start = 0, end = -16)
    month_end <- str_sub(Stop_Time, start = 6, end = -13)
    day_end <- str_sub(Stop_Time, start = 9, end = -10) 
    hour_end <- str_sub(Stop_Time, start = 12, end = -7) 
    End <- paste0(year_end,"_",month_end,"_", day_end,"_",hour_end)
    
    
    # find how many missing counts we have in our data
    n_TRUE <- length(which(is.na(Bin_data$Bin13_sensor)))
    
    if (!n_TRUE/nrow(Bin_data) > 0.35) tryCatch({
        
        # associate Bin to original diameter
        diameter <- bins_diameters[which(names(bins_diameters) == paste0("Bin", i-1))] 
        
        #    pairs(Bin_data [- which(names(Bin_data) == "Start_Time")],
        pairs(Bin_data,
              lower.panel = panel.smooth,
              upper.panel = panel.cor,
              diag.panel  = panel.hist,
              main = paste0("COUNTS and Volume @ Bin ", i-1, " (", diameter, "um)", " ", Begin, "_to_", End),
              cex.labels = 1.5,
              cex.cor  =3
        )
        print(i)
        
        # save static plot
        dev.copy(png, paste0(output_folder, "corr_plots/", "Bin", i-1, "_", Begin, "__", End,".png"),
                 width = 1200, height = 1050, units = "px", pointsize = 8,
                 bg = "white", res = 150)
        dev.off()
        
        # create pairs with ggpairs
        # plot <- ggpairs(Bin_data[- which(names(Bin_data) == "Start_Time")],
        plot <- ggpairs(Bin_data,
                        # upper = list(continuous = custom_cor_plotly),
                        lower = list(continuous = custom_smooth_plotly),
                        title = paste0("COUNTS and Volume @ Bin ", i-1, " (", diameter, "um)", " ", Begin, "_to_", End),
                        progress = T) +
            theme(plot.title = element_text(lineheight=.8, face="bold", size = 8, hjust = 0.5)) +
            theme(axis.title.y = element_text(colour="black", size=10),
                  axis.text.y  = element_text(angle=0, vjust=0.5, size=8, colour = "black")) +
            ggtitle(paste0(" \n",
                           "COUNTS and Volume @ Bin ", i-1, " (", diameter, "um)", " from ", Begin, "_to_", End)) +
            theme(plot.title = element_text(lineheight=.8, face="bold", size = 5, hjust = 0.5)) 
            theme_bw()
        # plot
        
        # interactive plots
        p <- ggplotly(plot, dynamicTicks = F, originalData = F) %>%
            highlight( off = "plotly_deselect", on = "plotly_selected") 
        # p
        
        # save dynamic plots
        htmlwidgets::saveWidget(p, paste0(output_folder, "corr_plots/", "Bin", i-1, "_", Begin, "__", End, ".html"), selfcontained = FALSE)
        
    }, error= function(err) { print(paste0("NOT enough COUNTS!"))
        
    }, finally = {
        
    })
    
}

###############################################################################
###############################################################################
# calculate the k parameters (dependant from particle composition) to
# be used in the growing function expression g(RH)
###############################################################################
###############################################################################


# reload all OPC data with OPC COUNTS for each bin, VOLUME for each bin and predicted OPC  COUNTS data according to the reference  
df_OPC <- read.csv("summary_OPC_Counts_Volume_wet.csv", header = T)
df_OPC$Start_Time <- as.POSIXct(df_OPC$Start_Time , tz = "UTC", origin = strptime("1970-01-01", "%Y-%m-%d"))
df_OPC$Stop_Time <- as.POSIXct(df_OPC$Stop_Time , tz = "UTC", origin = strptime("1970-01-01", "%Y-%m-%d"))

# load PM10 mass data in ug/cm3 from OPC sensor and from reference data (TEOM...in this case)
PM10_data <- df_OPC[df_OPC$density > 1, c("Start_Time", "OPC_PM10", "Ref_PM10", "Hum", "density")] 
PM10_data <- na.omit(PM10_data)
DataXY <- PM10_data %>% 
    mutate(x = Hum) %>% 
    mutate( y = OPC_PM10 / Ref_PM10 ) %>% 
    filter(y < 4 & y > -1)

########################################################################################################
# function to calculate the influence of the RH on the OPC sensor data using the Growing Factor equation
###### Andrea Di Antonio, O.A.M. Popoola, et al. Sensors, 18, 2790, 2018 ###############################
########################################################################################################
f_humidogram <- function(x, k = k, density = DataXY$density) { 
    # fit and return a vector giving a humidogram
    # x is a vector of numerical representing relative humidity in percentage 
    # k is a numerical, the initial value of k ~ 0.4, coefficient of KKohler (sensor  2018, 18, 2790)
    # df_OPC is the entire dataframe including counts, PM mass concentrations and met data
    
      
    return(1 + ( (1/density) * k ) / ( 100/x - 1)) 
}

# only use the Growing factor GH
f_humidogram2 <- function(x, k = k, density = DataXY$density) { 
    # fit and return a vector giving a humidogram
    # x is a vector of numerical representing relative humidity in percentage 
    # k is a numerical, the initial value of k ~ 0.4, coefficient of KKohler (sensor  2018, 18, 2790)
    # df_OPC is the entire dataframe including counts, PM mass concentrations and met data
    
    #browser()
    GF <- (1 - x/100)^-k
    return(GF)
}


# return modelled ratio of OPC/ref PM10 data in function of the RH
Model_humidogram <- nlsLM(y ~ C + f_humidogram2(x, k = k), 
                          data    = DataXY, 
                          start   = list(k = 0.2, C = 0))

summary(Model_humidogram)
# fitted data
Model <- list(Model = Model_humidogram, Tidy = tidy(Model_humidogram), Augment = augment(Model_humidogram), Glance = glance(Model_humidogram), Call = Model_humidogram$call, Coef = coef(Model_humidogram))


# plot
plot <-  ggplot() + 
    theme_bw() +
    geom_point(data = DataXY %>% filter(y > 0),
               aes(x = x, y = y), stat = "identity", fill = "gray") +
    geom_line(data = Model$Augment, aes(x, .fitted,  col = "Modelled"), size = 2) +
    theme(axis.title.x = element_text(colour  = "black", size = 15),
          axis.text.x  = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 15, colour = "black")) +
    theme(axis.title.y = element_text(colour = "black", size = 15),
          axis.text.y  = element_text(angle = 0, vjust = 0.5, size = 15, colour = "black")) +
    xlab(expression(paste("RH(%)"))) +
    ylab(expression(paste("OPC/TEOM")))

plot


####################################################################################
###### Fitting function for PM using k factor ######################################
###### L.R.Crilley et al. Atmos. Meas. Tech. 11, 709-72-, 2018 #####################
###### Andrea Di Antonio, O.A.M. Popoola, et al. Sensors, 18, 2790, 2018 ###########
####################################################################################


# get fitted OPC/TEOM (OPC/TEOM = C(RH) = PM_raw/PM_corrected)
C_RH <- Model$Augment$.fitted
OPC_PM10_corrected <- DataXY$OPC_PM10 / C_RH
DataXY$OPC_PM10_corrected <- OPC_PM10_corrected
names(DataXY)[names(DataXY) == 'Start_Time'] <- 'date'



# plot time-series
time_series_sensor <- data_frame_to_timeseries(DataXY, tz = threadr::time_zone(DataXY$date))
ts_OPC <- cbind(time_series_sensor$OPC_PM10, 
                time_series_sensor$OPC_PM10_corrected)
names(ts_OPC) <- c("PM10_OPC", "OPC_PM10_corrected")
colour_vector <- threadr::ggplot2_colours(45)
plot <- dygraph(ts_OPC,
                    main = paste0("Corrected PM<sub>10</sub> (&#956;g m<sup>-3</sup>) from ", Start_Time," to ", Stop_Time)) %>%
    dySeries("PM10_OPC", color = "red") %>%
    dySeries("OPC_PM10_corrected" ,color = "blue") %>%
    dyAxis("y", label = "Hourly PM<sub>10</sub> (&#956;g m<sup>-3</sup>)") %>% 
    dyLegend(show = "always", hideOnMouseOut = FALSE, width = 500) %>%
    dyRangeSelector(height = 40)
plot

# save plot
# attach DateTime label in the filename
year_start <- str_sub(Start_Time, start = 0, end = -16)
month_start <- str_sub(Start_Time, start = 6, end = -13)
day_start <- str_sub(Start_Time, start = 9, end = -10) 
hour_start <- str_sub(Start_Time, start = 12, end = -7) 
Begin <- paste0(year_start,"_",month_start,"_", day_start,"_",hour_start)

year_end <- str_sub(Stop_Time, start = 0, end = -16)
month_end <- str_sub(Stop_Time, start = 6, end = -13)
day_end <- str_sub(Stop_Time, start = 9, end = -10) 
hour_end <- str_sub(Stop_Time, start = 12, end = -7) 
End <- paste0(year_end,"_",month_end,"_", day_end,"_",hour_end)
output_folder
filename_html <- file.path(output_folder, paste0("Corrected_PM10__", Begin, "__" ,End, ".html"))

filename_png <- file.path(output_folder, paste0("Corrected_PM10__", Begin, "__" ,End, ".png"))
htmltools::save_html(plot, filename_html)
webshot::webshot(filename_html, file = filename_png, cliprect = "viewport")


#########################################################################################################
#########################################################################################################

# # min_bin <- "Bin10" 
# 
# # D_wet <- as.vector(bins_diameters[1:10])
# D_wet <- bins_diameters
# names(D_wet)
# RH <- df_OPC$Hum
# 
# f_growing <- function(RH, K = 0.4){
#     # x is the Relative Humidity (RH)
#     return(( 1 + K* (RH/ (100-RH) ) )^(1/3))
# }
# 
# # calculate the growing factor for all the Relative Humidity values
#   GF <- sapply(RH, f_growing)
#   GF <- as.data.frame(GF)
# 
# # calculate Dry diameters using the GF(RH)
# # D_dry = D_wet/GH
# # multpiply each element of the dataframe of GF with the numeric vector of diameters  
#   D_dry <- mapply(`*`, 1/GF, D_wet)
#   D_dry <- as.data.frame(D_dry)
#   names(D_dry) <- names(D_wet)
#  
#  
# # Function to calculate the max of each rows 
#  maxFun <- function(x) {max(na.omit(x))} 
#  
# # Apply the function to column row of the data.frame 
#  MAX_D_dry <- apply(D_dry, MARGIN =2, maxFun)
