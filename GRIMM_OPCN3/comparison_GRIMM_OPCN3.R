

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

# home
# source("C:/JRC_CA/AirSense/Shiny/151016 Sensor_Toolbox.R")
# source("C:/JRC_CA/AirSense/Shiny/Functions4ASE_FK.R")

# JRC
source("S:/Box Sync/AirSensEUR/Fieldtests/Shiny/151016 Sensor_Toolbox.R")
source("S:/Box Sync/AirSensEUR/Fieldtests/Shiny/Functions4ASE_FK.R")

# JRC
WD <- "S:/Box Sync/AirSensEUR/Fieldtests/Shiny/GRIMM_OPCN3" 
# home
# WD <- "C:/JRC_CA/AirSense/Shiny/GRIMM_OPCN3"
# home
# output_folder <- "C:/JRC_CA/AirSense/Shiny/JRC_11/Retrieved_plots/"
# JRC
output_folder <- "S:/Box Sync/AirSensEUR/Fieldtests/Shiny/JRC_11/Retrieved_plots/"


#################
# load GRIMM data
#################

# these are counts/L
# read Bin names
grim_file <- "/190418 Blg 100_107.txt"
# grim_file <- "/GRIMM_190426_190430_ Blg 100_107.txt"
bins_diameters_GRIMM <- read.table(paste0(WD,grim_file),
                       header = F, skip = 1, sep=",", nrows = 1)[-1]

names(bins_diameters_GRIMM) <- paste0("Bin", seq(1:length(names(bins_diameters_GRIMM))))
# remove special characters such as ">" and "µm"
# gsub(paste(c("µm", ">"), collapse = "|"), "",levels(bins_diameters_GRIMM$Bin0))
bins_diameters_GRIMM <- apply(bins_diameters_GRIMM,  MARGIN = 2, function(col) gsub(paste(c("µm", ">"), collapse = "|"), "",(col)) )
diameters_GRIMM <- as.numeric(bins_diameters_GRIMM)
MAX_Diam_GRIMM <- max(diameters_GRIMM)
bins_diameters_GRIMM <- as.data.frame(t(bins_diameters_GRIMM))


GRIMM_data <- read.table(paste0(WD,grim_file),
                         header = F, sep="," , fill = T)

# find lines where it is reported the word "File" and the symbol ">" 
# add all neccessary criteria to skip lines
logical <- apply(GRIMM_data, MARGIN = 2, function(col) grepl(paste(c("File", ">"), collapse = "|"), col))
logical <- as.data.frame(logical)
line.to.remove <- apply(logical, MARGIN = 2, function(col) which(col == TRUE) ) 
line.to.remove <-  unique(unlist(line.to.remove))

# read GRIMM data again but skip selected lines
GRIMM_data <- read.table(paste0(WD,grim_file),
                         header = F, sep="," , fill = T)[-c(line.to.remove), ]

# remove columns with 0 values
GRIMM_data <- GRIMM_data[, colSums(GRIMM_data != 0) > 0]
# add names of header 
names(GRIMM_data) <- c("date", names(bins_diameters_GRIMM))

## !!! need to do some data cleaning
# bins_diameters_GRIMM <- read.csv(paste0(WD,"/bin_diameters_GRIMM.csv"))
n.bin.GRIMM <- names(bins_diameters_GRIMM)
nbin.GRIMM.next <- paste0("Bin", as.numeric(sub(pattern = "Bin",replacement = "",n.bin.GRIMM)) +1 )


names(GRIMM_data)[which(names(GRIMM_data) != "date")] <- n.bin.GRIMM 
# Adjust date format --> replace "." with ":"
GRIMM_data$date <- gsub(".", ":", GRIMM_data$date, fixed = TRUE)
GRIMM_data$date  <- as.POSIXct(GRIMM_data$date,format="%d/%m/%Y %H:%M:%S", tz="UTC")

# make all fileds as numeric
GRIMM_data[, names(GRIMM_data)[which(names(GRIMM_data) != "date")]] <- sapply(names(GRIMM_data)[which(names(GRIMM_data) != "date")], function(x) { as.numeric(GRIMM_data[,x])})

# calculate the effective number of counts within two consecutive Bins of the GRIMM
GRIMM_data[,paste0("Bin",1:(length(n.bin.GRIMM)-1))] <- sapply(1:(length(n.bin.GRIMM)-1),   function(i) GRIMM_data[ ,paste0("Bin",i)] - GRIMM_data[, paste0("Bin",i+1)]) 


# if bin_diameters contains signs like ">" or "<" then do..
# if (grepl(">|< ", as.character(bins_diameters_GRIMM$Bin1)) == TRUE) {
# bins_diameters_GRIMM <- as.numeric(t(gsub(pattern = ">", replacement = "",t(bins_diameters_GRIMM), fixed = TRUE)))
# } else bins_diameters_GRIMM <- bins_diameters_GRIMM


Grim <- list(data = GRIMM_data, 
             unit = "counts/L", 
             Diameters = diameters_GRIMM, 
             units.diameters = "micrometers")


#####################################
###### OPC data #####################
#####################################

# read data from OPCN3 (from the AirSensEUR box)
bin_file <- read.table(paste0(WD,"/190404_BLG100_107_OPCN3.csv"),
                       header = T, skip = 6, sep=",", nrows = 5, fill = TRUE)
# remove columns with NA values
bin_file <- bin_file[, colSums(is.na(bin_file)) != nrow(bin_file)]
bins_diameters_OPCN3 <- data.frame(bin_file[2,])[-1]
# rename upper boundary column (Bin23 (upper boundary))
names(bins_diameters_OPCN3) <- paste0("Bin", seq(1:length(names(bins_diameters_OPCN3)))-1)


# load OPCN3 cleaned data (it should be the file General.Rdata)
# counts are in #/ml (from the InfluxDb)
OPCN3_data <- read.csv(paste0(WD,"/190423_BLG100_107_OPCN3_Influx_Bins.csv"),
                       header = T, stringsAsFactors = F)

# assign Bins to the size of the bins
colnames(OPCN3_data) <- c("date", bins_diameters_OPCN3[names(OPCN3_data)[which(names(OPCN3_data) != "date")]])

# attach the date field
# OPCN3_data$date  <- as.POSIXct(OPCN3_data$date,format="%d/%m/%Y %H:%M",tz="UTC")
OPCN3_data$date <- ymd_hms(OPCN3_data$date, tz = "Europe/Rome")
attr(OPCN3_data$date, "tzone") <- "UTC"


OPCN3_data <- na.omit(OPCN3_data)

# chose diameters withing the same range of the GRIMM
#select only date
date <- OPCN3_data$date
# OPCN3_data[, as.character( bins_diameters_OPCN3[bins_diameters_OPCN3 <= MAX_Diam_GRIMM] )] <- OPCN3_data[, as.character( bins_diameters_OPCN3[bins_diameters_OPCN3 <= MAX_Diam_GRIMM] )]/OPCN3_data$SFR_ml_s
#select only diameters <= max diameter of GRIMM
OPCN3_data <- OPCN3_data[, as.character( bins_diameters_OPCN3[bins_diameters_OPCN3 <= MAX_Diam_GRIMM] )]

# replace diameters with Bins names (again)
names(OPCN3_data) <- names(bins_diameters_OPCN3)[bins_diameters_OPCN3 <= MAX_Diam_GRIMM] 
# assign the date (again)
OPCN3_data$date <- date 

# select diameters <= Diameter GRIMM
bins_diameters_OPCN3 <- bins_diameters_OPCN3[, bins_diameters_OPCN3 <= MAX_Diam_GRIMM]



#################################################################################################
########## Plot Data ############################################################################

# Start_Time <- as.POSIXct("2019-04-04 16:45:00", tz = "UTC") 
# Stop_Time   <- as.POSIXct("2019-05-04 09:41:00", tz = "UTC") 

Start_Time <- as.POSIXct("2019-04-18 20:00:00", tz = "UTC") 
Stop_Time   <- as.POSIXct("2019-04-23 07:41:00", tz = "UTC") 

# Compute mean of reference count
# browser()

# add counts units in the Distribution_Ref_TS function..
Dist_Ref.d_TS <- Distribution_Ref_TS(RefData = Grim$data, DateBegin = Start_Time, DateEnd = Stop_Time, 
                                     diameters_bins = Grim$Diameters, units = "counts/L") 

# Plot of distribution of reference counts versus diameter in log/log
# lognormal distribution (plot)
# (Plot_Dist_Ref_log(Dist_Ref.d_TS$counts_Ref))

# compute mean of the OPC sensor counts (OPCN3)
Dist_OPC_Sensor_wet <- Distribution_OPC_Sensor(General.df = OPCN3_data, DateBegin = Start_Time, DateEnd = Stop_Time, 
                                               bins_diameters = bins_diameters_OPCN3, Counts_units ="counts/L", K = NULL) 

## Plot of distribution of OPC Sensor counts versus DRY diameter in dNlog/dlog_Dp
(Plot_Distribution_OPC_Sensor(Dist_OPC_Sensor_wet$counts_OPC, Dist_Ref.d_TS$counts_Ref, predict_OPC = NULL,
                                                DateBegin = Start_Time, DateEnd = Stop_Time, Counts_units = "counts/L"))



