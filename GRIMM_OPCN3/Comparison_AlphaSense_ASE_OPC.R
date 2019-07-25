
# alphasense

alpha.file <- choose.files()
# [1] "S:\\Box Sync\\AirSensEUR\\Fieldtests\\Shiny\\GRIMM_OPCN3\\190424_BLG100_107_OPCN3_raw_clean.csv"
df_alpha <- read.csv(alpha.file, header = T)

df_alpha$date <- lubridate::mdy_hm(df_alpha$date, tz = "Europe/Rome")
attr(df_alpha$date, "tzone") <- "UTC"
df_alpha <- openair::timeAverage(df_alpha, avg.time = "min")


# influx

Influx.file <- choose.files()
#[1] "S:\\Box Sync\\AirSensEUR\\Fieldtests\\Shiny\\GRIMM_OPCN3\\InfluxData_190424_clean.csv"
df_influx <- read.csv(Influx.file, header = T)
df_influx$date <- lubridate::ymd_hms(df_influx$date, tz = "UTC")


                 
plot <-  ggplot() + 
  geom_point(data = df_alpha, aes((date), (Bin00/SFR.ml.s.), col = "red"), stat = "identity") +
  geom_point(data = df_influx, aes((date), (Bin0), col = "blue"), stat = "identity") 
plot


