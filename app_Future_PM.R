#================================================================CR
# Licence: ====
# Copyright 2018 EUROPEAN UNION
# Licensed under the EUPL, Version 1.2 or subsequent versions of the EUPL (the "License"); 
# You may not use this work except in compliance with the License. 
# You may obtain a copy of the License at: http://ec.europa.eu/idabc/eupl
# Unless required by applicable law or agreed to in writing, the software distributed 
# under the License is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR CONDITIONS 
# OF ANY KIND, either express or implied. See the License for the specific language 
# governing permissions and limitations under the License.
# Date: 05/11/2017
# 
# Authors
# - Michel Gerboles        , michel.gerboles@ec.europa.eu  - European Commission - Joint Research Centre
# - Federico Karagulian    , federico.karagulian@ec.europa.eu - European Commission - Joint Research Centre
# - Laurent Spinelle       , laurent.spinelle@ec.europa.eu - European Commission - Joint Research Centre
# - Maria Gabriella Villani, mariagabriella.villani@enea.it - ENEA
# - Marco Signorini        , marco.signorini@liberaintentio.com - Liberatintentio srl
# - Alex Kotsev            , alexander.kotsev@ec.europa.eu - European Commission - Joint Research Centre
#================================================================CR

#================================================================CR
# Content ====
#================================================================CR
#  0 - Clear memory and restart R-session
#  1 - Get configuration parameters in ASEconfig_MG.R - Create file system structure check for General.Rdata availbility, create log file
#  3 - Shiny App

#================================================================CR
# 0 START ====
#================================================================CR
cat("-----------------------------------------------------------------------------------\n")
# Clear memory and restart R-session
remove(list = ls()) # [-which(ls() %in% c("DisqueFieldtest"))])

# checking if internet is available to access CRAN
havingIP <- function() {
    if (.Platform$OS.type == "windows") {
        ipmessage <- system("ipconfig", 
                            intern = TRUE)
    } else {
        ipmessage <- system("/sbin/ifconfig", 
                            intern = TRUE)
    }
    # validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]) {3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
    # any(grep(validIP, ipmessage))
    validIP <- "(?<=[^0-9.]|^)[1-9][0-9]{0,2}([.]([0-9]{0,3})){3}(?=[^0-9.]|$)"
    
    return(any(unlist(gregexpr( validIP, ipmessage, perl = TRUE) ) != -1))
}
isInternet <- TRUE # isInternet <- havingIP() # no use to test. If there is no Internet then it is not necessary to run the script
if (isInternet) cat("[shiny] INFO, internet is  available\n") else cat("[shiny] INFO, internet is not available\n")

# detectiong the OS
isOS <- .Platform$OS.type 
cat(paste0("[shiny] INFO, the OS platform is : ", isOS), sep = "\n") 

# Checking if RStudio is used
isRStudio  <- Sys.getenv("RSTUDIO") == "1"
if (isRStudio)  cat("[shiny, isTcltk] INFO, ASE_Script is run under Rstudio\n") else cat("[shiny, isTcltk] INFO, ASE_Script is not run under Rstudio\n")

#================================================================CR
#  1 - Get configuration parameters in ASEconfig_MG.R - Create file system structure check for General.Rdata availbility, create log file
#================================================================CR
#================================================================CR
#  1 Config ====
#   1.a Setting the Working Directory
#   1.b Checking Functions4AES.R and SensorToolBox availability. 
#   1.c Sourcing Functions4AES.R and SensorToolBox, geting path of ASEconfig_xx.R 
#   1.d Loading packages (global.R)
#   1.e Create the directory three for the AirSensEUR device, change working directory to point to the directory of the AirSensEUR device, 
#       sending console to a file in the directory three (script log)
#   1.f Init Shiny ----

#================================================================CR
#----------------------------------------------------------------CR
# 1.a Setting the Working Directory, ----
#  The files  ASEconfig_xx.R and Functions4ASE.R shall be in this Directory
#----------------------------------------------------------------CR
cat("-----------------------------------------------------------------------------------\n")
cat("[Shiny] INFO, setting working directory\n")
# Searching in the directory from where app.R is run
# https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
# Using automatic identification of the current directory with kimisc::thisfile(), 
# if it does not work we could use choose.dir() or tcltk/JAVA, java if the OS does not have tcltk capabilities.
# Once the DisqueFieldtest is set, we stop looking for it 
Script_Dir <- function(isRStudio, isInternet = TRUE) { 
    # isRStudio :  Logical, TRUE if Rstudio is used otherwise FALSE
    # isInternet:  Logical, TRUE  if internet is available to access CRAN, ddefault is TRUE
    # Return the path of current script
    
    # trying function getScriptPath of package envDocument
    if (!require(envDocument)) {
        if (isInternet) {
            install.packages("envDocument")
        } else stop(cat("[shiny, Script_Dir()] ERROR, internet missing to install the package envDocument The script is stopped."))   
    }
    require(envDocument)
    env_doc <- env_doc(output = c("return", "print", "table"), system = TRUE,
                       version = TRUE, packages = TRUE, script = FALSE, git = FALSE,
                       domino = c("auto", "on", "off"))
    print(env_doc)
    DisqueFieldtest <- as.character(env_doc[env_doc$Name == "Directory", "Value"])
    
    if (is.null(DisqueFieldtest)) DisqueFieldtest <- getSrcDirectory(function(x) {x})
    
    if (is.null(DisqueFieldtest)) DisqueFieldtest <- sys.calls()[[1]] [[2]] 
    
    if (is.null(DisqueFieldtest) & isRStudio) {
        
        IsRstudioapi <- require(rstudioapi)
        if (!IsRstudioapi) { # kimisc needs to be installed ot use kimisc::thisfile(), checking if internet is available
            if (isInternet) {
                install.packages("rstudioapi");
                require(rstudioapi);
            } else stop(cat("[shiny, Script_Dir()] ERROR, internet missing to install the package rstudioapi The script is stopped."))    
        }
        
        # Searching in the directory where the script is run
        if (dirname(rstudioapi::getActiveDocumentContext()$path) != "") {
            cat(paste0("[app.R] INFO, using rstudioapi::getActiveDocumentContext()$path, app.R is run from ", rstudioapi::getActiveDocumentContext()$path, "\n"))
            DisqueFieldtest <- dirname(rstudioapi::getActiveDocumentContext()$path)
        } else {
            cat(paste0("[app.R] ERROR, rstudioapi::getActiveDocumentContext()$path unable to detect the directory of app.R. Returning NULL.\n"))
            DisqueFieldtest <- NULL
        }
    } 
    
    if (is.null(DisqueFieldtest)) {
        
        IsKmisc <- require(kimisc); 
        IsKnitr <- require(knitr)
        if (!IsKmisc) { # kimisc needs to be installed ot use kimisc::thisfile(), checking if internet is available
            if (isInternet) {
                install.packages("kimisc");
                require(kimisc);
            } else stop(cat("[shiny, Script_Dir()] ERROR, internet missing to install the package kimisc. The script is stopped."))    
        }
        if (!IsKnitr) {# knitr needs to be installed, checking if internet is available
            if (isInternet) {
                install.packages("knitr");
                require(knitr);
            } else stop(cat("[shiny, Script_Dir()] ERROR, internet missing to install the package knitr. The script is stopped."))
        }
        # Searching in the directory where the script is run
        if (!is.null(kimisc::thisfile())) {
            cat(paste0("[app.R] INFO, app.R is run from ", dirname(kimisc::thisfile()), "\n"))
            DisqueFieldtest <- dirname(kimisc::thisfile())
        } else {
            cat(paste0("[app.R] ERROR, kimisc::thisfile() unable to detect the directory from where is run app.R. Returning NULL.\n"))
            DisqueFieldtest <- NULL
        }    
    }
    
    # This work only if we never change of working directory.
    # In fact we really change of working directory once an AirSensEUR is selected
    if (is.null(DisqueFieldtest)) DisqueFieldtest <- setwd(".")
    
    return(DisqueFieldtest)
}

# dectecting the directory of app.R
#if (!exists("DisqueFieldtest")) 
DisqueFieldtest <- Script_Dir(isRStudio = isRStudio, isInternet = isInternet)
DirShiny        <- DisqueFieldtest

# if Script_Dir does not work, e. g. when deploying the app then force to "/home/shinyadmin/R"
if (is.null(DisqueFieldtest)) DisqueFieldtest <- "/home/shinyadmin/App" 
cat(paste0("[shiny, isTcltk] INFO, directory from where the script is run: ", DisqueFieldtest), sep = "\n") 

# Set working directory to directory where is App.R
cat(paste0("[Shiny] INFO, setting working directory to ", DisqueFieldtest), sep = "\n")
setwd(DisqueFieldtest)
cat("-----------------------------------------------------------------------------------\n")
cat("\n")

#----------------------------------------------------------------CR
#   1.b Checking Functions4AES.R and SensorToolBox availability. 
#   1.c Sourcing Functions4AES.R and SensorToolBox, geting path of ASEconfig_xx.R 
#   1.d Loading packages (global.R)
#----------------------------------------------------------------CR
source("global_Merge.R")

#----------------------------------------------------------------CR
#  1.e Getting the file path for ASEconfig_xx.R ----
#----------------------------------------------------------------CR
# Name of the configuration file, the extension shall be .R

#----------------------------------------------------------------CR
# 1.f Init Shiny ----
#----------------------------------------------------------------CR
choices.ASEconfig <- list.files(path = getwd(), pattern = glob2rx("ASEconfig*.R"))
Dir.Logs          <- grep(pattern = glob2rx("*scriptsLog*"), x = list.dirs(DirShiny), value = TRUE)
choices.Logs      <- list.files(Dir.Logs, full.names = TRUE)
Selected.Logs     <- choices.Logs[which.max(file.info(choices.Logs)$mtime)]
jscode            <- "shinyjs.closeWindow = function() { window.close(); }"
TimeZone          <- c("UTC", "Etc/GMT-1", "Europe/Amsterdam" , "Europe/Berlin", "Europe/Paris", "Europe/Rome") # Fill up with other Time Zone
Influx.TimeZone   <- c("UTC", "Etc/GMT-1", "Local time", "Europe/Amsterdam" , "Europe/Berlin", "Europe/Paris", "Europe/Rome") # Fill up with other Time Zone
TableTZ           <- as.data.frame(cbind(seq_along(TimeZone), TimeZone), stringsAsFactors = FALSE)
Influx.TableTZ    <- as.data.frame(cbind( seq_along(Influx.TimeZone), Influx.TimeZone), stringsAsFactors = FALSE)
choices.shield    <- list.files(path = file.path(getwd(), "Shield_Files"), pattern = "*.asc")
choices.Ref.unit  <- c("ppb","ppm", "ug/m3","mg/m3","counts.L-1", "dcounts.dLogD-1")

# ui =============================================================
ui <- navbarPage(title = "AirSensEUR v0.13", id = "ASE", theme = shinytheme("cerulean"), selected = "SelectASE",
                 
                 # shinyjs must be initialized with a call to useShinyjs() in the app's ui.
                 useShinyjs(),
                 extendShinyjs(text = jscode, functions = c("closeWindow")),
                 
                 # Set up shinyalert
                 useShinyalert(), 
                 
                 # Include the line below in ui.R so you can send messages
                 # https://stackoverflow.com/questions/32226331/r-shiny-pop-up-window-with-options
                 tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});')),
                           tags$style(".shiny-plot-output{width: 72vw !important; height: 82vh !important; text-align: right;}")),
                 
                 tabPanel("SelectASE" , value = "SelectASE", icon = icon("mouse-pointer"),  
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  br(),
                                  selectInput( inputId = "Config_Files", label = "List of configured AirSensEURs"                      , 
                                               choices = choices.ASEconfig, selected = choices.ASEconfig[1]),
                                  textInput(   inputId = "Selected", label = "Selected AirSensEUR", value = ""),
                                  actionButton(inputId = "Select", label = "Select AirSensEUR", icon = icon("check-square")),
                                  # actionButton("Apply", label = "Apply"),
                                  hr(),
                                  textInput(inputId = "NewFile", label = "New config file (ASEconfig*, * = SOS id)", value = "ASEconfig"),
                                  actionButton(inputId = "Create.New", label = "Create new AirSensEUR", icon = icon("plus-circle")),
                                  hr(),
                                  actionButton(inputId = "Quit"   , label = "Quit", icon = icon("power-off"))
                                  , width = 3),
                              mainPanel(
                                  tabPanel("Selected_ASE",
                                           tabsetPanel(id = "tabMainPanel2",
                                                       # do not add a Spinner on the next TableOutput as it will not update
                                                       tabPanel(title = "Push data", icon = icon("download"), tableOutput("Pushdata.cfg")),
                                                       tabPanel("Filtering", icon = icon("filter"),
                                                                h4("Filtering Sensors data")  , tableOutput("FilteringSensor"),
                                                                h4("Filtering Reference data"), tableOutput("FilteringRef")
                                                       ),
                                                       tabPanel(title = "Calibration", tableOutput("Calib.cfg"), icon = icon("tachometer")),
                                                       tabPanel(title = "SetTime"    , tableOutput("SetTime.cfg"), icon = icon("time", lib = "glyphicon"))
                                           )
                                  )
                                  , width = 9)
                          )
                 ),
                 tabPanel("GetData", value = "GetData", icon = icon("database"),
                          sidebarLayout(
                              sidebarPanel(
                                  tabsetPanel(id = "ForServers",
                                              tabPanel(title = "Time-shield",  
                                                       value = "tPTimeshield",
                                                       uiOutput("uiUserMins"), 
                                                       uiOutput("uiUserMinsAvg"), 
                                                       uiOutput("uiDelay"), 
                                                       uiOutput("Dates"), 
                                                       uiOutput("Variables"),
                                                       uiOutput("uiasc.File")
                                              ),
                                              # tabPanel("shield",
                                              # ),
                                              tabPanel(title = "Proxy", 
                                                       value = "tPProxy",
                                                       uiOutput("uiPROXY"), 
                                                       div(style = "display: inline-block;vertical-align:top; width: 74%;",uiOutput("uiURL")),
                                                       div(style = "display: inline-block;vertical-align:top; width: 25%;",uiOutput("uiPORT")),
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiLOGIN")),
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiPASSWORD"))
                                              ),
                                              tabPanel(title = "Sensor Data",
                                                       value = "tPSensordown",
                                                       tabsetPanel(id = "SensorDown",
                                                                   tabPanel(title = "SOS"   , 
                                                                            value = "tPSOS",
                                                                            uiOutput("uiDown.SOS"), 
                                                                            uiOutput("uiAirsensWeb"), 
                                                                            uiOutput("ASE.name"), 
                                                                            uiOutput("uiSOS.tzone"), 
                                                                            uiOutput("uiDown_SOS")
                                                                   ),
                                                                   tabPanel(title = "Influx", 
                                                                            value = "tPInflux",
                                                                            uiOutput("uiDown.Influx"),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 74%;",uiOutput("uiHost")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 1%;",HTML("<br>")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 23%;",uiOutput("uiPort")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiUser")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiPass")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiDb")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("ASE.Box")),
                                                                            uiOutput("uiInflux.TZ"),
                                                                            uiOutput("uiDown_Influx")
                                                                   )
                                                       )
                                              ),
                                              tabPanel(title = "Reference Data" , value = "tPRef",
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiDown.Ref")),
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;",uiOutput("uiDown_Ref")),
                                                       uiOutput("uiSelected"),
                                                       br(),
                                                       tabsetPanel(id = "DownloadMode", selected = "ftp",
                                                                   # https://shiny.rstudio.com/reference/shiny/latest/fileInput.html
                                                                   # https://stackoverflow.com/questions/21813773/r-shiny-uploading-a-file-on-button-click
                                                                   tabPanel("csv",
                                                                            br(),
                                                                            p("reference data can only be in 1 csv or Rdata file with headers: date(Y-m-d H:M:S), CO_ppm or CO/co,NO,NO2,O3,NOx,SO2, PM2.5, PM10"),
                                                                            textInput(inputId = "file1", 
                                                                                      label   = "Choose CSV or Rdata file:"),
                                                                            actionButton(inputId = "browse", 
                                                                                         label   = "Browse"),
                                                                            checkboxInput(inputId = "header", 
                                                                                          label   = "Header", 
                                                                                          value   = TRUE),
                                                                            radioButtons(inputId = 'sep', 
                                                                                         label = 'Separator',
                                                                                         choices = c(Comma = ",", Semicolon = ";", Tab = '\t'), 
                                                                                         selected = ",", 
                                                                                         inline = TRUE),
                                                                            radioButtons(inputId = 'quote', 
                                                                                         label = 'Quote',
                                                                                         choices = c(None = '', 'Double Quote' = '"','Single Quote' = "'"), 
                                                                                         selected = '"', 
                                                                                         inline = TRUE),
                                                                            radioButtons(inputId = 'Ref.Type',
                                                                                         label   = 'Type of references: one Reference pollutant or binned PM distribution (bins in micrometers)',
                                                                                         choices = c('Ref', 'Bin.DMPS', 'Bin.APS', 'GRIMM'),
                                                                                         selected = 'Ref',
                                                                                         inline   = TRUE)),
                                                                   tabPanel("ftp",
                                                                            uiOutput("uiurlref"),
                                                                            p("Data can be in 1 or more url linking to 1 csv files with headers: 
                                                                              DateTime(Y-m-d H:M:S), CO_ppm,NO,NO2,O3,NOx,SO2, on a ftp site")),
                                                                   tabPanel("SOS",
                                                                            uiOutput("uiRefSOSname"),
                                                                            uiOutput("uiRef.SOS.name"), 
                                                                            uiOutput("uiRefPollutants"),
                                                                            uiOutput("uiRefDate")),
                                                                   tabPanel("a_i_p",
                                                                            uiOutput("uiRef__a_i_p__name"),
                                                                            uiOutput("uiRef.a_i_p.name"), 
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;", uiOutput("ui__a_i_p__User")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;", uiOutput("ui__a_i_p__Pass")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;", uiOutput("uiRef__a_i_p__Organisation")),
                                                                            div(style = "display: inline-block;vertical-align:top; width: 48%;", uiOutput("uiRef__a_i_p__Station")),
                                                                            uiOutput("uiRef__a_i_p__Pollutants"), 
                                                                            uiOutput("uiRef__a_i_p__Date"))
                                                       ),
                                                       tags$hr(),
                                                       uiOutput("uiReference.name"),
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;", uiOutput("uicoord.ref.Long")), 
                                                       div(style = "display: inline-block;vertical-align:top; width: 48%;", uiOutput("uicoord.ref.Lat")), 
                                                       uiOutput("uialt.ref"), 
                                                       uiOutput("uiref.tzone")
                                              )
                                  )
                                  , width = 3),
                              mainPanel(
                                  tabsetPanel(id = "InfoPrint",
                                              #tabPanel("Log", verbatimTextOutput("console")),
                                              tabPanel(title = "GetData Panel",
                                                       
                                                       h3("Time-shield"), 
                                                       h4("Time"), 
                                                       textOutput("UserMins"), 
                                                       textOutput("UserMinsAvg"), 
                                                       textOutput("Delay"),
                                                       hr(), h4("Shield Data"), 
                                                       textOutput("asc.File"), 
                                                       tableOutput("Shield"),
                                                       
                                                       hr(), h3("Updated Proxy values"), 
                                                       textOutput("PROXY"), 
                                                       textOutput("URL"), 
                                                       textOutput("PORT"), 
                                                       textOutput("LOGIN"), 
                                                       
                                                       hr(), h3("Sensor data download"),
                                                       h4("Influx"), 
                                                       textOutput("Down.Influx"), 
                                                       textOutput("Host"), 
                                                       textOutput("Port"), 
                                                       textOutput("User"), 
                                                       textOutput("Db"), 
                                                       textOutput("Dataset"), 
                                                       textOutput("Influx.TZ"),
                                                       
                                                       hr(), h4("SOS"), 
                                                       textOutput("Down.SOS"), 
                                                       textOutput("AirsensWeb"), 
                                                       textOutput("AirsensEur.name"), 
                                                       textOutput("SOS.TZ"),
                                                       
                                                       hr(), h3("Reference data download"), 
                                                       textOutput("Down.Ref"), 
                                                       textOutput("Reference.name"), 
                                                       textOutput("coord.ref"), 
                                                       textOutput("alt.ref"), 
                                                       textOutput("ref.tzone"),
                                                       textOutput("FTPMode"),
                                                       
                                                       h4("csv"), 
                                                       textOutput("csvref"),
                                                       h4("ftp"), 
                                                       textOutput("urlref"), 
                                                       
                                                       h4("SOS"), 
                                                       textOutput("GDPRefSOSname"),
                                                       textOutput("GDPRef.SOS.name"), 
                                                       textOutput("GDPRefPollutants"),
                                                       textOutput("GDPRefDateDownload"),
                                                       
                                                       h4("a_i_p"),
                                                       textOutput("GDPRef__a_i_p__name"),
                                                       textOutput("GDPRefUser__a_i_p__"),
                                                       textOutput("GDPRef__a_i_p__Organisation"),
                                                       textOutput("GDPRef__a_i_p__Station"),
                                                       textOutput("GDPRef__a_i_p__Pollutants"),
                                                       textOutput("GDPRef__a_i_p__DateDownload")
                                                       
                                              ),
                                              tabPanel(title = "Influx Sensor Data",  withSpinner(verbatimTextOutput('Influx.Content'), type = 8) ),
                                              tabPanel(title = "SOS Sensor Data",     withSpinner(verbatimTextOutput('SOS.Content'), type = 8) ),
                                              tabPanel(title = "Reference Data",      withSpinner(verbatimTextOutput('Ref.content'), type = 8) ),
                                              tabPanel(title = "General Sensor Data", withSpinner(verbatimTextOutput('General.Content'), type = 8) )
                                  )
                                  , width = 9)
                          )
                 )
                 ,
                 tabPanel("Data Treatment"  , value = "Data Treatment", icon = icon("calculator"),
                          sidebarLayout(
                              sidebarPanel(
                                  # change nav-tabs font size
                                  # #https://stackoverflow.com/questions/19813429/r-shiny-tabset-title-modify-font-size#19814885
                                  tags$head(tags$style(type = 'text/css', ".nav-tabs {font-size: 13px} "),
                                            tags$style(HTML(".selectize-input, .selectize-dropdown {font-size: 75%;}"))
                                  ), 
                                  
                                  actionButton(inputId = "Merge"    , label = "Merge Influx <-SOS <-Ref", icon = icon("compress")),
                                  actionButton(inputId = "Save"     , label = "Save"                    , icon = icon("save")) ,
                                  actionButton(inputId = "UpdateLog", label = "UpdateLog"               , icon = icon("list-ol")) ,
                                  hr(),
                                  div(style = "display: inline-block;vertical-align:top; width: 20%;", 
                                      checkboxInput(inputId = "SavePlot"   , label = "Save Plot", value = FALSE, width = NULL)
                                      #, bsButton(inputId = "SaveGeneral", label = "Save Gen.",size = "extra-small")
                                  ),
                                  div(style = "display: inline-block;vertical-align:top; width: 79%;",
                                      uiOutput("uiNameSensors")),
                                  tabsetPanel(id = "Calib_data", 
                                              selected = "uiFiltering",
                                              tabPanel("Filtering"       , icon = icon("filter")                 , uiOutput("uiFiltering")),
                                              tabPanel("Calib"           , icon = icon("tachometer")             , uiOutput("uiCalib") ),
                                              tabPanel("SetTime"         , icon = icon("time", lib = "glyphicon"), uiOutput("uiSetTime"))
                                  )    
                                  , width = 3),
                              mainPanel(
                                  uiOutput("MenuOPCBins"),
                                  tabsetPanel(id = "tabMainPanel",
                                              tabPanel("Config", icon = icon("eye"),
                                                       tabsetPanel(id = "Configs",
                                                                   tabPanel("Downloaded"   , icon = icon("download"), withSpinner(tableOutput("Downloaded" ), type = 6)),
                                                                   tabPanel("FilteringMain", icon = icon("filter"),
                                                                            h4("Filtering Sensor data")   , tableOutput("Outliers_Sensor"),
                                                                            h4("Filtering Reference data"), tableOutput("Outliers_Ref")
                                                                   ),
                                                                   tabPanel("CalibMain"    , tableOutput("Calib_data"), icon = icon("tachometer")),
                                                                   tabPanel("SetTimeMain"  , tableOutput("CalTime"), icon = icon("time", lib = "glyphicon") )
                                                       )
                                              ),
                                              tabPanel("PlotFiltering", icon = icon("filter"),
                                                       tabsetPanel(id = "tabPlots",
                                                                   tabPanel("Warming"      ,  icon = icon("toggle-off")  , withSpinner(dygraphOutput(outputId = "Warming"   , height = 750), type = 8) , width = "96%"),
                                                                   tabPanel("Temp.&Humid." ,  icon = icon("tint")        , withSpinner(dygraphOutput(outputId = "Temp.Humid", height = 750), type = 8) , width = "96%"),
                                                                   tabPanel("Invalid", icon = icon("cut"),
                                                                            tabsetPanel(id = "TabInvalid",
                                                                                        tabPanel("Table", icon = icon("bars"), 
                                                                                                 br(),
                                                                                                 helpText("Time periods of Invalid data for the selected sensor and reference data. ", 
                                                                                                          "Right-click on the table to delete/insert rows. ", 
                                                                                                          "Double-click on a cell to edit. The button \"Save\" only saves the file of invalid
                                                                                                          date time periods while discarding of invalids is carried out by setting the CheckBoxes 
                                                                                                          \"Apply validity periods\" to TRUE."),
                                                                                                 br(),
                                                                                                 actionButton(inputId = "Save.row.Valid", label = "Save",icon = icon("save")),
                                                                                                 br(),
                                                                                                 rHandsontableOutput("hot")),
                                                                                        tabPanel("Plot", icon = icon("signal"),  withSpinner(dygraphOutput(outputId = "Invalid.Sens", height = 750), type = 8) , width = "96%" ))),
                                                                   tabPanel("Neg.values",  icon = icon("minus-circle"), withSpinner(dygraphOutput(outputId = "Neg.values", height = 750), type = 8) , width = "96%" ),
                                                                   tabPanel("Outliers"  ,  icon = icon("log-out", lib = "glyphicon"), 
                                                                            tabsetPanel(id = "TabOutliers",
                                                                                        tabPanel("Sens.Outliers", icon = icon("thermometer"), withSpinner(dygraphOutput(outputId = "Sens.Outliers", height = 750), type = 8) , width = "96%"),
                                                                                        tabPanel("Ref.Outliers" , icon = icon("thermometer"), withSpinner(dygraphOutput(outputId = "Ref.Outliers" , height = 750), type = 8) , width = "96%" )
                                                                            )
                                                                   ),
                                                                   tabPanel("StatFiltering",  icon = icon("eye"), tableOutput("StatFiltered"))
                                                       )
                                              ),
                                              tabPanel("Covariates", icon = icon("sort-by-alphabet", lib = "glyphicon"),
                                                       tabsetPanel(id = "TabCovariates",
                                                                   tabPanel(title = uiOutput("title.Dist.Times"), icon = icon("stats", lib = "glyphicon"), withSpinner(htmlOutput("ts_Cov_dygraphs") , type = 8), width = "100%" ),
                                                                   tabPanel("Matrix" , icon = icon("th", lib = "glyphicon"), uiOutput("MenuMatrix") 
                                                                            # withSpinner(plotlyOutput("Matrix.i"), type = 8)
                                                                   )
                                                       ) 
                                              ),
                                              tabPanel("Calibration", icon = icon("tachometer"),
                                                       tabsetPanel(id = "TabCalibration",
                                                                   tabPanel("Map"             , icon = icon("map-marker", lib = "glyphicon")   , leafletOutput("mymapCal", height = 800)),
                                                                   tabPanel("Scatterplot"     , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Calibration") ),
                                                                   tabPanel("SummaryCal"      , icon = icon("list-alt"  , lib = "glyphicon")   , verbatimTextOutput("SummaryCal")),
                                                                   tabPanel("Calibrated"      , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Calibrated") ),
                                                                   tabPanel("TimeSeries"      , icon = icon("stats"     , lib = "glyphicon")   , withSpinner(dygraphOutput("ts_Cal_dygraphs", height = 750), type = 8) , width = "96%"),
                                                                   tabPanel("Residual Matrix" , icon = icon("th"        , lib = "glyphicon")   , withSpinner(plotOutput("ResCalMatrix"), type = 8) ),
                                                                   tabPanel("MultiLinear"     , icon = icon("list-alt"  , lib = "glyphicon")   , 
                                                                            fluidRow(
                                                                                column(width = 5, offset = 0,
                                                                                       br(),
                                                                                       helpText("Coefficients and type of relationship between covariates and sensor data: ", 
                                                                                                "Double-click on a cell with values to edit. The button \"Save\"  saves the file of multivariate
                                                                                                fitting. It shall be saved in order to be used for fitting calibration model.",
                                                                                                "The file of MultiLinear fitting can be edited and taken into consideration if \"Model for calibration\" in the SideBar Layout is set to MuliLinear."),
                                                                                       br(),
                                                                                       actionButton(inputId = "Save.row.Multi", label = "Save"  ,icon = icon("save")),
                                                                                       actionButton(inputId = "Del.row.Multi" , label = "Delete",icon = icon("Del")),
                                                                                       #actionButton(inputId = "New.row.Multi" , label = "Update file contents"   ,icon("list-alt"  , lib = "glyphicon")),
                                                                                       br(),
                                                                                       rHandsontableOutput("Multi")
                                                                                ),
                                                                                column(width = 5, offset = 0,
                                                                                       br(),
                                                                                       helpText("When fitting Multilinear calibration, the degrees of the covariates ", 
                                                                                                "are either set to 1 if there is no Multivariate file ", 
                                                                                                "in the window below or they are set to the degrees found in the", 
                                                                                                " Multivariate file if it exists.", 
                                                                                                "In order to change the degrees of the covariates , select a sensor,",
                                                                                                "edit the coefficient in the table at left and click the button \"Save\".", 
                                                                                                "In the SideBarLayout, tab \"Calib\", set the radio button",
                                                                                                "\"Method of Prediction\" to: \"Calibration with current data\".",
                                                                                                "The fitting starts automatically.",
                                                                                                "The field \"degree\" can be set to 0 (constant mean), 1 (Linear)",
                                                                                                "2 (parabolic), 3 (cubic) or ExpGrowth (exponential growth",
                                                                                                " C.exp(kx).",
                                                                                                "Use the button \"Delete\" to return to MultiLinear with degrees set to 1."),
                                                                                       br(),
                                                                                       verbatimTextOutput("ListValid")
                                                                                )
                                                                            )
                                                                   )
                                                       ) 
                                              ),
                                              tabPanel("Prediction" , icon = icon("line-chart"), 
                                                       tabsetPanel(id = "TabPrediction",
                                                                   tabPanel("Map"             , icon = icon("map-marker", lib = "glyphicon")   , leafletOutput("mymapExtrap",height = 800)),
                                                                   tabPanel("Scatterplot"     , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Prediction") ),
                                                                   tabPanel("SummaryExtra."   , icon = icon("list-alt"  , lib = "glyphicon")   , verbatimTextOutput("SummaryExtra")),
                                                                   #tabPanel("TimeSeries"      , icon = icon("stats"     , lib = "glyphicon")   , withSpinner(plotOutput("PredictionTS"), type = 8) ),       
                                                                   tabPanel("TimeSeries"      , icon = icon("stats"     , lib = "glyphicon")   , withSpinner(dygraphOutput("ts_Extra_dygraphs", height = 750), type = 8) , width = "96%"),
                                                                   tabPanel("Residual Matrix" , icon = icon("th"        , lib = "glyphicon")   , withSpinner(plotOutput("ResExtraMatrix"), type = 8) ),
                                                                   tabPanel("Uncertainty"     , icon = icon("stats"     , lib = "glyphicon")   , 
                                                                            fluidRow(
                                                                                column(width = 4, offset = 0,
                                                                                       tableOutput("U_Table")
                                                                                ),
                                                                                column(width = 4, offset = 0,
                                                                                       withSpinner(plotOutput("Uncertainty"), type = 8)
                                                                                ),
                                                                                column(width = 4, offset = 0,
                                                                                       plotOutput("SqrRes")
                                                                                )
                                                                            ),
                                                                            fluidRow(
                                                                                column(width = 4, offset = 0,
                                                                                       plotOutput("Scatter")
                                                                                )
                                                                            )
                                                                   ),
                                                                   tabPanel("U Target Diagram", icon = icon("screenshot"   , lib = "glyphicon"), withSpinner(htmlOutput("Target"), type = 8) ),
                                                                   tabPanel("Drift"        , icon = icon("external-link", lib = "font-awesome"), 
                                                                            tabsetPanel(id = "Drift", 
                                                                                        tabPanel("Absolute Drift vs time" , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Drift") ),
                                                                                        tabPanel("Relative Drift vs time" , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Rel.Drift") ),
                                                                                        tabPanel("Absolute Drift vs dose" , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Dose.Drift") ),
                                                                                        tabPanel("Relative Drift vs dose" , icon = icon("line-chart", lib = "font-awesome"), plotOutput("Rel.Dose.Drift") )
                                                                            )  
                                                                   )
                                                       ) 
                                              ),
                                              tabPanel("Report MarkDown", icon = icon("bars"), h3("work in progress..."), htmlOutput("renderedReport"),
                                                       downloadButton("report", "Generate report")),
                                              tabPanel("DataTable"    , icon = icon("bars")  , withSpinner(DT::dataTableOutput(outputId = "DataTable"), type = 8) ),
                                              #tabPanel("Retrieved"    , icon = icon("signal"), withSpinner(plotOutput(outputId = "Retrieved"), type = 8) ),
                                              tabPanel("RawData"      , icon = icon("signal"), withSpinner(htmlOutput("ts_RawData_dygraphs"), type = 8) , width = "96%"),
                                              tabPanel("Log"            , icon = icon("list-ol"), verbatimTextOutput("console"))
                                  )
                                  , width = 9)
                          )
                 ),
                 tabPanel("Memory" , value = "MemoryUsage", icon = icon("mouse-pointer"),  
                          # https://stackoverflow.com/questions/33502903/how-to-make-shiny-give-back-memory-after-a-session-ends
                          sidebarLayout(
                              sidebarPanel(
                                  br(), width = 3
                              ),
                              mainPanel(
                                  tabPanel("Memory",
                                           tableOutput('foo')
                                  )
                                  , width = 9
                              )
                          )
                 ),
                 tabPanel("About",  value = "About", icon = icon("info-circle"),
                          sidebarPanel(titlePanel("Version history")
                                       , width = 3
                          ),
                          mainPanel(
                              verbatimTextOutput("VersionInfo"), 
                              width = 9
                          )
                 )
                 ,
                 tabPanel("Help", value = "Help", icon = icon("question"),
                          sidebarPanel(titlePanel("User Manual")
                                       , width = 1
                          ),
                          mainPanel( 
                              #tags$iframe(style = "height:900px; width:100%; scrolling=yes", src = "ShinyASE.pdf"),
                              tags$iframe(class = "shiny-plot-output",
                                          src = "https://docs.google.com/document/d/e/2PACX-1vSH7N4piil32823BM5jJxNElQkwkm17RXczmgR6qyMXNOJyoY3BpxJoqL444o9s54VoNpxDZp74dwQB/pub?embedded=true")
                              , width = 9
                          )
                 ),
                 tabPanel("Console Logs", value = "ConsoleLogs", icon = icon("info-circle"),
                          sidebarPanel(titlePanel("Select Logs"), 
                                       selectInput( inputId  = "ConsoleLogsFile", 
                                                    label    = "List of console logs files", 
                                                    choices  = choices.Logs, 
                                                    selected = Selected.Logs
                                       ),
                                       width = 3
                          ),
                          mainPanel( 
                              verbatimTextOutput('LogstextWithHTML'), # ui output as a list of HTML p() tags 
                              width = 9
                          )
                 )
                 
)

# row <- function(...) {
#     tags$div(class= "row", ...)
# }
# 
# col <- function(width, ...) {
#     tags$div(class=paste0("span", width), ...)
# }

