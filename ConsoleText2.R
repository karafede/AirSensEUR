ReadLinesText = function(filepath, nLines=NULL) {
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1, ok = TRUE, warn = TRUE,
                     encoding = "unknown", skipNul = FALSE)
    if ( length(line) == 0 ) {
      break
    }
    print(line)
  }
  
  close(con)
}

library(tcltk)
ReadLinesText("/media/sf_Box_Sync/AirSensEUR/Fieldtests/Shiny/LANUV_01/scriptsLog/console_2017-12-10.log",10)
