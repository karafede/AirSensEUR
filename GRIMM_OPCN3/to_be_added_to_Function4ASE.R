

if (Ref.Type == "Bin.GRIMM") {
  
  # read Bin names
  bins_diameters_GRIMM <- read.table(csvFile,
                                     header = F, skip = 1, sep=",", nrows = 1)[-1]
  names(bins_diameters_GRIMM) <- paste0("Bin", seq(1:length(names(bins_diameters_GRIMM))))
  # remove special characters such as ">" and "µm"
  bins_diameters_GRIMM <- apply(bins_diameters_GRIMM,  MARGIN = 2, function(col) gsub(paste(c("µm", ">"), collapse = "|"), "",(col)) )
  diameters_GRIMM <- as.numeric(bins_diameters_GRIMM)
  MAX_Diam_GRIMM <- max(diameters_GRIMM)
  bins_diameters_GRIMM <- as.data.frame(t(bins_diameters_GRIMM))
  
  # read GRIMM data
  # check the structure of the GRIMM file first...
  Reference.i <- read.table(WD,csvFile,
                           header = F, sep="," , fill = T)
  
  # find lines where it is reported the word "File" and the symbol ">" 
  # add all neccessary criteria to skip lines
  logical <- apply(Reference.i, MARGIN = 2, function(col) grepl(paste(c("File", ">"), collapse = "|"), col))
  logical <- as.data.frame(logical)
  line.to.remove <- apply(logical, MARGIN = 2, function(col) which(col == TRUE) ) 
  line.to.remove <-  unique(unlist(line.to.remove))
  
  # read GRIMM data again but skip selected lines
  Reference.i <- read.table(csvFile,
                           header = F, sep="," , fill = T)[-c(line.to.remove), ]
  
  # remove columns with 0 values
  Reference.i <- Reference.i[, colSums(Reference.i != 0) > 0]
  # add names of header 
  names(Reference.i) <- c("date", names(bins_diameters_GRIMM))
  
  ## !!! need to do some data cleaning
  n.bin.GRIMM <- names(bins_diameters_GRIMM)
  nbin.GRIMM.next <- paste0("Bin", as.numeric(sub(pattern = "Bin",replacement = "",n.bin.GRIMM)) +1 )
  
  
  names(Reference.i)[which(names(Reference.i) != "date")] <- n.bin.GRIMM 
  # Adjust date format --> replace "." with ":"
  Reference.i$date <- gsub(".", ":", Reference.i$date, fixed = TRUE)
  Reference.i$date  <- as.POSIXct(Reference.i$date,format="%d/%m/%Y %H:%M:%S", tz="UTC")
  
  # make all fileds as numeric
  Reference.i[, names(Reference.i)[which(names(Reference.i) != "date")]] <- sapply(names(Reference.i)[which(names(Reference.i) != "date")], function(x) { as.numeric(Reference.i[,x])})
  
  # calculate the effective number of counts within two consecutive Bins of the GRIMM
  Reference.i[,paste0("Bin", 1:(length(n.bin.GRIMM)-1))] <- sapply(1:(length(n.bin.GRIMM)-1),   function(i) Reference.i[ ,paste0("Bin",i)] - Reference.i[, paste0("Bin",i+1)]) 
  
}
