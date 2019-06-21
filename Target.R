#================================================================CR
### Target.Param: Function to return metrics for Target Diagram (Vs 180428)
#================================================================CR
Target.Param <- function(Mat, verbose = TRUE) {
    # Mat            : DataFrame of data including Case number (optional), x And y
    # return a list with the orthogonal regression: b, ub, a, ua, RSS, rmse, mbe, Correlation, nb, CRMSE, NMSD, Mat, sx, sy 
    
    # return a list with slope (b and ub), intercept (a and ua), the sum of square of residuals (RSS), 
    # the root means square of error (rmse), the mean bias error (mbe), the coefficeint of correlation (Correlation), 
    # the number of valid measurment (nb), the centred rout mean square of error (CRMSE), the normalised mean standard deviation (NMSD)
    # and Mat with relative expanded uncertainty
    
    #checking that the mat is dataFrame
    if(class(Mat) != "data.frame"){
        
        print("Mat is not of dataFrame class. Returning NAs.")
        calib <- list(pente=NA, upente=NA, ordon=NA, uordon=NA, RSS=NA, rmse=NA, mbe=NA, Correlation=NA, nb=NA, CRMSE=NA, NMSD=NA, Mat=NA, sx=NA, sy=NA)
        
    } else {
        
        #checking that the mat dataFrame is not empty
        if(nrow(Mat) < 0) {
            
            print("The Mat dataFrame is empty. Returning NAs.")
            calib <- list(pente=NA, upente=NA, ordon=NA, uordon=NA, RSS=NA, rmse=NA, mbe=NA, Correlation=NA, nb=NA, CRMSE=NA, NMSD=NA, Mat=NA, sx=NA, sy=NA)
            
        } else {
            
            # checking if Mat has 2 columns x and y and renaming Mat Colnames
            if(length(colnames(Mat)) != 2){
                
                print("The Mat dataFrame does not contain 2 columns (x and y). Returning NAs.")
                calib <- list(pente=NA, upente=NA, ordon=NA, uordon=NA, RSS=NA, rmse=NA, mbe=NA, Correlation=NA, nb=NA, CRMSE=NA, NMSD=NA, Mat=NA, sx=NA, sy=NA)
                
            }  else {
                
                colnames(Mat) <- c("xis", "yis")
                
            }
            
            # convert Mat$x and y to its numeric value, it was chr because of reading the 3 first lines of the file
            Mat[,"xis"] <- as.double(Mat[,"xis"])
            Mat[,"yis"] <- as.double(Mat[,"yis"])
            str(Mat)
            
            #checking that the mat dataFrame is not empty after converting to double
            if(nrow(Mat) < 0) {
                
                print("The Mat dataFrame is empty. Returning NAs.")
                calib <- list(pente=NA, upente=NA, ordon=NA, uordon=NA, RSS=NA, rmse=NA, mbe=NA, Correlation=NA, nb=NA, CRMSE=NA, NMSD=NA, Mat=NA, sx=NA, sy=NA)
                
            } else {
                # Filtering for the valid data only
                Mat <- subset(Mat, !is.na(Mat$xis) & !is.na(Mat$yis))
                
                #Orthogonal regression (see annex b of equivalence method)
                nb <- nrow(Mat)
                mo <- mean(Mat$xis)
                mm <- mean(Mat$yis)
                Sxx <- sum((Mat$xis - mo)^2)
                Syy <- sum((Mat$yis - mm)^2)
                Sxy <- sum((Mat$xis - mo) * (Mat$yis - mm))
                pente <- (Syy - Sxx + sqrt((Syy- Sxx)^2 + 4*Sxy^2))/(2*Sxy)
                ordon <- mm - pente * mo
                upente <- sqrt((Syy - (Sxy^2/Sxx))/((nb-2)*Sxx))
                uordon <- sqrt(upente^2 * sum(Mat$xis^2)/nb)
                RSS <- sum((Mat$yis - (ordon + pente * Mat$xis))^2)
                sdo <- sd(Mat$xis)
                sdm <- sd(Mat$yis)
                
                # Regression statistics for Target Diagram (see delta tool user guide)
                rmse   <- sqrt(mean((Mat$yis - Mat$xis)^2)) #sqrt((sum((Mat$yis - (ordon + pente * Mat$xis))^2))/nb)
                nrmse  <- rmse / diff(range( Mat$xis))
                cvrmse <- rmse / mo    
                mbe    <- 1/nb * sum(Mat$yis - Mat$xis)
                nmbe   <- mbe / diff(range( Mat$xis))
                cvmbe  <- mbe / mo
                smbe   <- mbe / sdo
                mae    <- mean(abs(Mat$yis - Mat$xis))
                nmae   <- mae / diff(range( Mat$xis))
                cvmae  <- mae / mo
                CRMSE  <- sqrt(1/nb * sum(((Mat$yis - mm) - (Mat$xis - mo))^2))
                sCRMSE <- CRMSE / sdo
                NMSD   <- (sd(Mat$yis) - sd(Mat$xis)) / sd(Mat$xis)
                Correlation <- cor(Mat$xis,Mat$yis)
                
                # Printing
                if(verbose){
                    cat("\n")
                    cat("--------------------------------\n")
                    print(sprintf("Slope     : %.4g +/- %.4g",pente,upente),quote=FALSE)
                    print(sprintf("Intercept : %.4g +/- %.4g",ordon,uordon),quote=FALSE)
                    print(sprintf("R2        : %.4g",Correlation^2),quote=FALSE)
                    print(sprintf("rmse      : %.4g ",rmse),quote=FALSE)
                    print(sprintf("mbe       : %.4g ",mbe),quote=FALSE)
                    print(sprintf("CRMSE     : %.4g ",CRMSE),quote=FALSE)
                    print(sprintf("NMSD      : %.4g ",NMSD),quote=FALSE)
                    print(sprintf("n         : %.4g ",nb),quote=FALSE)
                    
                }
                # List to be returned
                calib <- list(pente, upente, ordon, uordon, RSS, rmse, mbe, Correlation, nb, CRMSE, NMSD, Mat, sx, sy)
            }
        }
        
        names(calib) <- c("Slope", "uSlope", "Intercept", "uIntercept", "RSS", "rmse", "mbe", "Correlation,r", "nb", "CRMSE", "NMSD", "Mat", "sx", "sy")
    }
    
    return(calib)
}

#================================================================CR
### fill.factor: Function to Addi Target Diagram metrics to Cal_factor DF (Vs 180428)
#================================================================CR
fill.factor <- function (name_Sensor, df.factor, field, orth) {
    # Adding statistics to Cal_factor
    # df.factor : data frame of factors
    # name_Sensor: name of the sensors to be filled with factor values
    # field: name of the field of Cal_Factor DF where are the sensor names
    # orth: list output of Target diagram metrics of the function slope_orth
    df.factor[df.factor[,field] == name_Sensor,"Slope"]       <- orth[[1]]
    df.factor[df.factor[,field] == name_Sensor,"u_slope"]     <- orth[[2]]
    df.factor[df.factor[,field] == name_Sensor,"Intercept"]   <- orth[[3]]
    df.factor[df.factor[,field] == name_Sensor,"u_intercept"] <- orth[[4]]
    df.factor[df.factor[,field] == name_Sensor,"RSS"]         <- orth[[5]]
    df.factor[df.factor[,field] == name_Sensor,"rmse"]        <- orth[[6]]
    df.factor[df.factor[,field] == name_Sensor,"mbe"]         <- orth[[7]]
    df.factor[df.factor[,field] == name_Sensor,"R2"]          <- orth[[8]]^2
    df.factor[df.factor[,field] == name_Sensor,"n"]           <- orth[[9]]
    df.factor[df.factor[,field] == name_Sensor,"CRMSE"]       <- orth[[10]]
    df.factor[df.factor[,field] == name_Sensor,"NMSD"]        <- orth[[11]]
    df.factor[df.factor[,field] == name_Sensor,"sx"]          <- orth[[13]]
    df.factor[df.factor[,field] == name_Sensor,"sy"]          <- orth[[14]]
    return(df.factor)
} 
