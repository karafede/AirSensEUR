library("rChoiceDialogs", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("tidyverse", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("xts", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("pspline", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")


load(  rchoose.files(default = getwd(),
                     caption = "Select files"))
General.df <- General.df%>%
    filter(date >= as.POSIXct("2018-09-03", tz = "UTC") & date <= as.POSIXct("2018-09-4", tz = "UTC"))

Y2fi2Ref   <- function(y2fit, yref) return(y2fit * diff(range(yref))/diff(range(y2fit)) + diff(c(mean(y2fit),mean(yref))) ) # * (diff(range(yref))/diff(range(y2fit))) 
Y2Ref.fit  <- function(y0, y2fit, yref) return( y0 * diff(range(yref))/diff(range(y2fit)) + diff(c(mean(y2fit),mean(yref))) )   # * (diff(range(yref))/diff(range(y2fit))) 
Derivative <- function(t, y, y_name = NULL, Spar = 0.35) {
    
    # t         :   Date PosixCT
    # y         :   parameter for which to calculate the 1st derivaive
    # Spar      :   smoothing parameter, typically (but not necessarily) in (0,1]. 
    #               When spar is specified, the coefficient λ of the integral of the squared second derivative in the fit (penalized log likelihood) criterion 
    #               is a monotone function of spar, see the details below. Alternatively lambda may be specified instead of the scale free spar=s.
    # return a numerical vector giving the 1st derivative vs t
    
    # Fitting 2 different exponential decay depending on the derivative of y
    
    browser()
    # creating dataframe
    DataXY <- na.omit(data.frame(t=t,y=y))
    
    # adding smooting of y to be able to derivate
    # https://stackoverflow.com/questions/3480388/how-to-fit-a-smooth-curve-to-my-data-in-r
    DataXY$ysmooth <- smooth.spline(t, y, spar = Spar)$y
    
    # Computing the 1st derivative of y
    # fit.psp <- smooth.Pspline(DataXY$t, DataXY$y)
    DataXY$dy <- splinefun(DataXY$t, DataXY$ysmooth)(t, 1)
    
    p <- ggplot(DataXY, aes(x = t)) +
        ggtitle(label = 'initial not smoothed')  + xlab('') + 
        # plotting y raw data with points
        geom_point( aes(y = y , col = 'Raw_data')) + ylab('Relative humidity in %') +  
        # adding y smoothed data wtih red line
        geom_line( aes(y = ysmooth, col = 'smoothed_data')) +
        # adding 1st derivative with green line
        geom_line(aes(y = Y2fi2Ref(dy,y), col = 'First_derivative')) +
        # now adding the secondary axis,
        scale_y_continuous(sec.axis = sec_axis(~(. - diff(c(mean(DataXY$dy),mean(DataXY$y)))) / (diff(range(DataXY$y))/diff(range(DataXY$dy))) , name = "1st derivative of y")) + 
        # Add horizontal line at derivative = 0
        geom_hline(yintercept = Y2Ref.fit(y0 = 0, y2fit = DataXY$dy, yref = DataXY$y)) + 
        # modifying colours and theme options
        scale_colour_manual(values = c('Raw_data' = 'black', 'smoothed_data' = 'red', 'First_derivative' = 'blue')) +
        labs(y = "'Relative humidity in %'",
             x = "Date and time",
             colour = "Parameter") +
        theme(legend.position = c(0.8, 0.9))
    # plotting
    p

    # fitting exponential decay
    y.Pos <- which(DataXY > 0)
    y.Neg <- which(DataXY < 0)
    
    return(DataXY$dy)
}
General.df$dRHdt <- Derivative(General.df$date,General.df$Relative_humidity)

f_ExpDD    <- function(t, y0, yf, k) {
    # http://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/
    # t         :   PosixCT, x values
    # y0        :   numeric, start values at mini t values
    # yf        :   numeric, final values at maxi t values
    # k         :   numeric, rate of the exponetial decay
    # equation: y = y0 + (yf - y0) e-kt, k > 0
    # Features:
    # t starts at y0 and decays towards yf at a rate k
    # Asymptotic to y = yf to right
    # Passes through (min(t), y0 )
    # Decreasing, but bounded below by y = yf
    
    #a= 1E5/(0.05 * diff(range(t, na.rm = T))) # if we assume that exp(-10) is near from 0
    #b = - a * min(t, na.rm = T)
    a=1; b=0
    
    return( yf + (y0 - yf) * exp(-k*(a * t + b)) )
}
f_Exp_reDD <- function(t, y0, yf, k) {
    # http://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/
    # t         :   PosixCT, x values
    # y0        :   numeric, start values at mini t values
    # yf        :   numeric, final values at maxi t values
    # k         :   numeric, rate of the exponetial decay
    # y = y0 + (yf - y0) * exp(k * (t - max(t, na.rm = T)), k > 0
    # Features:
    # t starts at y0 and decays towards yf at a rate k
    # Asymptotic to y = y0 to left
    # Passes through (min(t), y0 ) and (max(t), yf)
    # yo is the initial value
    # Decreasing, but bounded below by y = yf
    
    browser()
    # a= 1E5/(0.05 * diff(range(t, na.rm = T))) # if we assume that exp(-10) is near from 0
    # b = - a * max(t, na.rm = T)
    a=1; b= - max(t, na.rm = T)
    return( y0 + (yf - y0) * exp(k * (a * t + b)) )
}
Y0 <- max(General.df$NO2B43F_volt, na.rm = T)
Yf <- min(General.df$NO2B43F_volt, na.rm = T)
rate <- 0.05 
General.df$RH_ExpDD     <-    f_ExpDD(t = General.df$Relative_humidity, y0 = Y0, yf = Yf, k = rate)
General.df$RH_Exp.re.DD <- f_Exp_reDD(t = General.df$Relative_humidity, y0 = Y0, yf = Yf, k = rate)

  plot(General.df$Relative_humidity, General.df$RH_ExpDD,   type = "l",
       ylim = c(min(General.df$RH_ExpDD),   max(General.df$RH_ExpDD)),   
       xlim = c(min(General.df$Relative_humidity, na.rm = T),max(General.df$Relative_humidity, na.rm = T)))
lines(General.df$Relative_humidity, General.df$RH_Exp.re.DD, 
       ylim = c(min(General.df$RH_Exp.re.DD), max(General.df$RH_Exp.re.DD)), 
       xlim = c(min(General.df$Relative_humidity, na.rm = T),max(General.df$Relative_humidity, na.rm = T)))

for (i in seq(1.01,1.08, 0.005)) lines(RH, f_ExpDD(t = RH, yf = 1, k = i, y0 = 4))
