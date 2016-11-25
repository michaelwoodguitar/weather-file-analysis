CIBSE.calcs <- function(External.file){

 
  cat('Importing the data')
  cat('\n')
  
    # get and format the information
    External.data <- read.csv(External.file, stringsAsFactors = F) # This function needs to change....
    External.data$Date.Time <- gsub("24:00:00", "00:00:01", External.data$Date.Time) # add the date in...
    External.data$Date.Time <- parse_date_time(External.data$Date.Time, 'md HMS', tz = 'GMT')
    External.data$Date.Time <- as.POSIXct(External.data$Date.Time,"%Y/%m/%d %H:%M:%S") # convert to POSIX
    year(External.data$Date.Time) <- year(External.data$Date.Time) + 2016
    
    start <- yday(External.data$Date.Time[1]) 
    end <- yday(External.data$Date.Time[length(External.data$Date.Time)])
    Ndays <- end-start
    Timestep.min <- minute(External.data$Date.Time[2])-minute(External.data$Date.Time[1])
    steps.per.hour <- round(60/Timestep.min)  # the number of timesteps per hour.
    
    # settings
    Alpha <- 0.8    
    Temp <- Running.mean.temperature(External.data,
                                  Alpha,
                                  steps.per.hour,
                                  Ndays)   
    # variables
    Eps0 <- 5.67e-8
    kwh <- 1/3.6e6
    
        external.temp = External.data$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.
        solar.radiation = External.data$Environment.Site.Direct.Solar.Radiation.Rate.per.Area..W.m2..TimeStep.
        relative.humidity = External.data$MAIN.Zone.Air.Relative.Humidity.....TimeStep.
        
        Top <- (External.data$MAIN.Zone.Mean.Radiant.Temperature..C..TimeStep. +
                 External.data$MAIN.Zone.Air.Temperature..C..TimeStep.)*0.5
        
        DelT = round(Top-Temp$Tmax)
        
        ext.temp.positive = External.data$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep. > 0 
        spring.sum = month(External.data$Date.Time) > 4 & month(External.data$Date.Time) < 11
        occ = External.data$PEOPLE.People.Occupant.Count....TimeStep. > 0
        occ.spring.sum = spring.sum & occ
        
        occupied.hours = sum(occ.spring.sum) / steps.per.hour
        
        # reshape time and date
        dtm = External.data$Date.Time
        start.min = minute(dtm[1])
        dtm = dtm[minute(dtm)==start.min] # on the first hours of data...
        
        # reshape DelT (steps per hour across the top)
        DelTmat = DelT
        dim(DelTmat) = c(steps.per.hour, length(DelTmat) / steps.per.hour)
        DelTmat = t(DelTmat)
        
        # CIBSE criterion a)
        # ==================
        
        tic()
        cat('Calculating CIBSE a)')
        cat('\n')
        
        hrs.DT.greater.than.1 <- length(DelT[occ.spring.sum & (DelT > 1)]) / steps.per.hour
        CIBSE.A = hrs.DT.greater.than.1 / occupied.hours
        toc()
                
        # CIBSE criterion b) 
        # ==================
        
        cat('Calculating CIBSE b)')
        cat('\n')
        tic()
        
        mnth.range = 5:10
        WeMax_month = rep(0, length(mnth.range))
        
        for (mnth in mnth.range){
          
          # month index
          month.index = (month(External.data$Date.Time) == mnth)
          
          days.in.month =  days_in_month(mnth)
          
          WeMax_day = rep(0,days.in.month)
          
          for (dy in 1:days.in.month){
          
            # day index
            day.index = (day(External.data$Date.Time) == dy)
            
            hour.index = (day(dtm) == dy & month(dtm) == mnth)
            hour.rows = which(hour.index==T)
            We = apply(DelTmat[hour.rows,], 1, max)
            
            # maximum exceedanec for the day
            WeMax_day[dy] = sum(We[We>0])
              
          }
          
          WeMax_month[mnth+1-mnth.range[1]] = max(WeMax_day)
          
        }
        
        CIBSE.B = max(WeMax_month)
      
        toc()  
        
        # CIBSE C
        # =======
        tic()
        cat('Calculating CIBSE c)')
        cat('\n')
        
        CIBSE.C = max(DelT)
        toc()
        # outputs
        # =======
        
        
          o <- list(CIBSE.A,
                    CIBSE.B,
                    CIBSE.C,
                    WeMax_month,
                    Top,
                    steps.per.hour,
                    DelT,
                    WeMax_day) # hours greater than the comfort temperature.
          
          names(o) <- c('CIBSE.A',
                        'CIBSE.B',
                        'CIBSE.C',
                        'WeMax_month',
                        'Top',
                        'steps.per.hour',
                        'DelT',
                        'WeMax_day')
                 
        return(o) # Need to put something in this output


}