Running.mean.temperature <- function(External.data,
                                     Alpha,
                                     Timestep,
                                     Ndays){

  
  # RUNNING_MEAN_TEMPERATURE Given an input temperature calculates the daily 
  # running mean temperature

  # So, first we need to calculate the mean daily temperature
  
  Tmean <- rep(0,Ndays) # initialise
  
  for (dayofyear in 1:Ndays){
    
    dayindexes <- yday(External.data$Date.Time) == dayofyear
    
    daytemps <- External.data$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.[dayindexes] # get the daily temperature
    
    Tmean[dayofyear] <- mean(daytemps)
    
  }
  
    # initialise
    Trm <- Tmean[1]*rep(1,length(Tmean))
    
    # Good definition of the running mean temperature here:
    # http://www.sciencedirect.com/science/article/pii/S036013230800303X
    # image of the equation we need here: https://dl.dropboxusercontent.com/spa/pftmqkz6pd9ohxb/p40lnj2t.png
    
    # loop over all days
    for (i in 1:(Ndays-1)){
                if (is.na(Tmean[i])){ # check to see if the mean value is valid or not 
                  Trm[i+1] = Trm[i] 
                } else { # if not, then do the calculation
                  Trm[i+1] <- (1-Alpha) * Tmean[i] + Alpha * Trm[i]
                }
    }
    
    Tcomf = 0.33 * Trm + 18.8
    Tmax =  0.33 * Trm + 21.8
    
    # turn Tconf into one long vector at 5 minutes timesteps
    sampsPerDay <- 24*Timestep
    Tcomfcolrep <- matrix(rep(Tcomf, sampsPerDay), nrow=Ndays, ncol=sampsPerDay)
    Tcomf <- as.vector(t(Tcomfcolrep))
    
    output <- list(Trm,Tcomf, Tmax)
    names(output) <- c('Trm', 'Tcomf', 'Tmax')
    return(output)
    
}

