sort.overheating <- function(External.file){

    # Things that I still need to do
    # 
    # [ ] Go through matt's paper to check that I have defined each of these factors correctly.
 
  
  
    Complete.flag <- 0
    External.data <- read.csv(External.file, stringsAsFactors = F) # This function needs to change....
    

    
    External.data$Date.Time <- gsub("24:00:00", "00:00:01", External.data$Date.Time) # add the date in...
    External.data$Date.Time <- parse_date_time(External.data$Date.Time, 'md HMS', tz = 'GMT')
    External.data$Date.Time <- as.POSIXct(External.data$Date.Time,"%Y/%m/%d %H:%M:%S") # convert to POSIX
    year(External.data$Date.Time) <- year(External.data$Date.Time) + 2016
    
    start <- yday(External.data$Date.Time[1]) 
    end <- yday(External.data$Date.Time[length(External.data$Date.Time)])
    Ndays <- end-start
    Timestep.min <- minute(External.data$Date.Time[2])-minute(External.data$Date.Time[1])
    Timestep <- round(60/Timestep.min)  # the number of timesteps per hour.
    
    
    Alpha <- 0.8    
    Temp <- Running.mean.temperature(External.data,
                                  Alpha,
                                  Timestep,
                                  Ndays)   
    
    # Variables
    Eps0 <- 5.67e-8
    kwh <- 1/3.6e6
    
    
 
    # Output <- matrix(0,n.files,12) # create an output mtraix
    # Output.timestep <- 0 # create an empty structure? Maybe not needed?



        # str <- cat(suffix,'/',Output.File,'/',Output.File,'.csv', sep = '')
        # data <- importdata(str)
        # data <- data.data
        # 
        #extract output variables
        Temp_ext <- External.data$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.
        Solar_rad <- External.data$Environment.Site.Direct.Solar.Radiation.Rate.per.Area..W.m2..TimeStep.
        RH <- External.data$MAIN.Zone.Air.Relative.Humidity.....TimeStep.
        
        
        # index1 <- data[,1] > 0 # Making sure that we have a month
        # index2 <- datestr2 > 4 & datestr2 < 10 # May to October
        # index <- logical(index1.*index2)
        
        #this all currently assumes just 1 zone
        solar_data <- External.data[,c(5:10)]  # all solar data
               
        Top <- (External.data$MAIN.Zone.Mean.Radiant.Temperature..C..TimeStep. +
                 External.data$MAIN.Zone.Air.Temperature..C..TimeStep.)*0.5
        Delta_T <- Top-Temp$Tconf
        
        # these are data for the summer months only
        
        # this was index2
        ext.temp.positive = External.data$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep. > 0 
        spring.sum = month(External.data$Date.Time) > 4 & month(External.data$Date.Time) < 11
        occ = External.data$PEOPLE.People.Occupant.Count....TimeStep. > 0
        occ.spring.sum = spring.sum & occ
        
        
        Delta_T_2 <- Delta_T[occ.spring.sum]
        Temp_data <- External.data$MAIN.Zone.Air.Temperature..C..TimeStep.[occ.spring.sum]
        Top_data <- Top[occ.spring.sum]
        
        Delta_T_3 <- Delta_T_2[Temp_data>21]
        Occ <- External.data$PEOPLE.People.Occupant.Count....TimeStep.
        HasOccupant <- External.data$PEOPLE.People.Occupant.Count....TimeStep. > 0 # make all the 0 occupancies = 1
        # Output.timestep(j).Temp <- data(:,3)
        # Output.timestep(j).Tconf <- Tconf
        # Output.timestep(j).Top <- Top
        
        
        # Weighted cooling degree hours
        # =============================
        
        WCDH <- Delta_T
        WCDH[External.data$MAIN.Zone.Air.Temperature..C..TimeStep.<=21 | WCDH < 0 ] <- 0
        WCDH[WCDH > 0] <- WCDH[WCDH>0]^2 # replace with WCDH squared...

## ===============================================================================================
##       
        
        # [ ] Need to check that this function is right....
        Out12 <- sum((Delta_T_3>3)+1)/Timestep # same as 0.5 pmv - normal expectation #hrs Tcomf > 3, Max is plus 3, criteria is when this is exceeded by 1 degree
        
## ===============================================================================================
##
                # This one!!!!!!!!
        ACH_percent <- Out12/(sum(occ.spring.sum)/Timestep) # percent of hours during the heating season akin to TM52

## ===============================================================================================
##        

        WCDHtotal <- sum(WCDH)/Timestep #WCDH
        
## ===============================================================================================
##
        Hrs25 <- sum(Temp_data>25)/Timestep              #hrs>25 ---- Timestep is 12 steps per hour
        Hrs28 <- sum(Temp_data>28)/Timestep              #hrs>28

        
## ===============================================================================================
##        
        Hrs25Top <- sum(Top>25)/Timestep              #Top hrs>25
        Hrs28Top <- sum(Top>28)/Timestep              #Top hrs>28
        
## ===============================================================================================
##
        PMV <- PMV_BSI(External.data$MAIN.Zone.Air.Temperature..C..TimeStep.,
                       External.data$MAIN.Zone.Mean.Radiant.Temperature..C..TimeStep.,
                       External.data$MAIN.Zone.Mean.Radiant.Temperature..C..TimeStep.*0+0.1,
                       External.data$MAIN.Zone.Air.Relative.Humidity.....TimeStep.)$PMV
        PMV_occ <- PMV[HasOccupant]
        PMV <- PMV[occ.spring.sum]
       

## ===============================================================================================
##
        Delta_T_2_daily <- Delta_T
        Delta_T_2_daily[occ.spring.sum==0] = 0 # zero outside of the heating season
        # index3 <- index1 <- 0
        # Delta_T_2_daily[index3] <- 0 # Not really sure what this is doing
        Delta_T_2_daily[External.data$MAIN.Zone.Air.Temperature..C..TimeStep.<=21] <- 0
        
        Delta.days <- matrix(0, nrow=24*Timestep, ncol=Ndays)
        
        if (Ndays == 365){
          days=1:365
          days = days[-60] # less the leap year day
        } else {
          days = 1:366 
        }
        
        for (i in days){
          Delta.days[,i] <- Delta_T_2_daily[yday(External.data$Date.Time)==i]# need to index the external data as [1] so that the cbind works
        }
        
        
        
        # need to change into a variables
        Delta_T_2_daily <- Delta.days # make this into a daily matrix
        
        
       
        Delta_T_2_daily_WF_sum <- round(colSums(Delta_T_2_daily)/Timestep)# sum each of the days....
        DeltaT2out <- max(Delta_T_2_daily_WF_sum)
        
        
## ===============================================================================================
##
        
        DeltaT3out <- max(Delta_T_3-4)
    
## ===============================================================================================
##
        
        PMV2 <- PMV + External.data$FLOOR.Surface.Inside.Face.Solar.Radiation.Heat.Gain.Rate.per.Area..W.m2..TimeStep.[occ.spring.sum] / 200 # this is the solar heating on the floor
        Out4 <- sum(PMV>0.5)/Timestep                   #PMV hrs > 0.5
        
        Out9 <- sum(PMV2>0.5)/Timestep                  #PMV hrs in sun > 0.5
        
        
        # HERE!
        UTCI_temp1 <- UTCI_matt2(External.data$MAIN.Zone.Air.Temperature..C..TimeStep.,
                                 External.data$MAIN.Zone.Mean.Radiant.Temperature..C..TimeStep.,
                                 External.data$MAIN.Zone.Air.Relative.Humidity.....TimeStep./100,
                                 External.data$MAIN.Zone.Air.Relative.Humidity.....TimeStep.*0+0.5)
        
        # UTCI_temp1 <- UTCI_temp1[occ.spring.sum]
        
        
        
        # Output.timestep(j).UTCI_temp1 <- UTCI_temp1
        TR1 <- External.data$MAIN.Zone.Mean.Radiant.Temperature..C..TimeStep.
        S1 <- ((TR1 + 273.15)^4)*0.95*Eps0        
        S2 <- S1 + 0.7 * solar_data[,5] #assume 1m2 in total
        TR2 <- (S2/(Eps0*0.95))^(0.25) - 273.15
        
        UTCI_temp2 <- UTCI_matt2(External.data$MAIN.Zone.Air.Temperature..C..TimeStep.,
                                 TR2,
                                 External.data$MAIN.Zone.Air.Relative.Humidity.....TimeStep./100,
                                 External.data$MAIN.Zone.Air.Relative.Humidity.....TimeStep.*0+0.5)

        # Output.timestep(j).UTCI_temp2 <- UTCI_temp2
        # UTCI_temp <- UTCI_temp(index)
        
        UTCI25 <- sum(UTCI_temp1[occ.spring.sum]>25)/Timestep      #UTCI hrs > 25
        UTCI28 <- sum(UTCI_temp2[occ.spring.sum]>28)/Timestep     #UTCI hrs > 28
        
        Delta_T <- UTCI_temp1[occ.spring.sum] - Temp$Tconf[occ.spring.sum]
        # Delta_T_2 <- Delta_T[occ.spring.sum]
        Delta_T_3 <- Delta_T_2[Temp_data>21]

        UTCI.WCDH <- sum(Delta_T_3[Delta_T_3>0]^2)/Timestep  #
        
        Delta_T <- UTCI_temp2 - Temp$Tconf
        Delta_T_2 <- Delta_T[occ.spring.sum]
        Delta_T_3 <- Delta_T_2[Temp_data>21]

        Out11 <- sum(Delta_T_3[Delta_T_3>0]^2)/Timestep #
        
        # if Complete_flag <- 1
        #     j<-j+1
        # end
        

 # Heat stress iso stuff       
 #       Temp <- heat_stress_iso7933(data(:,3),data(:,2),data(:,4)/100,data(:,4)*0+0.5)
 #       Tre <- Temp(index,5)
 #       Output(i,7) <- sum(Tre>38)
        Out8 <- sum(External.data$ELECTRIC.HEATER.Baseboard.Electric.Power..W..TimeStep.)*kwh #Heating
        
        if (length(data) > 5){
            Out18<- sum(External.data$ELECTRIC.HEATER.Baseboard.Electric.Power..W..TimeStep.)*kwh
        }
        
          # Single figure outputs.
          o.ts <- list(UTCI_temp2,
                    UTCI_temp1,
                    PMV2,
                    Delta_T_2_daily_WF_sum,
                    PMV_occ,
                    PMV,
                    WCDH)
          
          names(o.ts) <- c('UTCI_temp2',
                        'UTCI_temp1',
                        'PMV2',
                        'Delta_T_2_daily_WF_sum',
                        'PMV_occ',
                        'PMV',
                        'WCDH')
          
          # Timestep outputs
          o <- list(Out8,
                       Out11,
                       UTCI.WCDH, 
                       UTCI28,
                       UTCI25,
                       Out9,
                       Out4,
                       Hrs28Top,
                       Hrs28,
                       Hrs25Top,
                       Hrs25,
                       WCDHtotal,
                       ACH_percent,
                       Out12,
                       DeltaT2out,
                       DeltaT3out) # hours greater than the comfort temperature.
          
          names(o) <- c('Out8',
                           'Out11',
                           'UTCI.WCDH', 
                           'UTCI28',
                           'UTCI25',
                           'Out9',
                           'Out4',
                           'Hrs28Top',
                           'Hrs28',
                           'Hrs25Top',
                           'Hrs25',
                           'WCDHtotal',
                           'ACH_percent',
                           'Out12',
                           'DeltaT2out',
                           'DeltaT3out')
          
        
          
          
        outputs <- list(o,o.ts)
          
        names(outputs) <- c('o',
                            'o.ts')
          
          
          
        return(outputs) # Need to put something in this output


}