generateSiteValues <- function(p){
  
  
  
  
  # site <- matrix(,nrow=0, ncol=0) # we don't need to initialise this variable$..
  e <- define_e()
  
  #Calculating the real values of the building
  floorArea <- p[2]
  footprint <- p[2]/p[3]      #37.5
  
  a <- sqrt(p[5]*footprint)   #9.6
  b <- footprint/a            #3.9
  
  #S E N W
  wallAreas <- cbind(a, b ,a ,b)*2.8*p[3]
  
  wallArea <- 2*2.8*p[3]*(a+b) #151 
  roofArea <- footprint        #37.5
  vol <- roofArea*2.8*p[3]
  
  if (e$convection==TRUE){
    Rhc = 0.13+0.04             #m2K/W
    RhcR = 0.10+0.04
  } else {
    Rhc = 1/25+1/7             #m2K/W
    RhcR = 1/25+1/7
  }
  
  UVWalls = (Rhc +1/p[6])^-1
  #Adding the correction
  #UVWalls = UVWalls*(1-(-3.1100e-03*UVWalls^2-5.1815e-02*UVWalls+5.0888e-05))
  
  UVRoof = (RhcR +1/p[7])^-1
  #Adding the correction
  #UVRoof = UVRoof*(1-( 1.6763e-02*UVRoof-1.0654e-05))
  
  #Windows
  if (e$windows==TRUE){
    windowSizeTotal = p[9]*p[2]
  } else {
    windowSizeTotal = 0
  }
  
  
  case <- p[14]
  
  if (case==1){
    UVWin = 5.7    #5.778 in e+
    #Correction
    UVWin = 3.04
  } else if (case==2){
    UVWin = 2.7    #2.67 in e+
    #correction
    UVWin = 1.39
  } else if (case==3){
    UVWin = 1.7    #
    #correction
    UVWin = 0.9
  } else {
    UVWin = 1.7    #
    #correction
    UVWin = 0.9
  }
  
  
  wallConsidered <- c(1, 1, 1, 1)
  
  if (p[4] > 0){
    wallConsidered[p[4]] <- 0
  }
  
   case <- p[1]
  if (case==1){    
    exposedWallArea <- t(wallAreas*wallConsidered) - windowSizeTotal
    UVRoof <- 0
    UA_OE <- exposedWallArea*UVWalls + roofArea * UVRoof    
  } else if (case==2){
    exposedWallArea <- t(wallAreas*wallConsidered) - windowSizeTotal    
    UA_OE <- exposedWallArea*UVWalls + roofArea*UVRoof
  } else if (case==3){
    wallConsidered[1,1] <- wallConsidered[1,1]*wallConsidered[1,3]
    wallConsidered[1,3] <- wallConsidered[1,1]*wallConsidered[1,3]
    wallConsidered[1,2] <- wallConsidered[1,2]*wallConsidered[1,4]
    wallConsidered[1,4] <- wallConsidered[1,2]*wallConsidered[1,4]
    exposedWallArea <- wallAreas*t(wallConsidered) - windowSizeTotal    
    UA_OE <- exposedWallArea*UVWalls + roofArea*UVRoof
  } else {
    exposedWallArea <- wallArea - windowSizeTotal
    UA_OE <- exposedWallArea*UVWalls + roofArea*UVRoof
  }
  
  UA_Win <- windowSizeTotal*UVWin
  
  #Include the lossess due to constant ventilation (now included as a neg gain)
  UA_QR <- UA_Win #Only in this case
  totalUA <- UA_QR + UA_OE 
  
  realHC <- 60000
  
  realCw <- realHC*(wallArea+roofArea)
  realCa <- p[8]/100*30000*wallArea + vol*1000*1.25
  
  
  
  
  site <- list(a,
               b,
               floorArea,
               vol,
               wallArea,
               roofArea,
               windowSizeTotal,
               UVWin,
               exposedWallArea,
               wallConsidered,
               UVWalls,
               UVRoof,
               UA_OE,
               UA_QR,
               totalUA,
               realCw,
               realCa)
  
  names(site) <- c('a',
                   'b',
                   'floorArea',
                   'vol',
                   'wallArea',
                   'roofArea',
                   'windowSizeTotal',
                   'UVWin',
                   'exposedWallArea',
                   'wallConsidered',
                   'UVWalls',
                   'UVRoof',
                   'UA_OE',
                   'UA_QR',
                   'totalUA', 
                   'realCw',
                   'realCa')
  
  site$a = a
  site$b = b
  site$floorArea = floorArea
  site$vol = vol
  site$wallArea = wallArea
  site$roofArea = roofArea
  site$winArea = windowSizeTotal
  site$UVWindows = UVWin
  site$exposedWallArea = exposedWallArea
  site$wallConsidered = wallConsidered
  site$UVWalls = UVWalls
  site$UVRoof = UVRoof
  site$UAOE = UA_OE
  site$UAQR = UA_QR
  site$totalUA = totalUA 
  site$Cw = realCw
  site$Ca = realCa
  
  return(site)
  
  
  
}