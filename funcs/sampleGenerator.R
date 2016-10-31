sampleGenerator <- function(){
  
  p <- rep(0,21)
  
  #we first create the variables that are correlated:
  mean <- c(4.5338e+00,  -9.7815e-01,  -4.2696e-01,   1.3479e+00,  -1.2692e+00)
  cov <- rbind(c(1.7255e-01,   9.5898e-02,  -2.2405e-02,  -2.7720e-02,  -1.3487e-02),
               c(9.5898e-02,   2.0443e-01,   7.6528e-02,  1.1362e-02,   7.5485e-0),
               c(-2.2405e-02,   7.6528e-02,   6.2260e-01, -1.4195e-02,   2.0263e-01),
               c(-2.7720e-02,   1.1362e-02,  -1.4195e-02,  1.9546e-01,   5.4829e-02),
               c(-1.3487e-02,   7.5485e-03,   2.0263e-01,  5.4829e-02,   2.3706e-01))
  
  #   X = mgd(1,5,mean,cov);
  #   
  #     X(1) = exp[X(]));   #floor area
  #     X(2) = exp[X(]));    #Ventilation
  #     X(3) = exp[X(]));    #Wall UValue
  #     X(4) = exp[X(]));    #Glazing U-Value
  #     X(5) = exp[X(]));    #Roof U Value
  
  minimum <- rep(0,5)
  maximum <- rep(0,5)
  #Definition of the limits
  minimum[1] <- 10
  minimum[2] <- 0.05
  minimum[3] <- 0.1
  minimum[4] <- 1
  minimum[5] <- 0.1
  maximum[1] <- 800
  maximum[2] <- 10
  maximum[3] <- 3.822
  maximum[4] <- 10
  maximum[5] <- 3.85
  
  for (j in 1:1e2){
    #fprintf('\nPerforming sample #d\n',j);
    X <- mvrnorm(n = 1, mean, cov, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
    #X <- mgd(1,5,mean,cov)
    flag <- array(0,c(1,5))
    X[1] <- exp(X[1])    #floor area
    X[2] <- exp(X[2])    #Ventilation
    X[3] <- exp(X[3])    #Wall UValue
    X[4] <- exp(X[4])    #Glazing U-Value
    X[5] <- exp(X[5])    #Roof U Value
    
    flag[X<minimum] <- 1
    flag[X>maximum] <- 1
    
    
    if (sum(flag)==0){
      print('something went wrong in the floor area bit...')
    }
  }
  
  #Selecting the building type (1)
  # 1 = Flat
  # 2 = Terrace
  # 3 = Semi
  # 4 = Dettached
  
  
  #  label(1) = {'Building type'};
  x <- c(1, 2, 3, 4)
  y <- c(49, 12, 32, 6)
  
  p[1] <- 40 ## im just leaving this one to see what it does...
  
  #selecting floor area (2)
  p[2] <- X[1]
  
  #select number of floors
  mu <- 2.5
  sigma <- 1
  
  
  # clear('x','y')
  
  p[3] <- rnorm(1, mean=mu, sd=sigma) # selects from the normal distribution
  if (p[3]<1){p[3]<- 1}
  if (p[3]>3){p[3]<- 3}
  
  if (p[1] == 1){
    p[3] <- 1
  }
  
  #ADIABATIC WALL
  #select adiabatic walls
  #This number represent the facade that has 
  #been considered adiabatic following the sequence
  #N S E W, if the buildign is a terrace then the 
  #opposite facade is also made adiabatic
  #if p[1)]= 1 or 4 this number is 0;
  
  if (p[1] == 1 || p[1] ==4){
    p[4] = 0
  } else {
    x = c(1, 2, 3, 4)
    y = c(0.25, 0.25, 0.25, 0.25)
    
    p[4] = 0.25 # there may be some issues with this 0 didn't really understand what the bit in alfonso's code was doing.
  }
  
  #ASPECT RATIO (5)
  mu= 1
  sigma = 1
  
  p[5] <- 0
  # Add in floor and ceiling values...
  p[5] <- rnorm(1, mean=mu, sd=sigma)
  if (p[5]<0.33){p[5]=0.33}    
  if (p[5]>3){p[5]=3} 
  
  #U-Value of Walls (6)
  p[6] = X[3]
  
  
  #U-Value of Roof (7)
  p[7] = X[5]
  
  
  #Infiltration(8)
  p[8] = X[2]
  
  
  #Fenestration area per m^2 of floor area(9)
  mu <- 0.19
  sigma <- 0.049
  p[9] <- rnorm(1, mean=mu, sd=sigma)
  if(p[9]<0.075){p[9]<-0.075}
  
  ##U-Value of windows, in this case there are three options (10)
  # 1- single
  # 2- double
  # 3- triple
  
  if (X[4]<2){
    
    p[10] <- 3
    
  } else {
    
    if (X[4]>4){
      p[10] <- 1
    } else {
      p[10] <- 2
    }
  }
  
  #Thermal mass(11)
  mu <- 50 #mm
  sigma <- 20
  
  # p[11] <- 0
  
  p[11] <- 2 # assume that we have double glazing
  
  #Domestic hot water use(12)
  mu <- 18
  sigma <- 1.15
  
  p[12] <- 0
  
  p[12] <- rnorm(1, mean=mu, sd=sigma)
  if(p[12]<=0){p[12]=0}
  
  #Activity of occupants(13) ## the xs are the values and the ys are the probs
  # x <- c(1, 2, 3)
  # y <- c(60, 30, 10)
  
  a <- runif(1, 0, 1)
  
  if (a > 0.9){
    p[13] <- 120 #Running
  } else if (a > 0.6){
    p[13] <- 80 #Walking around
  } else {
    p[13] <- 70 #TV
  }
  
  #Curtains(14)
  
  p[14] <- runif(1, 0, 100)
  
  
  #Obstacle hight North (15)
  p[15] = runif(1, 0, 1)
  
  #Obstacle hight South (16)
  p[16] = runif(1, 0, 1)
  
  #Obstacle hight East (17)
  p[17] = runif(1, 0, 1)
  
  #Obstacle hight West (18)
  p[18] = runif(1, 0, 1)
  
  #Weather uncertainty(19)
  mu <- 0.0
  sigma <- 0.8
  # [x,y] <- rnorm(1, mean=mu, sd=sigma)
  p[19] <- rnorm(1, mean=mu, sd=sigma)
  
  #Electricity Use(20)
  mu <- 38.1
  sigma <- 17.95
  
  # [x,y] = generateNormal(mu,sigma);
  p[20] <- rnorm(1, mean=mu, sd=sigma)
  
  if (p[20]<10){p[20]<-10}
  
  #Ventilation for air renewal(21)
  mu <- 0.035
  sigma <- 0.001167
  
  # [x,y] = generateNormal(mu,sigma);
  
  
  p[21] = rnorm(1, mean=mu, sd=sigma)
  
  if (p[21]<0){p[21]<-0}
  
  p[22] <- 1 # this is the shading device. 
  
  output<-list(p,flag)
  names(output) <- c('p','flag')
  
  return(output)
  
}












