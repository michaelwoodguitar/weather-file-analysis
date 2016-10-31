analyse.location = function(fold=NULL, file=NULL, nug=NULL, dim.mult=10){
  
  # General set up variables ------------------------------------------------
  
  # set the simulator function
  sim = simulator
  # runs the weather file selection function 
  weather.name = weather.file.select(fold, file)
  # bring in the weather meta-data
  weather.meta <- read.csv("weather.epw", header=FALSE, stringsAsFactors=FALSE)[1,]
  lat = as.numeric(weather.meta$V7)
  long = as.numeric(weather.meta$V8)
  # put map in plot area (for reminder)
  # qmap(paste(lat,long), maptype='hybrid', zoom = 12)
  # set the working directory (for E+ use later)
  wd = getwd()
  # set the preliminaries for the building simulation file
  fileName = 'Mikefile.idf'
  base8 = 'baseV8_DSY_test_office.idf'
  # thresholds
  T1 = 0.03 # 3 % seems reasonable for this one
  T2 = 6 # 6 - sould be less than or equal to 6
  T3 = 4 # we've set the output at 4, which is the typical limit
  # create the run simulators function which returns all of the outputs 
  source('Run.Simulators.R')
  
  # Initial run ----------------------------------------------------
  
  # variable file - THIS LINE IS VERY IMPORTANT
  var.file = 'all-variables.xlsx'
  # read 'all-variables.xlsx'
  variables = read.xlsx(var.file, sheetIndex = 1)
  
  # Note the we only sum the variables that we are going to change
  sample.set = t(maximinLHS(sum(variables$Vary.)*dim.mult, sum(variables$Vary.))) 
  
  # Then set up and run the samples
  sens.response1 = sens.response2 = sens.response3 = rep(0, ncol(sample.set)) # set up the responses
  
  # This is the bit that runs the training set
  for (i in 1:(ncol(sample.set))){
    outsim = run.simulators(sample.set[,i])
    sens.response1[i] = outsim$CIBSE.A
    sens.response2[i] = outsim$CIBSE.B
    sens.response3[i] = outsim$CIBSE.C
  }
  
  
  # Emulator 1
  em1 = km(formula = ~1, design = t(sample.set),
           response = sens.response1,
           covtype = "matern5_2",
           nugget = nug)
  
  # Emulator 2 
  em2 = km(formula = ~1, design = t(sample.set),
           response = sens.response2,
           covtype = "matern5_2",
           nugget = nug)
  
  # Emulator 3 
  em3 = km(formula = ~1, design = t(sample.set),
           response = sens.response3,
           covtype = "matern5_2",
           nugget = nug)
  
  em1.func = function(x){
    return(predict_nobias_km(object=em1, newdata = as.matrix(x), type = 'UK')$mean)
  }
  
  em2.func = function(x){
    return(predict_nobias_km(object=em2, newdata = as.matrix(x), type = 'UK')$mean)
  }
  
  em3.func = function(x){
    return(predict_nobias_km(object=em3, newdata = as.matrix(x), type = 'UK')$mean)
  }
  
  # get the names of the variables
  vary.var = as.character(variables[variables$Vary.==1,]$Name)
  
  
  # Peform the sensitivity analysis -----------------------------------------
  
  sens1 = fast99(em1.func, 
                 factors = vary.var, 
                 M = 9,
                 n = 1000, 
                 q = "qunif", 
                 q.arg = list(min = -pi, max = pi))
  
  sens2 = fast99(em2.func, 
                 factors = vary.var, 
                 M = 9, 
                 n = 1000, 
                 q = "qunif", 
                 q.arg = list(min = -pi, max = pi))
  
  sens3 = fast99(em3.func, 
                 factors = vary.var, 
                 M = 9, 
                 n = 1000, 
                 q = "qunif", 
                 q.arg = list(min = -pi, max = pi))
  
  # extract the sensitive variables
  sensitive.vars = top.x(sens1,
                         sens2,
                         sens3,
                         vary.var,
                         x=5)
  
  # these are the variables that we will carry forward for the analysis.
  important.variables = variables
  # set the vary flag to zero
  important.variables$Vary. = 0 
  # change so that this is a vector of strings, not factors.
  variable.names = as.character(important.variables$Name)
  # set the vary flag vector to zero
  to.vary = rep(0,nrow(variables))
  
  # populate the vary flag
  for (i in 1:length(sensitive.vars)){
    # which vector matches the important one.
    important.index = which(variable.names == names(sensitive.vars[i]))
    # set the vary flag
    to.vary[important.index] = 1
  }
  
  important.variables$Vary. = to.vary
  # add in the mean of the min and max for the un-varying variables.
  mns = apply(important.variables[important.variables$Vary.!=1,c(3,4)], 1, mean)
  important.variables[important.variables$Vary.!=1,c(3,4)] = cbind(mns, mns)
  
  # write 'important-variables.xlsx'
  write.xlsx(important.variables, file = 'important.variables.xlsx', sheetName = 'important variables')
  
  
  # save data from the initial phase ----------------------------------------
  
  responses.initial = list(sens.response1,
                           sens.response2,
                           sens.response3)
  sensitivity.analysis.results = list(sens1,
                                      sens2,
                                      sens3)
  initial.emulators = list(em1,
                           em2,
                           em3)
  
  thresholds = list(T1,
                    T2,
                    T3)
  
  phase1.data = list(weather.name,
                     lat,
                     long,
                     sample.set,
                     responses.initial,
                     sensitivity.analysis.results,
                     initial.emulators,
                     variables,
                     important.variables,
                     thresholds)
  
  names(phase1.data) = c('weather.name',
                         'lat',
                         'long',
                         'sample.set',
                         'responses.initial',
                         'sensitivity.analysis.results',
                         'initial.emulators',
                         'variables',
                         'important.variables',
                         'thresholds')
  
  
  # Kriging threshold analysis ----------------------------------------------
  
  # variable file - THIS LINE IS VERY IMPORTANT
  var.file = "important.variables.xlsx"
  
  # read in the variables
  variables = read.xlsx(var.file, sheetIndex = 1)
  
  # say whether we want to run the building model 
  # run.building.model = T
  # thresholds for the outputs
  
  
  # Note the we only sum the variables that we are going to change
  training.set = t(maximinLHS(sum(variables$Vary.)*dim.mult, sum(variables$Vary.))) 
  
  # Then set up and run the samples
  response1 = response2 = response3 = rep(0, ncol(training.set)) # set up the responses
  
  # This is the bit that runs the training set
  # ==========================================
  for (i in 1:(ncol(training.set))){
    outsim = run.simulators(training.set[,i])
    response1[i] = outsim$CIBSE.A
    response2[i] = outsim$CIBSE.B
    response3[i] = outsim$CIBSE.C
  }
  # This section builds the emulators
  # =================================
  
  # Emulator 1
  em1 = km(formula = ~1, design = t(training.set),
           response = response1,
           covtype = "matern5_2",
           nugget = nug)
  
  # Emulator 2 
  em2 = km(formula = ~1, design = t(training.set),
           response = response2,
           covtype = "matern5_2",
           nugget = nug)
  
  # Emulator 3 
  em3 = km(formula = ~1, design = t(training.set),
           response = response3,
           covtype = "matern5_2",
           nugget = nug)
  
  # Efficient Global *Inversion* 
  # ============================
  
  inversionSteps = 7 # ncol(variables)# Where *n* is the number of dimensions
  
  # These functions are wrappers for the simulator functions so that we can pass them to the EGI
  # function
  func1 <- function(x){
    out=run.simulators(x)
    return(out[[1]])
  }
  
  func2 <- function(x){
    out=run.simulators(x)
    return(out[[2]])
  }
  
  func3 <- function(x){
    out=run.simulators(x)
    return(out[[3]])
  }
  
  # variables that can be varied:
  upper.lower = variables[variables$Vary.==1, c(4,5)]
  
  lower = as.vector(upper.lower[,1])
  upper = as.vector(upper.lower[,2])
  optimcontrol = list(method="genoud",pop.size=10)
  
  em1i = EGI(T1, em1, lower = lower, upper = upper, fun=func1, iter=inversionSteps)$lastmodel
  em2i = EGI(T2, em2, lower = lower, upper = upper, fun=func2, iter=inversionSteps)$lastmodel # need to add the lower and upper values in this
  em3i = EGI(T3, em3, lower = lower, upper = upper, fun=func3, iter=inversionSteps)$lastmodel
  
  # create the latin hypercube to test it
  no.of.mc.samples = 10000
  mc.samps = maximinLHS(length(upper), no.of.mc.samples)
  
  em.out3 = em.out1 = em.out2 = rep(0, no.of.mc.samples)
  # Get the emulator values
  for (i in 1:no.of.mc.samples){
    
    em.out1[i] = predict_nobias_km(object=em1i, newdata = as.matrix(t(mc.samps[,i])), type = 'UK')$mean 
    em.out2[i] = predict_nobias_km(object=em2i, newdata = as.matrix(t(mc.samps[,i])), type = 'UK')$mean 
    em.out3[i] = predict_nobias_km(object=em3i, newdata = as.matrix(t(mc.samps[,i])), type = 'UK')$mean 
  }
  
  # Then calculate the number that passed the criterion
  em.out1.bin = (em.out1<T1)
  em.out2.bin = (em.out2<T2)
  em.out3.bin = (em.out3<T3)
  
  em1.passed = sum(em.out1.bin)/length(em.out1.bin)
  em2.passed = sum(em.out2.bin)/length(em.out2.bin)
  em3.passed = sum(em.out3.bin)/length(em.out3.bin)
  
  em.both.1.2 = em.out1.bin & em.out2.bin
  em.both.2.3 = em.out2.bin & em.out3.bin
  em.both.1.3 = em.out1.bin & em.out3.bin
  
  em.all = em.out1.bin & em.out2.bin & em.out3.bin
  all.passed = sum(em.all) / length(em.all)
  
  # save all the data form the emulator inversion section -------------------
  
  responses.inversion = list(response1,
                             response2,
                             response3)
  
  second.emulators = list(em1,
                          em2,
                          em3)
  
  inversion.emulators = list(em1i,
                             em2i,
                             em3i)
  
  pass.results = list(em1.passed,
                      em2.passed,
                      em3.passed,
                      all.passed)
  
  phase2.data = list(training.set,
                     responses.inversion,
                     second.emulators,
                     inversion.emulators,
                     pass.results,
                     inversionSteps)
  
  names(phase2.data) = c('training.set',
                         'responses.inversion',
                         'second.emulators',
                         'inversion.emulators',
                         'pass.results',
                         'inversionSteps')
  
  
  # save all the results ----------------------------------------------------
  
  all.data = list(phase1.data,
                  phase2.data)
  
  names(all.data) = c('phase1.data',
                      'phase2.data')
  
  saveRDS(all.data, file = paste('Output/', weather.name, '.RDS', sep=''), ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)
  
  
  return(1) # i
  
}