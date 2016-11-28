simulator.deconstruct <- function(p){
  
  # this runs the simulator with the decomposed weather file.

  cat('In simulator')
  fileName="Mikefile.idf"
  # create the file
  mod.gen(p, # inputs
          fileName, # name of the file to create
          'baseV8_DSY_test_office.idf') # name of the base file
  
  
  # Note that this won't work on Mac OR any other version of E+ other than 8.1
  IDF = fileName
  # dat = readChar(fileName, file.info(fileName)$size)
  # write(dat, file = fileName, append = F)
  
  Weather = 'weather-decomposed.epw'

  runEP(IDF,Weather, Rdir=getwd()) 
  
  # this file gets all of the overheatin data
  out = CIBSE.calcs('C:\\EnergyPlusV8-1-0-weather\\ExampleFiles\\Outputs\\Mikefile.csv')
  
  # the variables 'out$...' then contain all of the overheating variables we need...
  # However, we are most interseted in the hours over 25 and 28:
  
  o = list(out$CIBSE.A,
           out$CIBSE.B,
           out$CIBSE.C,
           out$Top,
           out$steps.per.hour,
           out$DelT,
           out$WeMax_day)
  
  names(o) = c('CIBSE.A',
               'CIBSE.B',
               'CIBSE.C',
               'Top',
               'steps.per.hour',
               'deltaT',
               'WeMax_day')
  return(o)   
}
