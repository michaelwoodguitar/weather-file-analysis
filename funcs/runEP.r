runEP <- function(fileName,weatherFile, Rdir){
  # files is any extra files. E.g. files{1} = 'electricityProfile.csv';
  #                                files{2} = 'hotWaterUse.csv';
  
  print('Inside runEP')
  ePexecutablePath <- 'C:\\EnergyPlusV8-1-0-weather'
  ePexecutableName <-'RunEPlus.bat'
  
  currentDir <- Rdir
  currentDirWin <- gsub("/","\\\\",currentDir)
  runwin(print(sprintf('cd %s', currentDirWin))) # Change windows directory to R director...
  
  # for (i in 1:nrow(files)){
  #  # I've taken out the error handling here..
  #   string <- print(sprintf('copy %s %s\\ExampleFiles\\%s',files[i],ePexecutablePath,files[i])) # Get the files in the right location...
  #   runwin(string)
  # }
  
  # shell(paste(shQuote(string, type="cmd")))   # how to run the windows command
  # print(sprintf("hello %s", 56 )           # hot to replace text in a stringing,
  runwin('dir')
  string <- print(sprintf('copy %s %s\\ExampleFiles\\%s',fileName,ePexecutablePath,fileName))
 
  runwin(string)
  
  string <- print(sprintf('copy %s %s\\WeatherData\\%s',weatherFile,ePexecutablePath,weatherFile)) # Copy executable files
  runwin(string)
  
  # setwd(sprintf('%s\\R-sims', ePexecutablePath))
  
  setwd(ePexecutablePath)
  
  # run E+
  fileName = gsub('.idf','',fileName) # remove the IDF bit.
  weatherFile = gsub('.epw','',weatherFile) # remove the EPW bit (needed for the executable function)
  string <- print(sprintf('%s %s %s',ePexecutableName,fileName,weatherFile))
  runwin(string)
  
  
  currentDir <- gsub("/","\\\\", currentDir)
  

  
  
  
  setwd(currentDir)
}