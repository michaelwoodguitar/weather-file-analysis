weather.file.select = function(fold=NULL, file=NULL){
  
  # the path for the weather files
  path = '../weather-files'
  
  # list all of the weather files
  folders = dir(path = path)
  
  files = list()
  
  for (i in 1:length(folders)){
    files[[i]] = dir(path = paste('../weather-files/', folders[i], sep=''), pattern = '*.epw')
  }
  
  print(folders)
 if (is.null(fold)){ 
  fold = as.numeric(readline(prompt="Select folder by number: "))
 } 
  print(dir(path = paste('../weather-files/', folders[fold], sep=''), pattern = '*.epw'))
 
  
 if (is.null(file)){  
  file = as.numeric(readline(prompt ="Select file by number:"))
 } 
  weather.file.address = paste('/', folders[fold], '/', files[[fold]][file], sep='')
  
  # need to hack changing the drive location
  curr = getwd()
  setwd(path)
  source = getwd() # get the source directory
  setwd(curr) # go back to the current directory

  weather.loc = paste(source,weather.file.address, sep='')  
  cat(paste('copying from', weather.loc, 'to', getwd()), '\n')
  
  success = file.copy(from = weather.loc,
                      to = getwd(), overwrite = T)
  
  if (success==T){
    cat('success!\n')
  } else {
    cat('problem with copying weather file.\n')
  }
  
  # delete weather.epw
  if (file.exists('weather.epw')){
    file.remove('weather.epw')
  }
  # rename copied file as 'weather.epw'
  file.rename(files[[fold]][file], 'weather.epw')
  o = files[[fold]][file]
  
  return(o)
  
}