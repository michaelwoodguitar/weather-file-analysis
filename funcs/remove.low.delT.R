remove.low.delT = function(binary.string, plot.TF=F){ 
  
  cat('\n')
  cat('Removing low delT values\n')
  cat('\n')
  
  fileName="weather.epw"
  connection=file(fileName,open="r")
  header=readLines(connection)[1:8] 
  weather.dat = read.table(fileName, skip = 8, header = FALSE, sep =',')
  close(connection)
  
  
  # decompose the weather file
  decompFile = "weather-decomposed.epw"
  cat('Decomposing the weather file')
  
  # split the temperatures up into residue and imfs
  V7=emd(weather.dat$V7, max.sift=6, boundary="wave", plot.imf=F) # dry bulb
  V8=emd(weather.dat$V8, max.sift=6, boundary="wave", plot.imf=F) # dew point

  # sum the imfs and remove areas of non-interest.
  V7.rm = apply(V7$imf, MARGIN = 1, sum)*binary.string
  V8.rm = apply(V8$imf, MARGIN = 1, sum)*binary.string

  # add the results back in
  weather.dat$V7 = V7$residue + V7.rm
  weather.dat$V8 = V8$residue + V8.rm # should always be lowest
  weather.dat$V8[weather.dat$V8>weather.dat$V7] = weather.dat$V7[weather.dat$V8>weather.dat$V7] # this is to get rid of dew point errors

  if(plot.TF==T){
    plot(weather.dat$V7, type='l', 
         col='red', main='Low DelT removed',
         ylim=c(-5,40))
    lines(weather.dat$V8, col='blue')
    lines(weather.dat$V7, col='red')
    lines(weather.dat$V7-weather.dat$V8, col='black', lty=2)
  }
  
  cat('Done! \n')
  
  # write the decomposed weather file
  fileConn=file(decompFile)
  writeLines(header, fileConn) # write header
  close(fileConn)
  write.table(weather.dat, 
              file='weather-decomposed.epw', 
              append = T, 
              col.names = F,
              row.names = F,
              sep=',') # write the data
  
  # get rid of the quotation marks in the text file
  temp  <- readLines("weather-decomposed.epw")
  temp  <- gsub(pattern = '"', replace = '', x = temp)
  writeLines(temp, con="weather-decomposed.epw")
  
  return('Success!')
}