display.temps = function(weather.file){
  
  # displays data for files in the EPW weather file format.
  connection=file(weather.file,open="r")
  header=readLines(connection)[1:8] 
  weather.dat = read.table(weather.file, skip = 8, header = FALSE, sep =',')
  close(connection)
  
  
  plot(weather.dat$V7, type='l', 
       ylab='Temperature (deg)',
       ylim=c(-5,40),
       main=weather.file)
  lines(weather.dat$V8, col='blue')
  lines(weather.dat$V7-weather.dat$V8, lwd=3, col='red')
  return('Done!')
  
}