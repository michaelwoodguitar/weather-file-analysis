gen.run.weather = function(file,fold, build.design, gen.overheat, show.plots, decomp.TF){ 
  
  cat('\n')
  cat('_____                               __                   __   __             __ \n')
  cat('|     |.-----.----.-----.-----.-----|  |_.----.--.--.----|  |_|__.-----.-----|  |\n')
  cat('|  --  |  -__|  __|  _  |     |__ --|   _|   _|  |  |  __|   _|  |  _  |     |__|\n')
  cat('|_____/|_____|____|_____|__|__|_____|____|__| |_____|____|____|__|_____|__|__|__|\n')
  cat('\n')
  
  weather.name = weather.file.select(file = file, fold = fold) # this moves the weather file
  fileName="build files/weather.epw"
  connection=file(fileName,open="r")
  header=readLines(connection)[1:8] 
  weather.dat = read.table(fileName, skip = 8, header = FALSE, sep =',')
  close(connection)
  
  # create the binary string
  bin.string = create.gen.heatwav(x=gen.overheat, plot.bin = show.plots)
  ratio.noughts = 1-(sum(bin.string)/length(bin.string)) # the more 0's the better
  
  # adjust the weather file using k - don't run if not zero
  decompFile = "weather-decomposed.epw"
  cat('Decomposing the weather file:', weather.name,'to', decompFile, '\n')
  # weather.dat$V7 = weather.dat$V7 * (1+k) # a magnitude temperature change
  decomp=emd(weather.dat$V7, max.sift=6, boundary="wave", plot.imf=F)
  # this is an apply function because there may be more than one 'k'
  
  if(show.plots==T){plot(weather.dat$V7, type='l', col='red')}
  
  the.rest = apply(decomp$imf, MARGIN = 1, sum)
  
  the.rest.bit.crush = the.rest * bin.string
  if (decomp.TF==T){ # only decompose the weather file if we want it!
    weather.dat$V7 = decomp$residue + the.rest.bit.crush
  } else {
    ratio.noughts = 0
  }
  cat('Done! \n')
  
  # create the weather header file
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
  
  # the date
  date = paste(weather.dat$V1, # year
               weather.dat$V2, # month
               weather.dat$V3, # day of the month
               sep='/') 
  # the time
  time = paste((weather.dat$V4-1), # hour
               ':00', sep='')
  # concatenate the date and time
  dtm = paste(date, time) 
  # get the dry bulb temperature
  dry.bulb = as.vector(weather.dat$V7)
  # make data frame
  # weather.df = data.frame(dtm,
                          # dry.bulb,
                          # month.of.year)
  # convert the date and time to the posix format
  # weather.df$dtm = as.POSIXct(weather.df$dtm,format="%Y/%m/%d %H:%M", tz='GMT') 

    # month.of.year = as.factor(month(dtm.posix))
  
  if (show.plots==T){lines(weather.dat$V7, type='l', col='blue')}

  out = run.simulators(x)
  out = c(out, ratio.noughts) # add the noughts ratio
  
  
  return(out)
}