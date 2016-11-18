make.plot.exp3 = function(results.f.names, 
                          DSY.file.name,
                          n.build){
  
  # n.build is the total number of buildings
  
  # match DSY
  DSY.match = which(grepl(DSY.file.name,results.f.names))
  # match building

  
  
  # results.f.names[DSY.match]
  results.all = list()
  all.temp.data = included.hours = list()
  
  # for each file, extract the deltaT for DSY1
  for (building in 1:n.build){

    build.string = paste(' ', building, ' ', sep='')
    build.match = which(grepl(build.string, results.f.names))
    insct = intersect(DSY.match, build.match)
    temp.dat = readRDS(file = results.f.names[insct])
    bin.vec = temp.dat$`Delta T`>0
    # do the convolution bit
    spread = rep(1,24*days.spread)
    # spread = c(seq(0,1, 1/((24*days.spread)/2)), seq(1,0, -1/((24*days.spread)/2)))
    bin.string.spread = filter(bin.vec,spread, sides=2) # convolve strings
    bin.string.spread[bin.string.spread>0]=1
    bin.string.spread[is.na(bin.string.spread)]=0 # get rid of NAs
    included.hours[[building]] = which(bin.string.spread==1)
    all.temp.data[[building]] = temp.dat
    
  }
  
  # http://stackoverflow.com/questions/7977383/xy-plot-between-strings-and-numbers (useful link)
  
  # plot the line charts
  plot(included.hours[[1]],rep(1, length(included.hours[[1]])), pch='.', 
       ylim=c(0, length(DSY.match)), xlim=c(0,8760),
       xlab='Hour of the year', ylab='Building ID',
       main='Times of the year where Delta T is greater than 0',
       sub=DSYid)
  
  for (i in 2:n.build){
    points(included.hours[[i]],rep(i, length(included.hours[[i]])), pch='.')
  }
  
  # plot the histogram
  data = unlist(included.hours)
  hist(data, 
       breaks = seq(0,8706,1),
       main = DSY.file.name,
       xlab='Hour of the year',
       ylab='Count of hours where deltaT > 0')

  return(all.temp.data)
}