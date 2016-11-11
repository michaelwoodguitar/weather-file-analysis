make.plot.exp3 = function(results.f.names,
                          DSYid){
  
  loc = which(grepl(DSYid,results.f.names))
  results.f.names[loc]
  results.all = list()
  included.hours = list()
  
  # for each file, extract the deltaT for DSY1
  for (i in loc){
    temp.dat = readRDS(file = results.f.names[i])
    bin.vec = temp.dat$`Delta T`>0
    # do the convolution bit
    spread = rep(1,24*days.spread)
    # spread = c(seq(0,1, 1/((24*days.spread)/2)), seq(1,0, -1/((24*days.spread)/2)))
    bin.string.spread = filter(bin.vec,spread, sides=2) # convolve strings
    bin.string.spread[bin.string.spread>0]=1
    bin.string.spread[is.na(bin.string.spread)]=0 # get rid of NAs
    included.hours[[i]] = which(bin.string.spread==1)
  }
  
  # http://stackoverflow.com/questions/7977383/xy-plot-between-strings-and-numbers (useful link)
  
  build.id = 1
  
  plot(included.hours[[loc[1]]],rep(build.id, length(included.hours[[loc[1]]])), pch='.', 
       ylim=c(0, length(loc)), xlim=c(0,8760),
       xlab='Hour of the year', ylab='Building ID',
       main='Times of the year where Delta T is greater than 0')
  
  for (i in loc[-1]){
    build.id = build.id+1
    points(included.hours[[i]],rep(build.id, length(included.hours[[i]])), pch='.')
  }
  
  return()
}