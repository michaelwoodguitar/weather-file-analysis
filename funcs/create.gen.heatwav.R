
create.gen.heatwav = function(x, plot.bin=F){

  x.scaled = round(x*(2^30-1)) # 30 day chunks
  hex.x = as.hexmode(rep(0,length(x)))
  # turn each element in x into a hex number
  for (i in 1:length(x.scaled)){
    numb = as.numeric(x.scaled[i])
    hex.x[i] = as.hexmode(numb)
  }
  # creat the binary matrix
  bin.mat = apply(as.character(t(hex.x)), MARGIN = 2, hex2bin)
  bin.mat = bin.mat[-1:-2,]
  # create the binary vector
  bin.vec = as.vector(bin.mat) 
  bin.vec.bool = bin.vec==1
  # total days until 30th April
  to.april.30 = rep(0, 120)
  bin.vec.bool 
  to.dec.31 = rep(0, 65)
  year.bin.daily = c(to.april.30, bin.vec.bool, to.dec.31)
  if (plot.bin==T){
    plot(year.bin.daily, type='l')
  }
  year.bin.hourly = rep(year.bin.daily, each=24) # 24 hours per day
  if (plot.bin==T){
    plot(year.bin.hourly, type='l')
  }
  
  return(year.bin.hourly)
}
