# experiment 2 - analysis using deltaT
# ====================================

# this is the variables file...
var.file = 'data/all-variables.xlsx'
file = 2 # choose the type of weathe file
fold = 1 # choose the weather file
variables=read.xlsx(var.file, sheetIndex = 1)
nvar=sum(variables$Vary.)
x = rep(0.5,nvar)

sim = simulator # normal simulator
weather.file.select(file = file, fold = fold) # get and set the weather file

# this first step is just a straight run of the simulator
outsim = run.simulators(x) 
delT = outsim$deltaT  

# find the areas where deltaT is greater than 0
delT.hourly= outsim$deltaT[seq(1,length(outsim$deltaT),12)] # 
plot(delT.hourly, type='l', col='blue') # hourly plot
bin.string = delT.hourly*0
bin.string[delT.hourly>0] = 1
days.spread = 3
spread = rep(1,24*days.spread)
bin.string.spread = filter(bin.string,spread, sides=2) # convolve strings
bin.string.spread[bin.string.spread>1]=1
lines(bin.string) 
plot(bin.string, type='l', col='blue')
lines(bin.string.spread, col='green')
# make the deconstructed weather file.
remove.low.delT(bin.string.spread, plot.TF=T) # binary string is the T/F for including the extra IMF values. 

# so we run of the deconstructed weather file
sim=simulator.deconstruct 
outsim.deconstruct = run.simulators(x) # runs the deconstructed simulator

# plot(delta.samp, col='red', type='l')

# decomp.delT = emd(delta.samp, max.sift=6, boundary="wave", plot.imf=T)


