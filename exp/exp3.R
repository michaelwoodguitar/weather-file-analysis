# experiment 3 - seeing what the deltaT output looks like
# =======================================================

# this is the variables file...
var.file = 'data/all-variables-exp3.xlsx'
# file = 7 # choose the type of weathe file
fold = 1 # folder 1 is DSY, folder 2 is TRY
days.spread = 3


# put a for loop in for buildings...

# Building analysis -------------------------------------------------------
no.buildings = 30
variables=read.xlsx(var.file, sheetIndex = 1)
nvar=sum(variables$Vary.)
# x = runif(n = nvar,min = 0, max = 1)
x = lhsDesign(n = no.buildings, dimension = nvar, randomized = T)$design # is is x$design[i,] for each individual design...
save.folder = 'exp/exp3/'
# note that there are 42 DSY in my wather file folder to be analysed. 
  
for (file in 1:42){ # for each weather file (DSY type and location)
        
    for (building in 1:no.buildings){ # for each building
      
        building.sample = x[building,]
        real.x = cbind(scale.x(building.sample, variables=variables), variables$Vary.)
        colnames(real.x)=c('Value','Varied?')
        rownames(real.x)=gsub('%%','',variables$Name) # these are the real values of x
        
        sim = simulator # normal simulator
        weather.name=weather.file.select(file = file, fold = fold) # get and set the weather file
        
        # this first step is just a straight run of the simulator
        outsim = run.simulators(building.sample) 
        delT = outsim$deltaT  
        
        # find the areas where deltaT is greater than 0
        delT.hourly= outsim$deltaT[seq(1,length(outsim$deltaT),12)] # 
        plot(delT.hourly, type='l', col='blue') # hourly plot
        bin.string = delT.hourly*0
        bin.string[delT.hourly>0]=1
        
        spread = rep(1,24*days.spread)
        # spread = c(seq(0,1, 1/((24*days.spread)/2)), seq(1,0, -1/((24*days.spread)/2)))
        bin.string.spread = filter(bin.string,spread, sides=2) # convolve strings
        bin.string.spread[bin.string.spread>1]=1
        bin.string.spread[is.na(bin.string.spread)]=0 # get rid of NAs
        # plot(bin.string, type='l', col='blue', ylim=c(0,3), xlim=c(3000, 5500))
        # lines(bin.string.spread, col='green')
        
        # make the deconstructed weather file.
        remove.low.delT(bin.string.spread, plot.TF=T) # binary string is the T/F for including the extra IMF values. 
        display.temps('weather-decomposed.epw') # display the output of the weather file
        display.temps('weather.epw')
        # so we run of the deconstructed weather file
        sim=simulator.deconstruct 
        outsim.deconstruct = run.simulators(building.sample) # runs the deconstructed simulator
        
        # display the compared results as a table
        res = display.results(delT.hourly,
                              outsim, 
                              outsim.deconstruct, 
                              build.design=real.x, 
                              weather.name=weather.name, 
                              warm.up = days.spread)
        
        save.name = gsub('.epw','.RDS',res$`Weather name`)
        save.name = paste('Building', building, save.name) # put the building ID in the save name
        save.loc = paste(save.folder, save.name, sep='')
        
        saveRDS(res, file = save.loc)
        
      }
}

saveRDS(x, file = paste(save.folder, 'building.configs/building configurations.RDS', sep=''))

# Analyse results ---------------------------------------------------------
results.f.names = paste(save.folder, dir(save.folder, pattern = '*.RDS'), sep='')
temp.data = make.plot.exp3(results.f.names, DSY.file.name = 'Manchester_DSY3', n.build = 30)
make.plot.exp3(results.f.names, 'DSY2')
make.plot.exp3(results.f.names, 'DSY3')




