# experiment 2 - analysis using deltaT
# ====================================

# this is the variables file...
var.file = 'data/all-variables.xlsx'
# file = 7 # choose the type of weathe file
fold = 1 # folder 1 is DSY, folder 2 is TRY
days.spread = 3


# put a for loop in for buildings...

# Building analysis -------------------------------------------------------
no.buildings = 2
variables=read.xlsx(var.file, sheetIndex = 1)
nvar=sum(variables$Vary.)
# x = runif(n = nvar,min = 0, max = 1)
x = lhsDesign(n = no.buildings, dimension = nvar, randomized = T)$design # is is x$design[i,] for each individual design...
save.folder = 'exp/exp3/'
# note that there are 42 DSY in my wather file folder to be analysed. 

for (building in 1:no.buildings){ # for each building
  
      for (file in 1:2){ # for each weather file (DSY type and location)
        
        building.sample = x[building,]
        real.x = cbind(scale.x(building.sample, variables=variables), variables$Vary.)
        colnames(real.x)=c('Value','Varied?')
        rownames(real.x)=gsub('%%','',variables$Name) # these are the real values of x
        
        sim = simulator # normal simulator
        weather.name=weather.file.select(file = file, fold = fold) # get and set the weather file
        
        # this first step is just a straight run of the simulator
        outsim = run.simulators(x) 
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
        outsim.deconstruct = run.simulators(x) # runs the deconstructed simulator
        
        # display the compared results as a table
        res = display.results(outsim, 
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


# Analyse results ---------------------------------------------------------

results.f.names = dir(save.folder, pattern = '*.RDS')

results.all = list()
no.files = length(results.f.names)
partial = full = matrix(NA, nrow=no.files, ncol=3)
colnames(partial) = colnames(full) = c('CIBSE A','CIBSE B','CIBSE C')
rownames(partial) = rownames(full) = results.f.names


for (i in 1:no.files){
  results.all = readRDS(file = paste(save.folder,results.f.names[i], sep=''))
  # for each result calculate the percentage difference between the true value and the decomposed value (in %)
  full[i,] = c(results.all$Comparison[1,])
  partial[i,] = c(results.all$Comparison[2,])
}

# process the files
Place = gsub('(_DSY)([123])(.RDS)', '', rownames(full))
WeatherFile = rownames(full)
`DSY Type` = gsub("^.*DSY","", WeatherFile)
`DSY Type` = gsub(".RDS","", `DSY Type`)
partial.df = cbind(as.data.frame(partial), Place, WeatherFile, 'Partial', `DSY Type`)
full.df = cbind(as.data.frame(full), Place, WeatherFile, 'Full', `DSY Type`)
colnames(partial.df)[6] = colnames(full.df)[6] = 'Type of file'

all.results = rbind(partial.df, full.df)
all.results$`CIBSE A` = all.results$`CIBSE A`*100 # make CIBSE A a percentage rather than a ratio


ggplot(data = all.results, aes(x = `CIBSE A`, y = Place,  color=`Type of file`, shape=`DSY Type`)) +
  geom_line(data=all.results, aes(x=`CIBSE A`, y = Place, group=WeatherFile)) +
  geom_point() + 
  labs(x = "CIBSE A: % of occupied hours \n where deltaT > 1") + 
  labs(y = "Design summer year location") + 
  theme_linedraw()

ggplot(data = all.results, aes(x = `CIBSE B`, y = Place,  color=`Type of file`, shape=`DSY Type`)) +
  geom_line(data=all.results, aes(x=`CIBSE B`, y = Place, group=WeatherFile)) +
  geom_point() +
  labs(x = "CIBSE B: Maximum weighted exceedance on \na given day") + 
  labs(y = "Design summer year location") + 
  theme_linedraw()

ggplot(data = all.results, aes(x = `CIBSE C`, y = Place,  color=`Type of file`, shape=`DSY Type`)) +
  geom_line(data=all.results, aes(x=`CIBSE C`, y = Place, group=WeatherFile)) +
  geom_point() +
  labs(x = "CIBSE C: Maximum value of deltaT") + 
  labs(y = "Design summer year location") + 
  theme_linedraw()


