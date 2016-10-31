analyse.results = function(file.loc='Output/', target='Output/summary.xlsx', plot.map = T){
  
 files = list.files(file.loc, pattern = '*.epw.RDS') 
  data = list()
  weather.file.names = gsub('.RDS','', files)
  
 names.files = gsub('.epw.RDS','', files)
  
 key.variables = CIBSE.A = CIBSE.B = CIBSE.C = rep(0, length(files))
 
 imp.var = matrix(NA, nrow=length(files), ncol=10)
 
 for (i in 1: length(files)){
   data = readRDS(paste(file.loc, files[i], sep=''))
   CIBSE.A[i] = as.numeric(data$phase2.data$pass.results[[1]])
   CIBSE.B[i] = as.numeric(data$phase2.data$pass.results[[2]])
   CIBSE.C[i] = as.numeric(data$phase2.data$pass.results[[3]])
   important.varaibles = data$phase1.data$important.variables[data$phase1.data$important.variables$Vary.==1,2]
   important.varaibles = gsub('%%','', important.varaibles)
   imp.var[i, 1:length(important.varaibles)] = important.varaibles
 }
 
 # create table
 table = cbind(names.files, 
               round(CIBSE.A*100, digits=0), 
               round(CIBSE.B*100, digits=0), 
               round(CIBSE.C*100, digits=0),
               imp.var)  
 # columns names
 colnames(table[,1:4]) = c('Weather file',
                     'Perc passed He)',
                     'Perc passed MaxDailyWe',
                     'Perc passed MaxDeltaT')
 
 # extract the percentages
 percentages = cbind(as.numeric(table[,2]), 
                     as.numeric(table[,3]), 
                     as.numeric(table[,4]))
 
 # get the lats and longs
 for (i in 1:nrow(percentages)){
   file.name = list.files(path = '../weather-files/', pattern = weather.file.names[i], recursive = TRUE)
   weather.meta <- read.csv(paste("../weather-files/", file.name, sep=''), header=FALSE, stringsAsFactors=FALSE)[1,]
   lat[i] = as.numeric(weather.meta$V7)
   long[i] = as.numeric(weather.meta$V8)
 }
 
 # plot the map
 if (plot.map==T){
   (map <- leaflet() %>% 
      addTiles() %>% 
      setView(-1.5, 53.4, 5) %>% # map location - centered around sheffield
      addCircles(color = "red", 
                 long, 
                 lat, 
                 100*as.vector(percentages[,1])) %>% # CIBSE A
      addCircles(color = "green", 
                 long, 
                 lat,  
                 100*as.vector(percentages[,2])) %>% # CIBSE B
      addCircles(color = "blue", 
                 long, 
                 lat, 
                 100*as.vector(percentages[,3])))  # CIBSE C
 }
 
 # write to file
 write.xlsx(table, target)
 
 return(map)
 
}