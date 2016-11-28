display.results = function(delT.hourly,
                           WeMaxFull,
                           WeMaxDecons,
                           full.weather, 
                           partial.weather, 
                           build.design,
                           weather.name, 
                           warm.up){
  # this is the 
  
  tbl = rbind(unlist(full.weather[1:3]), 
              unlist(partial.weather[1:3]))
  
  rownames(tbl) = c('Full weather','Partial weather')
  
 
  
  o = list(delT.hourly,
           WeMaxFull,
           WeMaxDecons,
           build.design,
           weather.name, 
           tbl, 
           warm.up)
  
  names(o) = c('Delta T',
               'WeMaxFull',
               'WeMaxDecons',
               'Building design',
               'Weather name', 
               'Comparison',
               'Warm up days')
  
  return(o)
}
