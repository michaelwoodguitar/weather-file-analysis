define_e <- function(){
  
  # Effects to be included
  Heating = 1
  Sun = 1
  Wind = 1
  Electricity = 1
  Metabolic = 1
  DHW = 1
  Ventilation = 1
  Infiltration = 1
  Shading = 1
  sky = 1
  radiation = 1
  convection = 1
  windows = 1
  weather = 1
  shading = 1
  obstacles = 1
  dynamics = 1
  partitions = 1 
  
  e = list(Heating,
            Sun,
            Wind,
            Electricity,
            Metabolic,
            DHW,
            Ventilation,
            Infiltration,
            Shading,
            sky,
            radiation,
            convection,
            windows,
            weather,
            shading,
            obstacles,
            dynamics,
            partitions)
  
  names(e)  =  c('Heating',
                'Sun',
                'Wind',
                'Electricity',
                'Metabolic',
                'DHW',
                'Ventilation',
                'Infiltration',
                'Shading',
                'sky',
                'radiation',
                'convection',
                'windows',
                'weather',
                'shading',
                'obstacles',
                'dynamics',
                'partitions')                
  
  return(e)
}