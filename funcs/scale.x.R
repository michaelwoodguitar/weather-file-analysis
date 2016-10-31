scale.x <- function(x, variables){
  
  # this function converts the sizes of the 
  
  scaled = rep(0, nrow(variables))
  
  xcount = 1 
  
  for (i in 1:nrow(variables)){
    
    if (variables$Vary.[i]==1){
      
      scaled[i] = variables$Min.[i]+(x[xcount]*(variables$Max[i]-variables$Min.[i]))
      
      xcount=xcount+1
      
    } else {
      
      scaled[i] = variables$Max[i] # just make it equal to the maximum
      
    }
    
    # print(i)
  }
  
  return(scaled)
  
}