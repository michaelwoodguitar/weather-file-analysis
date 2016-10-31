run.simulators = function(x){
  # The aim of this simulator function is to onl run a certain number of the variables
  x = scale.x(x, variables=variables)
  # Return the value of the simulator output 
  out = sim(x)
  return(out)
}