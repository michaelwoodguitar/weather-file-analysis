# experiment 1 - real valued GA.
# ==============================

# assigns the simulator
sim=simulator.deconstruct 

# this is the variables file...
var.file = 'data/all-variables.xlsx'
file = 8
fold = 2
variables=read.xlsx(var.file, sheetIndex = 1)
nvar=sum(variables$Vary.)
x = rep(0.5,nvar)

# ga simulation function design
detune = function(gen.overheat){
  outsim = gen.run.weather(file,fold, build.design=x, gen.overheat=gen.overheat, show.plots=F, decomp.TF = T)
  ratio.noughts = outsim[[6]] #
  return(outsim$CIBSE.A*ratio.noughts^3)  
}

# run the GA optimisation
GA <- ga(type = "real-valued", 
         fitness = detune,
         min = rep(0, 6), max = rep(1, 6), 
         popSize = 20, maxiter = 50,
         optim = TRUE)

sol = as.vector(GA@solution) # the GA solution
dcomped = gen.run.weather(file,fold, x, sol, show.plots=T, decomp.TF = T) # run the model with plots
non.dcomped = gen.run.weather(file,fold, x, sol, show.plots=T, decomp.TF = F) # run the model with plots
