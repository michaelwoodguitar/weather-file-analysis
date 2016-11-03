exp2.chart1 = function(x, name){
  
  x$places <- factor(x$places) # it must be a factor
  # assign the colours to spaces.
  
  place.names = levels(x$places)
  
  # code the colour for the place
  for (i in 1:length(place.names)){
    x$colour[x$places==place.names[i]] = i
  }
  
  dotchart(100*x$`CIBSE A`,labels=row.names(x),cex=.7,groups= x$places, pch=16,
           main=paste(name, "CIBSE A\nGrouped by place"),
           xlab=name, gcolor="black", color=x$colour)
  
  dotchart(x$`CIBSE B`,labels=row.names(x),cex=.7,groups= x$places, pch=16,
           main=paste(name, "CIBSE B\nGrouped by place"),
           xlab=name, gcolor="black", color=x$colour)
  
  dotchart(x$`CIBSE C`,labels=row.names(x),cex=.7,groups= x$places, pch=16,
           main=paste(name, "CIBSE C\nGrouped by place"),
           xlab=name, gcolor="black", color=x$colour)
  
}