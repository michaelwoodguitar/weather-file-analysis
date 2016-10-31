windowCoordinates <- function(areaW,p1,p2,p3,p4) {
  
  #calculating the center of the face
  c = (p1+p3)/2
  
  #calculating the area of the facade
  a = sqrt(sum((p1-p2)*(p1-p2)))
  b = sqrt(sum((p3-p2)*(p3-p2)))
  
  area = a*b
  
  factor = areaW/area
  if (factor > 1) {
    factor = 0.8
  }
  
  v1 = p1-c
  pw1 = c + sqrt(factor)*v1
  
  v2 = p2-c;
  pw2 = c + sqrt(factor)*v2
  
  v3 = p3-c;
  pw3 = c + sqrt(factor)*v3
  
  v4 = p4-c;
  pw4 = c + sqrt(factor)*v4
  
  pw <- list("pw1" = pw1,"pw2" = pw2,"pw3" = pw3,"pw4" = pw4)
  
  return(pw)
  
}







