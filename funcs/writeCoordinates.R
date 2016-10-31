writeCoordinates <- function(fileID,p1,p2,p3,p4){

  write(file=fileID,sprintf('\r\n'), append = T)
  write(file=fileID,sprintf('%f, %f, %f,\r\n',p1[1], p1[2], p1[3]), append = T)
  write(file=fileID,sprintf('%f, %f, %f,\r\n',p2[1], p2[2], p2[3]), append = T)
  write(file=fileID,sprintf('%f, %f, %f,\r\n',p3[1], p3[2], p3[3]), append = T)
  write(file=fileID,sprintf('%f, %f, %f;\r\n',p4[1], p4[2], p4[3]), append = T)
  write(file=fileID,sprintf('\r\n'), append = T)

}