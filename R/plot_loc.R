#' Plot locations from database
#'
#' @param db SQLite data base
#' @param range 
#'
#' @export
#'
plot_loc<-function(db, range=NA)
{
  loc<-dbReadTable(db,"locid")
  library(luftdaten)
  data(germanyborder)
  if (any(is.na(range)))
    {
    min.x<-min(c(min.x,loc$lon))
    max.x<-max(c(max.x,loc$lon))
    max.y<-max(c(max.y,loc$lat))
    min.y<-min(c(min.y,loc$lat))
  }
  else
  {
    min.x<-range[1]
    max.x<-range[2]
    min.y<-range[3]
    max.y<-range[4]
  }
  
  xx<-max.x-min.x
  yy<-1.5*(max.y-min.y)
  par(mai=rep(0,4),pin=c(3,3*yy/xx))
  plot(c(min.x,max.x),c(min.y,max.y),axes=FALSE,col="white",xlab="",ylab="")
  for (i in 1:length(germany16))polygon(germany16[[i]],border=grey(0.9),col="transparent")
  for (i in 1:length(germany))polygon(germany[[i]],border=grey(.5))
  points(loc$lon,loc$lat, pch=19, cex=0.2)

}
