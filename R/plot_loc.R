#' Plot locations from database
#'
#' @param loc dataframe with lat and lon (latitude and longitude)
#' @param bbox bounding box
#' @param col colour for each point (forwarded to points)
#'
#' @export
#'
plot_loc<-function(loc, bbox=c(range(locid$lat),range(locid$lon))[c(1,3,2,4)], col="black")
{
  data("germanyborder", package="luftdaten")

  min.x<-bbox[2]
  max.x<-bbox[4]
  min.y<-bbox[1]
  max.y<-bbox[3]

  xx<-max.x-min.x
  yy<-1.5*(max.y-min.y)
  par(mai=rep(0,4),pin=c(3,3*yy/xx))
  plot(c(min.x,max.x),c(min.y,max.y),axes=FALSE,col="white",xlab="",ylab="")
  for (i in 1:length(germany16))polygon(germany16[[i]],border=grey(0.9),col="transparent")
  for (i in 1:length(germany))polygon(germany[[i]],border=grey(.5))
  par(...)
  points(loc$lon,loc$lat, pch=19, col=col)

}
