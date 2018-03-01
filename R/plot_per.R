#' Plot per time period
#'
#' @param data vector with data
#' @param timestamp vector with timestamps. Character if type is hour, minute or second, otherwise Date-class 
#' @param type character, options are: "month", "week", "weekday", "monthday", "day", "hour", "minute", "second"
#' @param ylab character, for y axis
#' @param main character, title for plot
#'
#' @return plot
#' @export
#'
plot_per<-function(data, timestamp, type="hour",ylab="", main="")
{
  if (type=="hour")
    class<-as.numeric(substr(timestamp,start = 12,stop=13))
  if (type=="minute")
    class<-as.numeric(substr(timestamp,start = 15,stop=16))
  if (type=="second")
    class<-as.numeric(substr(timestamp,start = 18,stop=19))
  if (type=="week")
    class<-as.numeric(format(timestamp,"%V"))
  if (type=="month")
    class<-factor(months(timestamp, abbreviate=TRUE), levels=c("Jan","Feb","Mär","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Dez"))
  if (type=="weekday")
    class<-factor(weekdays(timestamp, abbreviate=TRUE), levels=c("Mo","Di","Mi","Do","Fr","Sa","So"))
  if (type=="monthday")
    class<-as.numeric(format(timestamp,"%d"))
  if (type=="day")
    class<-as.numeric(format(timestamp,"%j"))
 
  class<-as.numeric(by(data,class,mean))
  plot(class[c(1:length(class),1)], type="s", axes=FALSE, xlab="", ylab=ylab, main=main)
  axis(2, ylab=ylab)  
  box()

  if (type=="hour")axis(1, at=0.5+(1:24), labels =0:23) 
  if (type=="minute"|type=="second")axis(1, at=0.5+(1:60), labels =0:59) 
  if (type=="week")axis(1, at=0.5+(1:53), labels =1:53) 
  if (type=="month")axis(1, at=0.5+(1:12), labels =c("Jan","Feb","Mär","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Dez")) 
  if (type=="weekday")axis(1, at=0.5+(1:7), labels =c("Mo","Di","Mi","Do","Fr","Sa","So")) 
  if (type=="monthday")axis(1, at=0.5+(1:31), labels =1:31) 
  if (type=="day")axis(1, at=0.5+(1:366), labels =1:366) 
  
}
